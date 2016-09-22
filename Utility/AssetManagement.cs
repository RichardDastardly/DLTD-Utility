using System;
using System.IO;
using System.Reflection;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;
using KSPAssets;

/*
 * Assets:
 * 
 * An asset is a discrete item, stored in:
 * - A bundle
 * - A directory
 * 
 * Stock KSP Assets are identified by a path relative to game base dir/gamedata <- find out
 * Most work of asset location is done by KSPAssets.UrlDir - this doesn't appear to gracefully handle assetbundles
 * 
 * Assets
 * - the loaded unity asset
 * - KSP asset manager compatible location identifier
 * - container - bundle or directory or Gamedatabase interface object ( the latter to enable extended attributes )
 * - type
 * - loader - should NOT directly load, should request data load from the container & then do individual post-load things. should not assume it's being handed a finished asset ( images for instance )
 * -- perhaps this should not be termed "loader", but a request to container who calls asset OnLoaded()
 * - unloader - this probably just destroys the object & marks it as unloaded ( use IUnload )
 * - extended attributes - confignode compatible extended settings
 * - status
 * - status callback
 * - reference count?
 * - implement ISerializationCallbackReceiver - unity copies by serialisation, so either make sure the unity asset is loaded before copy, or after
 * 
 * AssetLoaders
 * - At most basic, takes an existing unity asset & returns it
 * - file loaders need an attribute of which file extension they're handling ( should be able to handle multiples - check attribute class can do this )
 * - file loader might need to construct a unity object
 * 
 * Containers
 * - is a Bundle or directory or KSP Gamedatabase proxy - by the looks of it need one proxy per asset type
 * - location
 * - list of KSP compatible location identifiers ( URLs ) of asset names - make this a property
 * - loader - either bundle or directory or gamedatabase
 * -- gameDB loader - just retrieve object & wrap it in an asset object
 * -- bundle loader needs to watch if the bundle file is already loaded, should probably keep it around for a little while in case of further load requests
 * -- two step loader - one scans bundle + retrieves extended attribs, one instanciates asset objects
 * -- directory loader needs the ability to rescan
 * - (static) Directory of unity asset <-> this asset obect types
 * - status callback/client list - status updates on all stages of bundle load ( important one is retrieving asset list and bundle complete ) and directory scan ( update as directory list / asset load / complete )
 * - Attribute object handled here - a unity text object or a plain text file, contains confignodes of extended settings for assets
 * 
 * 
 * Manager - singleton MonoBehavior
 * - master list of bundles
 * - scans & preloads any Preload bundles
 * - search methods
 * - unload queue - IUnload interface, client needs to implement integer TTL property & Unload()
 * 
 * 
 * Path class -
 * - path seperator constant
 * - base - just reference KSPUtil.ApplicationRootPath 
 * - gameData - base+sep+"GameData"
 * - Mfg - optional step in path to allow Developer/Mod
 * - Mod - optional step ( to allow mfg globals ) - should set one of Mfg/Mod at least
 * -- Plugins - dll store
 * --- PluginData
 * -- Assets - parts/loose assets
 * -- Packages - bundles
 * -- Preload - reference or actual bundles/assets loaded at startup
 */

namespace DLTD.Utility.AssetManagement
{
    #region Constant
    internal static partial class Constant
    {
        public const int defaultTTL = 30;
    }
    #endregion
    #region Static utilities
    internal static class Generics
    {
        public static int secondsToFixedFrames( int secs )
        {
            return secs * 50;
        }
    }
    #endregion

    #region Factory
    public class Factory<I,T> where T : class
    {
        /// <summary>
        /// Indexed by unity object type. Directory AssetSource should match file type to unity type.
        /// </summary>
        private Dictionary<I, Type> constructors;

        public Factory()
        {
            constructors = new Dictionary<I, Type>();
        }

        public bool ContainsKey( I key )
        {
            return constructors.ContainsKey(key);
        }

        public T Generate(I index, params object[] args)
        {
            if (constructors.ContainsKey(index))
            {
                var newAsset = Activator.CreateInstance(constructors[index], args) as T;
                return newAsset;
            }

            return Activator.CreateInstance(typeof(T), args) as T;

            //throw new Exception("[DLTD Factory] Attempted to construct unknown asset type " + AssetType.ToString());
        }

        public void Register(I index, Type instanceType)
        {
            constructors[index] = instanceType;
        }
    }
    #endregion


    #region StateWithEvent
    public delegate void StateChangeEvent<T>(T state);
    /// <summary>
    /// Generic class intended for use with enums to hold state information. Will invoke a delegate for each state change, both generic and per state.
    /// </summary>
    /// <typeparam name="T"></typeparam>
    public class StateWithEvent<T>
    {
        private T state;
        public event StateChangeEvent<T> StateChangeEvent;
        private Dictionary<T, StateChangeEvent<T>> stateEvents;

        public T State
        {
            get { return state; }
            set { state = value; OnStateChange(); }
        }

        public StateWithEvent()
        {
            stateEvents = new Dictionary<T, StateChangeEvent<T>>();
        }

        public StateWithEvent(T initial) : this()
        {
            state = initial;
        }

        public StateWithEvent( T state, StateChangeEvent<T> ev_handler ) : this()
        {
            stateEvents[state] += ev_handler;
        }

        public static implicit operator T(StateWithEvent<T> state)
        {
            return state;
        }

        protected virtual void OnStateChange()
        {
            StateChangeEvent?.Invoke(state);
            if( stateEvents.ContainsKey( state ))
                stateEvents[state]?.Invoke(state);
        }

        public void Install( T state, StateChangeEvent<T> handler )
        {
            stateEvents[state] += handler;
        }

        public void Uninstall( T state, StateChangeEvent<T> handler )
        {
            stateEvents[state] -= handler;
        }
    }
    #endregion

    #region AssetManagement

    #region AssetTypeComposite
    public class AssetTypeComposite 
    {
        public string[] extensions;
        public string unityType;
        public Type type;
        // make methods static, implement IEqualityComparer
        private bool checkExtensions( string ext )
        {
            for (int i = 0; i < extensions.Length; i++)
                if (extensions[i].Equals(ext, StringComparison.OrdinalIgnoreCase))
                    return true;
            return false;
        }

        public bool Equals( Type checkType )
        {
            return (type == checkType);
        }

        public bool Equals( string checkUnityType )
        {
            return (unityType.Equals(checkUnityType, StringComparison.OrdinalIgnoreCase));
        }

        public bool Equals ( string[] checkExt )
        {
            for (int i = 0; i < checkExt.Length; i++)
                if (checkExtensions(checkExt[i]))
                    return true;

            return false;
        }

        public bool Equals( AssetTypeComposite check )
        {
            if (Equals(check.unityType))
                return true;

            if (Equals(check.type))
                return true;

            if (Equals(check.extensions))
                return true;

            return false;
        }

        public bool Equals( string checkUnityType = null, Type checkType = null, string[] checkExt = null )
        {
            if (checkUnityType != null && Equals(checkUnityType))
                return true;

            if (checkType != null && Equals(checkType))
                return true;

            if (checkExt != null && Equals(checkExt))
                return true;

            return false;
        }

    }
    #endregion

    #region Asset
    public enum AssetState { Unloaded, SourceNotReady, Loading, Compiling, Loaded, FailedToLoad }
    public class Asset //: AssetDefinition
    {
        public NameContainer Name;
        public string Path
        {
            get { return Name.path; }
        }
        public Type Type;
        protected AssetSource Container;
        protected ConfigNode attributes;
        public ConfigNode Attributes
        {
            get
            {
                if (attributes == null)
                    attributes = Container.Attributes[Name];
                return attributes;
            }
        }

        protected virtual void parseAttributes()  {
            return;
        }

        protected UnityEngine.Object UEObject;
        public UnityEngine.Object UnityAsset
        {
            get
            {
                if (UEObject == null)
                    UEObject = Container.LoadObject(Name);
                return UEObject;
            }
        }
        public StateWithEvent<AssetState> State;

        public Asset(NameContainer Name)
        {
            this.Name = Name;
            State = new StateWithEvent<AssetState>();
        }

        public Asset(NameContainer Name, AssetSource container ) : this (Name)
        {
            Container = container;
        }

    }
    #endregion

    #region Special files

    public delegate bool TagMatchAction(string tag);

    [AttributeUsage(AttributeTargets.Class)]
    public sealed class SpecialAssetMatch : Attribute
    {
        public string matchString;
        public string wellKnownAs;
    }

    public abstract class SpecialTextAsset
    {
        protected static string NameMatch;
        protected static TagMatchAction _match = (tag) => {
            var tagStripped = Path.GetFileNameWithoutExtension(tag);
            return ((tagStripped.Length > NameMatch.Length) && tagStripped.Substring(tagStripped.Length - NameMatch.Length).Equals(NameMatch, StringComparison.Ordinal));
        };

        public abstract void Parse(string source);
        public static bool isMatch(string tag)
        {
            return _match.Invoke(tag);
        }

        public SpecialTextAsset()
        {
            var attribs = GetType().GetCustomAttributes(typeof(SpecialAssetMatch), true);
            if( attribs.Length > 0 )
                NameMatch = (attribs[0] as SpecialAssetMatch).matchString;
        }
    }

    #region AssetAttributes
    // Just some convenient wrappers around confignode
    // not actually C# attributes
    [SpecialAssetMatch(matchString = "_attributes", wellKnownAs = "Attributes")]
    public class AssetAttributes : SpecialTextAsset
    {
        private const string NodeID = "ASSET_ATTRIBUTES";

        public string UnityName;

        private DictionaryValueList<string, ConfigNode> Attributes;
        public ConfigNode this[string index]
        {
            get
            {
                ConfigNode rval;
                return Attributes.TryGetValue(index, out rval) ? rval : null;
            }
        }

        public override void Parse( string source )
        {
            var parsed = ConfigNode.Parse(source).GetNodes(NodeID);
            var valid = 0;

            for (int i = 0; i < parsed.Length; i++)
            {
                var name = parsed[i].GetValue("name");
                if (name != null)
                {
                    Attributes[name] = parsed[i];
                    valid++;
                }
            }
        }

        public AssetAttributes( string source )
        {
            Attributes = new DictionaryValueList<string, ConfigNode>();
            Parse(source);
        }

        [SpecialAssetMatch(matchString = "_bundle", wellKnownAs = "KSPAssetDefinitions")]
        public class KSPBundleDefinitions : SpecialTextAsset
        {
            private BundleDefinition bundleDefs;
            public List<AssetDefinition> assetDefs
            {
                get
                {
                    return bundleDefs?.assets;
                }
            }

            public AssetDefinition this[int index]
            {
               get { return assetDefs?[index]; }
            }

            public AssetDefinition this[string index]
            {
                get { return bundleDefs?.GetAssetWithPath(index); }
            }

            public override void Parse(string source)
            {
                bundleDefs = BundleDefinition.CreateFromText(source);
            }
        }
    }
    #endregion

    #endregion
    #region AssetSource

    public interface IDispatchKey
    {
        object[] Keys { get; }
    }

    [AttributeUsage(AttributeTargets.Class)]
    public sealed class ValidExtensions : Attribute, IDispatchKey
    {
        public string[] extensions;
        public object[] Keys
        {
            get { return extensions; }
        }

        public string description;

        public ValidExtensions(params string[] Extensions)
        {
            extensions = Extensions;
        }
    }
    
    // May want two enums
    public enum AssetSourceState { Unloaded, Unscanned, Scanning, Scanned, LoadingAsset, Loaded }
    public enum AssetAsyncState { Unloaded, Loading, Loaded, FailedToLoad }

    public abstract class AssetSource
    {
        protected abstract ClassStructure Structure { get; set; }

        protected PathContainer Location;
        public NameContainer Locate(string path)
        {
            return new NameContainer(KSPPaths.BuildPath(Location, path));
        }

        public NameContainer LocateName(string name)
        {
            return new NameContainer(name, Location );
        }

        public DictionaryValueList<PathContainer, Asset> Assets;
        private MonoBehaviour processor;
        public MonoBehaviour Processor
        {
            set { processor = value; }
        }

        protected Factory<Type,Asset> assetFactory;
        internal AssetAttributes Attributes;
        public bool AttributesLoaded
        {
            get { return Attributes != null; }
        }

        public StateWithEvent<AssetSourceState> State;

        public AssetSource( PathContainer path )
        {
            Location = path;
            Assets = new DictionaryValueList<PathContainer, Asset>();
            State = new StateWithEvent<AssetSourceState>();
            State.State = AssetSourceState.Unloaded;

            if (Structure == null)
                Structure = new ClassStructure(this);

            if (AssetManagement.instance != null)
            {
                Processor = AssetManagement.instance;
                assetFactory = AssetManagement.assetFactory;
            }
            GenerateAssetList();
        }

        protected void StartCoroutine( IEnumerator function )
        {
            processor.StartCoroutine(function);
        }

        /// <summary>
        /// Implementation should populate Assets - should not actually load assets
        /// </summary>
        protected abstract void GenerateAssetList();
        protected abstract string[] GetAllAssetNames();

        protected void LoadAttributes(TextAsset assetObj)
        {
            if (!AttributesLoaded)
            {
                Attributes = new AssetAttributes(assetObj.text);
                Attributes.UnityName = assetObj.name;
            }
        }

        /// <summary>
        /// Implementation should ensure the Asset object has loaded the object - Assets should know how to load
        /// their own Unity objects, but querying an asset object for it's Unity object should be synchronous
        /// unless there is an immediate substitute.
        /// </summary>
        /// <param name="name">Asset name/path or PathContainer</param>
        /// <returns></returns>
        public abstract Asset GetAsset(string name);
        public abstract Asset GetAsset(PathContainer name);

        public abstract Asset[] GetAssets();
        public abstract Asset[] GetAssets(string[] names);
        public abstract Asset[] GetAssets(PathContainer[] names);

        /// <summary>
        /// Most asset replies will be of the basic Asset type, but 
        /// the assetFactory can return any type of asset object
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public abstract Asset[] GetAssetsOfType<T>() where T : Asset;
        /// <summary>
        /// pathFilter is intended to be a substring to enable loading assets from a specific directory.
        /// assetbundle should probably just implement this by ignoring the match
        /// </summary>
        /// <typeparam name="T">Type of assets to filter</typeparam>
        /// <param name="pathFilter">substring match for name filtering</param>
        /// <returns></returns>
        public abstract Asset[] GetAssetsOfType<T>( string pathFilter ) where T : Asset;

        public abstract Asset[] GetAssetsOfUnityType<T>() where T : UnityEngine.Object;

        /// <summary>
        /// Used by Asset objects to load Unity objects
        /// </summary>
        /// <param name="name"></param>
        /// <returns></returns>
        internal abstract UnityEngine.Object LoadObject(string name);
        internal abstract UnityEngine.Object LoadObject(PathContainer name);

        internal virtual UnityEngine.Object LoadObjectAsync(string name, StateChangeEvent<AssetAsyncState> callback) { return null; }
        internal virtual UnityEngine.Object LoadObjectAsync(PathContainer name, StateChangeEvent<AssetAsyncState> callback) { return null; }
    }

    #endregion

    #region AssetBundleSource
    [ValidExtensions("abl","assetbundle")]
    public class AssetBundleSource : AssetSource, IUnloadable
    {
        private static ClassStructure structure;
        protected override ClassStructure Structure {
            get { return structure; }
            set { structure = value; }
        }

        #region IUnloadable
        private int _defaultTTL = Constant.defaultTTL;
        public int defaultTTL {  get { return _defaultTTL; } }
        private int _TTL = Constant.defaultTTL;
        public int TTL
        {
            get { return _TTL;  }
            set { _TTL = value; }
        }

        public IUnloader Unloader
        {
            get { return AssetManagement.instance; }
        }

        private bool unloadAll = false;

        public void Unload()
        {
            if (bundle != null)
                bundle.Unload(unloadAll);

            bundleFileState.State = AssetAsyncState.Unloaded;
        }

        public void ResetTTL()
        {
            _TTL = _defaultTTL;
        }
        #endregion

        private AssetBundle bundle;

        private StateWithEvent<AssetAsyncState> bundleFileState = new StateWithEvent<AssetAsyncState>(AssetAsyncState.Unloaded);

        public AssetBundleSource(PathContainer path) : base(path)
        {
            _defaultTTL = Generics.secondsToFixedFrames( 120 );
            _TTL = _defaultTTL;
        }

        // don't forget to set states

        protected override string[] GetAllAssetNames()
        {
            if (bundleFileState.State != AssetAsyncState.Loaded)
                return null;

            return bundle.GetAllAssetNames();
        }

        private void LoadBundleImmediate()
        {
            if (bundleFileState.State != AssetAsyncState.Unloaded)
                return;

            using (WWW unityWWWStream = new WWW(KSPPaths.BuildPath("file:/", (Location as NameContainer).Absolute())))
            {
                bundleFileState.State = AssetAsyncState.Loading;
                if (!string.IsNullOrEmpty(unityWWWStream.error))
                {
                    bundleFileState.State = AssetAsyncState.FailedToLoad;
                    throw new Exception("WWW failed to load asset bundle " + (Location as NameContainer) );
                }

                bundleFileState.State = AssetAsyncState.Loaded;
                bundle = unityWWWStream.assetBundle;
                Unloader.queueForUnload(this);
            }
        }

        private IEnumerator LoadBundle()
        {
            if (bundleFileState.State != AssetAsyncState.Unloaded)
                yield break;

            using (WWW unityWWWStream = new WWW(KSPPaths.BuildPath("file:/", (Location as NameContainer).Absolute())))
            {
                bundleFileState.State = AssetAsyncState.Loading;
                yield return unityWWWStream;

                if (!string.IsNullOrEmpty(unityWWWStream.error))
                {
                    bundleFileState.State = AssetAsyncState.FailedToLoad;
                    throw new Exception("WWW failed to load asset bundle " + (Location as NameContainer));
                }

                bundleFileState.State = AssetAsyncState.Loaded;
                bundle = unityWWWStream.assetBundle;
                Unloader.queueForUnload(this);
            }
        }

        // this and below need combining somehow
        private IEnumerator AssetLoaderAsync(NameContainer name )
        {
            if (bundleFileState == AssetAsyncState.FailedToLoad)
                yield break;

            if (bundleFileState == AssetAsyncState.Unloaded)
                StartCoroutine(LoadBundle());

            while( bundleFileState != AssetAsyncState.Loaded )
            {
                if(bundleFileState == AssetAsyncState.FailedToLoad)
                    yield break;
                yield return bundleFileState;
            }

            var UnityObject = bundle.LoadAssetAsync(name);
            yield return bundle;

            if(UnityObject == null )
                throw new Exception("[DLTD AssetBundleSource] " + (Location as NameContainer) + " AssetLoaderAsync(" + name + ") failed to load asset");

            var asset = assetFactory.Generate(UnityObject.GetType(), LocateName(name), this);
            if ( asset == null )
                throw new Exception("[DLTD AssetBundleSource] " + (Location as NameContainer) + " AssetLoaderAsync(" + name + ") failed to generate Asset for type " + UnityObject.GetType());

            Assets[name] = asset;
        }

        private IEnumerator AllAssetLoaderAsync()
        {
            if (bundleFileState.State == AssetAsyncState.FailedToLoad)
                yield break;

            if (bundleFileState.State == AssetAsyncState.Unloaded)
                StartCoroutine(LoadBundle());

            while (bundleFileState.State != AssetAsyncState.Loaded)
            {
                if (bundleFileState.State == AssetAsyncState.FailedToLoad)
                    yield break;
                yield return AssetAsyncState.Loading;
            }

            var Request = bundle.LoadAllAssetsAsync();
            yield return Request;

            var UnityObjects = Request.allAssets;

            if (UnityObjects == null)
                throw new Exception("[DLTD AssetBundleSource] " + (Location as NameContainer) + " AllAssetLoaderAsync() failed to load assets");

            for( int i = 0; i < UnityObjects.Length; i++ )
            {
                var u_obj = UnityObjects[i];

                // either have to do somthing with special text files here or handle them earlier & just ignore.

                //if (AssetAttributes.isMatch(u_obj.name))
                //{
                //    LoadAttributes(u_obj as TextAsset );
                //    continue;
                //}

                //if( u_obj.name.Substring( u_obj.name.Length - 7).Equals( "_bundle", StringComparison.OrdinalIgnoreCase ))
                //{
                //    bundleDefs = BundleDefinition.CreateFromText(( u_obj as TextAsset).text );
                //    continue;
                //}

                DLTDLog.Log("[DLTD AssetBundleSource] loading " + u_obj.name + " of type "+ u_obj.GetType());
                var asset = assetFactory.Generate(u_obj.GetType(), LocateName(u_obj.name), this);
                if (asset == null)
                    throw new Exception("[DLTD AssetBundleSource] " + (Location as NameContainer) + " AllAssetLoaderAsync() failed to generate Asset for type " + u_obj.GetType());

                Assets[Locate(UnityObjects[i].name)] = asset;
            }

        }

        protected override void GenerateAssetList()
        {
            StartCoroutine(AllAssetLoaderAsync());
        }

        public override Asset GetAsset(string name)
        {
            var nameAsKey = Locate(name);
            return GetAsset(nameAsKey);
        }

        public override Asset GetAsset(PathContainer name)
        {
            Asset locatedAsset;

            // In future, have this fall back to querying the GameDatabase proxy

            Assets.TryGetValue(name, out locatedAsset);
            return locatedAsset;
        }

        public override Asset[] GetAssets()
        {
            var results = new Asset[Assets.Count];
            for (int i = 0; i < Assets.Count; i++)
                results[i] = Assets.At(i);

            return results;
        }

        public override Asset[] GetAssets(string[] names)
        {
            Asset[] locatedAssets = new Asset[names.Length];
            int locatedCount = 0;

            for (int i = 0; i < names.Length; i++)
            {
                Asset thisLocatedAsset;
                if (Assets.TryGetValue(Locate(names[i]), out thisLocatedAsset))
                        locatedAssets[locatedCount++] = thisLocatedAsset;
            }
            return locatedAssets;
        }

        public override Asset[] GetAssets(PathContainer[] names)
        {
            return GetAssets(Array.ConvertAll( names, item =>(string)item ));
        }


        /// <summary>
        /// The return type is not bounded by number of located assets - check for null
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public override Asset[] GetAssetsOfType<T>()
        {
            Asset[] locatedAssets = new T[Assets.Count];
            int locatedCount = 0;

            for(int i = 0; i < Assets.Count; i++ )
            {
                var curAsset = Assets.At(i);
                if (curAsset.GetType() == typeof(T))
                    locatedAssets[locatedCount++] = curAsset;
            }

            return locatedAssets;

        }

        public override Asset[] GetAssetsOfType<T>(string pathFilter)
        {
            Asset[] locatedAssets = new T[Assets.Count];
            int locatedCount = 0;

            for (int i = 0; i < Assets.Count; i++)
            {
                var curAsset = Assets.At(i);
                if ((curAsset.GetType() == typeof(T)) && pathFilter.Equals(curAsset.Path.Substring(0, pathFilter.Length ), StringComparison.OrdinalIgnoreCase))
                    locatedAssets[locatedCount++] = curAsset;
            }

            return locatedAssets;
        }

        /// <summary>
        /// The return type is not bounded by number of located assets - check for null
        /// </summary>
        /// <typeparam name="T"></typeparam>
        /// <returns></returns>
        public override Asset[] GetAssetsOfUnityType<T>()
        {
            Asset[] locatedAssets = new Asset[Assets.Count];
            int locatedCount = 0;

            for (int i = 0; i < Assets.Count; i++)
            {
                var curAsset = Assets.At(i);
                if (curAsset.Type == typeof(T))
                    locatedAssets[locatedCount++] = curAsset;
            }
            return locatedAssets;
        }

        internal override UnityEngine.Object LoadObject(string name)
        {
            LoadBundleImmediate();
            if (bundleFileState.State == AssetAsyncState.FailedToLoad)
                return null;

            var names = GetAllAssetNames();
            for (int i = 0; i < names.Length; i++)
                DLTDLog.Log("LoadObject dump asset names: " + names[i]);

            DLTDLog.Log("LoadObject loading " + name);
            var result =  bundle.LoadAsset(name);
            DLTDLog.Log("LoadObject attempted load " + name + " [" + result?.ToString()+"]");
            return result;
        }

        internal override UnityEngine.Object LoadObject(PathContainer name)
        {
            return LoadObject((string)name);
        }
    }
    #endregion
    #endregion


    #region AssetManagement
    public interface IUnloadable
    {
        int TTL { get; set; }
        int defaultTTL { get; }
        IUnloader Unloader { get; }

        void ResetTTL();
        void Unload();
    }

    public interface IUnloader
    {
        void queueForUnload(IUnloadable item);
    }

    [KSPAddon(KSPAddon.Startup.Instantly, true)]
    public class AssetManagement : MonoBehaviour, IUnloader
    { 
        private ValidatedKSPPaths paths;
        public static AssetManagement instance;

        public static Factory<string, AssetSource> assetSourceFactory;
        public static Factory<Type,Asset> assetFactory;

        public static List<AssetSource> assetSources;

        #region Unload queue
        private List<IUnloadable> unloadQueue;
        private void processUnloadQueue()
        {
            for(int i = 0; i < unloadQueue.Count; i++ )
                if( unloadQueue[i].TTL <= 0 )
                {
                    unloadQueue[i].ResetTTL();
                    unloadQueue[i].Unload();
                    unloadQueue.RemoveAt(i);
                    if (unloadQueue.Count == 0)
                        unloadQueue.TrimExcess();
                }
                else
                {
                    unloadQueue[i].TTL--;
                }
             
        }
        public void queueForUnload( IUnloadable item )
        {
            if (unloadQueue == null)
                unloadQueue = new List<IUnloadable>();

            unloadQueue.Add(item);
        }
        #endregion

        #region Preload
        private void PreloadDLTDGlobalAssets()
        {
            var preloadFiles = Directory.GetFiles(paths["PreLoad"]);
            for ( int i = 0; i < preloadFiles.Length; i++ )
            {
                var filePath = new NameContainer(preloadFiles[i]);
                var fileExt = filePath.Extension;
                DLTDLog.Log("AssetManagement PreLoad looking at file " + filePath + ", trying extension [" + fileExt + "]");
                if(assetSourceFactory.ContainsKey( fileExt.ToLower() ))
                {
                    DLTDLog.Log("AssetManagement PreLoad loading file " + filePath);
                    assetSources.Add(assetSourceFactory.Generate(fileExt, filePath ));
                }
            }
        }
        #endregion

        #region Unity
        public void Awake()
        {
            instance = this;
            DontDestroyOnLoad(this);

            unloadQueue = new List<IUnloadable>();
            assetFactory = new Factory<Type,Asset>();
            assetSourceFactory = new Factory<string, AssetSource>();
            paths = new ValidatedKSPPaths();
            assetSources = new List<AssetSource>();

            RegisterDispatchableTypes();
            PreloadDLTDGlobalAssets();

        }

        public void FixedUpdate()
        {
            processUnloadQueue();
        }

        #region IAssetSourceDispatcher
        private void RegisterDispatchableTypes()
        {
            var types = Assembly.GetAssembly(GetType()).GetTypes();
            for( int i = 0; i < types.Length; i++ )
                if( types[i] != null && typeof(AssetSource).IsAssignableFrom(types[i]))
                {
                    var attr = types[i].GetCustomAttributes(true);
                    if( attr.Length > 0 )
                    {
                        for( int j = 0; j < attr.Length; j++ )
                        {
                            var interfaces = attr[j].GetType().GetInterfaces();
                            for( int k = 0; k < interfaces.Length; k++ )
                            {
                                if(interfaces[k] == typeof(IDispatchKey))
                                {
                                    RegisterHandler(types[i], (attr[j] as IDispatchKey).Keys as string[]);
                                }
                            }
                        }
                    }
                }
        }

        public void RegisterHandler(Type handler, string[] extensions)
        {
            for (int i = 0; i < extensions.Length; i++)
            {
                if (extensions[i] == null)
                    break;
                DLTDLog.Log("Registering fileExt [" + extensions[i].ToLower() + "] for type " + handler.ToString());
                assetSourceFactory.Register(extensions[i].ToLower(), handler);
            }
        }
        #endregion

        #endregion

    }
    #endregion





    #region random dump
    /**************************************************************************************************************************************************************************/
    // TEMPORARY
    // prototype/pseudocode & code dump

    enum FileState { Unread, Reading, Done };
    internal class YieldingBlockReader
    {

        private byte[] buffer;
        string filePath;

        private StateWithEvent<FileState> ReadMonitor = new StateWithEvent<FileState>(FileState.Unread);

        CoroutineHost processor;

        public YieldingBlockReader(string file)
        {
            filePath = file;
        }

        public YieldingBlockReader(string file, StateChangeEvent<FileState> client) : this(file)
        {
            ReadMonitor.StateChangeEvent += client;
        }

        public IEnumerator Read()
        {
            using (FileStream stream = new FileStream(filePath, FileMode.Open))
            {
                buffer = new byte[stream.Length];
                var _leftToRead = stream.Length;
                var _readLength = Math.Min(_leftToRead, int.MaxValue);
                ReadMonitor.State = FileState.Reading;

                while (_leftToRead > 0)
                {
                    stream.Read(buffer, (int)(stream.Length - _leftToRead), (int)_readLength);
                    _leftToRead -= _readLength;
                    _readLength = Math.Min(_leftToRead, int.MaxValue);
                    yield return stream;
                }
            }
            ReadMonitor.State = FileState.Done;
        }

    }
    #endregion

}
