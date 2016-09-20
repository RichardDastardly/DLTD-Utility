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
    internal static class Constant
    {
        public const string mfg = "DLTD";
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

    #region Paths
    #region PathContainer
    public class PathContainer
    {
        public string pathComponent;

        public PathContainer(string path) { pathComponent = path.Replace("//", "/"); }
        public string name
        {
            get { return Path.GetFileName(pathComponent); }
        }

        public static implicit operator string(PathContainer c)
        {
            return c.pathComponent;
        }

        public string Absolute()
        {
            return Absolute(this);
        }

        public static string Absolute( PathContainer path )
        {
            return KSPPaths.FullPath(path.pathComponent);
        }

        public static string Absolute( string path )
        {
            return KSPPaths.FullPath( path );
        }
    }

    public class NameContainer : PathContainer
    {
        public NameContainer(string path) : base(path) {}
        public string Path
        {
            get { return pathComponent; }
        }

        public static implicit operator string(NameContainer c)
        {
            return c.name;
        }
    }
    #endregion

    #region KSPPaths
    public class KSPPaths
    {

        public static char pathSeparator = '/';
        public static string BuildPath(params string[] pathMembers)
        {
            return string.Join(pathSeparator.ToString(), pathMembers);
        }

        private static string Base;
        public static string FullPath(string pathRelative)
        {
            return BuildPath(Base, pathRelative);
        }


        private Dictionary<string, PathContainer> commonPaths;
        public PathContainer this[string index]
        {
            get {
                if( commonPaths.ContainsKey(index))
                    return commonPaths[index];
                return null;
            }
        }

        public List<string> pathKeys
        {
            get { return new List<string>(commonPaths.Keys); }
        }

        private static ClassStructure structure;

        #region PathComponentAttributes
        [AttributeUsage(AttributeTargets.Field | AttributeTargets.Property)]
        private sealed class PathElement : Attribute
        {
            public string pathKey { get; set; }
            public string parentPathKey { get; set; }
            public string[] pathComponents;

            public PathElement(params string[] components)
            {
                pathComponents = components;
            }
        }
        #endregion

        public string LocalDir
        {
            get { return Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location).Replace('\\', pathSeparator); }
        }

        public static readonly string GameData = "GameData";
        public static string GDRelative(string gdRelativePath) { return BuildPath(GameData, gdRelativePath); }

        [PathElement(pathKey = "Manufacturer")]
        private string Mfg = Constant.mfg;
        public string Manufacturer
        {
            set { Mfg = value;  buildCommonPaths(); }
        }

        [PathElement(parentPathKey = "Manufacturer", pathKey = "Mod")]
        private string Mod;
        public string ModDir
        {
            set { Mod = value; buildCommonPaths(); }
        }

        [PathElement(parentPathKey = "Mod")]
        private string Plugins = "Plugins";

        [PathElement(parentPathKey = "Plugins", pathKey = "PluginSub")]
        private string pluginDataSubPath;

        [PathElement(parentPathKey = "PluginSub")]
        private string PluginData = "PluginData";

        [PathElement(parentPathKey = "Mod")]
        private string Assets = "Assets";

        [PathElement(parentPathKey = "Assets")]
        private string Packages = "Packages";

        [PathElement(parentPathKey = "Mod")]
        private string PreLoad = "PreLoad";

        public static void setBasePath()
        {
            Base = Path.GetDirectoryName(KSPUtil.ApplicationRootPath);
        }

        private static void setupStatic()
        {
            if (structure == null)
            {
                setBasePath(); 
                structure = new ClassStructure(typeof(KSPPaths), BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance );
            }
        }

        private void buildCommonPaths()
        {
            commonPaths.Clear();

            var commonFields = structure.GetMembersWithAttribute<PathElement>();

            var isSetup = new Dictionary<string, bool>(commonFields.Count);
            var attribs = new Dictionary<string, PathElement>(commonFields.Count);
            var fieldKeys = new List<string>(commonFields.Keys);
            var fieldsLeft = fieldKeys.Count;

            // preliminary pass to set up flags
            for( int i = 0; i < fieldKeys.Count; i++ )
            {
                var _key = fieldKeys[i];
                isSetup[_key] = false;
                attribs[_key] = commonFields[_key].GetAttributeOfType<PathElement>();
            }

            while( fieldsLeft > 0 )
            {
                for( int i = 0; i < fieldKeys.Count; i++ )
                {
                    var _key = fieldKeys[i];
                    if (isSetup[_key])
                        continue;

                    var parentPathKey = attribs[_key].parentPathKey;
                    var pathKey = attribs[_key].pathKey;
                    PathContainer keyComponent = null;
                    
                    if (pathKey == null)
                        pathKey = commonFields[_key].MemberName;

                    if (parentPathKey == null) // gamedata relative
                    {
                        keyComponent = new PathContainer( GDRelative(
                            BuildPath( BuildPath( attribs[_key].pathComponents), commonFields[_key].GetValue(this) as string)));
                    }
                    else if (commonPaths.ContainsKey(parentPathKey))
                    {
                        keyComponent = new PathContainer(BuildPath(commonPaths[parentPathKey], commonFields[_key].GetValue(this) as string));
                    }

                    if( keyComponent != null )
                    {
                          commonPaths[pathKey] = keyComponent;
                        isSetup[_key] = true;
                        fieldsLeft--;
                        continue;
                    }
                }
            }

        }

        public KSPPaths(string modName = null, string mfg = null, string pdl = null )
        {
            Mod = modName;
            if (mfg != null)
                Mfg = mfg;

            pluginDataSubPath = pdl;

            commonPaths = new Dictionary<string, PathContainer>();
            setupStatic();

            buildCommonPaths();
        }

        public PathContainer GetContainerForPath( string pathKey, params string[] path )
        {
            return new PathContainer(BuildPath(commonPaths[pathKey], BuildPath( path )));
        }
    }
    #endregion

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
            StateChangeEvent(state);
            stateEvents[state](state);
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
    #region Asset
    public enum AssetState { Unloaded, SourceNotReady, Loading, Compiling, Loaded, FailedToLoad }
    public class Asset
    {
        protected NameContainer Name;
        public string Path
        {
            get { return Name.Path; }
        }
        public Type Type;
        protected AssetSource Container;
        public ConfigNode attributes;
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
                    UEObject = Container.LoadObject(Path);
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

    #region AssetFactory
    public class AssetFactory
    {
        /// <summary>
        /// Indexed by unity object type. Directory AssetSource should match file type to unity type.
        /// </summary>
        private Dictionary<Type, Type> constructors;

        public AssetFactory()
        {
            constructors = new Dictionary<Type, Type>();
        }

        public Asset Generate( Type AssetType, params object[] args )
        {
            if (constructors.ContainsKey(AssetType))
            {
                var newAsset = Activator.CreateInstance(constructors[AssetType], args) as Asset;
                newAsset.Type = AssetType;
            }

            throw new Exception("[DLTD AssetFactory] Attempted to construct unknown asset type " + AssetType.ToString());
        }

        public void Register( Type type, Type assetType )
        {
            constructors[type] = assetType;
        }
    }
    #endregion

    #region AssetAttributes
    // Just some convenient wrappers around confignode
    // not actually C# attributes
    public class AssetAttributes
    {
        private const string NodeID = "ASSET_ATTRIBUTES";
        private const string NameMatch = "_attributes";
        public string UnityName;

        public static bool isAttributeName ( string name )
        {
            return ((name.Length > NameMatch.Length) && name.Substring(name.Length - NameMatch.Length).Equals( NameMatch, StringComparison.Ordinal ));
        }

        private DictionaryValueList<string, ConfigNode> Attributes;
        public ConfigNode this[string index]
        {
            get
            {
                ConfigNode rval;
                return Attributes.TryGetValue(index, out rval) ? rval : null;
            }
        }

        private int ParseAttribs( string source )
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
            return valid;
        }

        public AssetAttributes( string source )
        {
            Attributes = new DictionaryValueList<string, ConfigNode>();
            ParseAttribs(source);
        }
    }
    #endregion

    #region AssetSource

    // May want two enums
    public enum AssetSourceState { Unloaded, Unscanned, Scanning, Scanned, LoadingAsset, Loaded }
    public enum AssetAsyncState { Unloaded, Loading, Loaded, FailedToLoad }
    public abstract class AssetSource
    {
        protected PathContainer Location;
        public NameContainer Locate(string path)
        {
            return new NameContainer(KSPPaths.BuildPath(Location, path));
        }

        public DictionaryValueList<PathContainer, Asset> Assets;
        private MonoBehaviour processor;
        public MonoBehaviour Processor
        {
            set { processor = value; }
        }

        protected AssetFactory factory;
        internal AssetAttributes Attributes;
        public bool AttributesLoaded
        {
            get { return Attributes != null; }
        }

        public StateWithEvent<AssetSourceState> State;

        public AssetSource( PathContainer path )
        {
            Location = path;
            State = new StateWithEvent<AssetSourceState>();

            if( AssetManagement.instance != null )
            {
                Processor = AssetManagement.instance;
                factory = AssetManagement.Factory;
            }
        }

        protected void StartCoroutine( IEnumerator function )
        {
            processor.StartCoroutine(function);
        }

        /// <summary>
        /// Implementation should populate Assets - should not actually load assets
        /// </summary>
        protected abstract void GenerateAssetList();

        /// <summary>
        /// Implementation should ensure the Asset object has loaded the object - Assets should know how to load
        /// their own Unity objects, but querying an asset object for it's Unity object should be synchronous
        /// unless there is an immediate substitute.
        /// </summary>
        /// <param name="name">Asset name/path or PathContainer</param>
        /// <returns></returns>
        public abstract Asset GetAsset(string name);
        public abstract Asset GetAsset(PathContainer name);

        public abstract Asset[] GetAssets(string[] names);
        public abstract Asset[] GetAssets(PathContainer[] names);

        /// <summary>
        /// Most asset replies will be of the basic Asset type, but 
        /// the factory can return any type of asset object
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
    public class AssetBundleSource : AssetSource, IUnloadable
    {
        
        #region IUnloadable
        private int _defaultTTL = Constant.defaultTTL;
        public int defaultTTL {  get { return _defaultTTL; } }
        private int _TTL = Constant.defaultTTL;
        public int TTL
        {
            get { return TTL;  }
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
        }

        public void ResetTTL()
        {
            _TTL = _defaultTTL;
        }
        #endregion

        private AssetBundle bundle;

        public AssetBundleSource(PathContainer path) : base(path)
        {
            _defaultTTL = Generics.secondsToFixedFrames( 120 );
            _TTL = _defaultTTL;
        }

        // don't forget to set states

        private IEnumerator LoadBundle()
        {
            using (WWW unityWWWStream = new WWW(Location))
            {
                yield return unityWWWStream;
                if (!string.IsNullOrEmpty(unityWWWStream.error))
                {
                    throw new Exception("WWW failed to load asset bundle " + Location.name );
                }

                bundle = unityWWWStream.assetBundle;
                Unloader.queueForUnload(this);
            }
        }

        private IEnumerator AssetLoaderAsync(NameContainer name )
        {
            if (bundle == null)
                throw new Exception("[DLTD AssetBundleSource] " + Location.name + " AssetLoaderAsync("+name+") attempted to load from unloaded bundle");

            var UnityObject = bundle.LoadAssetAsync(name);
            yield return bundle;

            if(UnityObject == null )
                throw new Exception("[DLTD AssetBundleSource] " + Location.name + " AssetLoaderAsync(" + name + ") failed to load asset");

            var asset = factory.Generate(UnityObject.GetType(), Locate(name), this);
            if ( asset == null )
                throw new Exception("[DLTD AssetBundleSource] " + Location.name + " AssetLoaderAsync(" + name + ") failed to generate Asset for type " + UnityObject.GetType());

            Assets[name] = asset;
        }

        private IEnumerator AllAssetLoaderAsync()
        {
            if (bundle == null)
                throw new Exception("[DLTD AssetBundleSource] " + Location.name + " AllAssetLoaderAsync() attempted to load from unloaded bundle");

            var Request = bundle.LoadAllAssetsAsync();
            yield return Request;

            var UnityObjects = Request.allAssets;

            if (UnityObjects == null)
                throw new Exception("[DLTD AssetBundleSource] " + Location.name + " AllAssetLoaderAsync() failed to load assets");

            for( int i = 0; i < UnityObjects.Length; i++ )
            {
                var u_obj = UnityObjects[i];

                if (AssetAttributes.isAttributeName(u_obj.name))
                {
                    if (!AttributesLoaded)
                    {
                        Attributes = new AssetAttributes((u_obj as TextAsset).text);
                        Attributes.UnityName = u_obj.name;
                    }
                    continue;
                }

                var asset = factory.Generate(u_obj.GetType(), Locate(u_obj.name), this);
                if (asset == null)
                    throw new Exception("[DLTD AssetBundleSource] " + Location.name + " AllAssetLoaderAsync() failed to generate Asset for type " + u_obj.GetType());

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
            throw new NotImplementedException();
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
            throw new NotImplementedException();
        }

        internal override UnityEngine.Object LoadObject(PathContainer name)
        {
            throw new NotImplementedException();
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
        public static AssetManagement instance;

        public static AssetFactory Factory;

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

        #region Unity
        public void Awake()
        {
            instance = this;
            DontDestroyOnLoad(this);

            unloadQueue = new List<IUnloadable>();
            Factory = new AssetFactory();
        }

        public void FixedUpdate()
        {
            processUnloadQueue();
        }

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
