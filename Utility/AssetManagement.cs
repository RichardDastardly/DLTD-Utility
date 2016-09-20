using System;
using System.IO;
using System.Reflection;
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
 * Loaders
 * - At most basic, takes an existing unity asset & returns it
 * - file loaders need an attribute of which file extension they're handling ( should be able to handle multiples - check attribute class can do this )
 * - file loader might need to construct a unity object
 * 
 * Containers
 * - is a Bundle or directory or KSP Gamedatabase proxy - by the looks of it need one proxy per asset type
 * - location
 * - list of KSP compatible location identifiers ( URLs ) of asset names - make this a property
 * - loader - either bundle or directory or gamedatabase
 * -- gameDB loader - just retrieve object
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
    }
    #endregion

    #region Paths
    #region PathComponent
    public class PathComponent
    {
        public string pathComponent;

        public PathComponent(string path) { pathComponent = path.Replace("//", "/"); }

        public static implicit operator string(PathComponent c)
        {
            return c.pathComponent;
        }

        public string Absolute()
        {
            return Absolute(this);
        }

        public static string Absolute( PathComponent path )
        {
            return KSPPaths.FullPath(path.pathComponent);
        }

        public static string Absolute( string path )
        {
            return KSPPaths.FullPath( path );
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


        private Dictionary<string, PathComponent> commonPaths;
        public PathComponent this[string index]
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
        private sealed class PathComponentAttributes : Attribute
        {
            public string pathKey { get; set; }
            public string parentPathKey { get; set; }
            public string[] pathComponents;

            public PathComponentAttributes(params string[] components)
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

        [PathComponentAttributes(pathKey = "Manufacturer")]
        private string Mfg = Constant.mfg;
        public string Manufacturer
        {
            set { Mfg = value;  buildCommonPaths(); }
        }

        [PathComponentAttributes(parentPathKey = "Manufacturer", pathKey = "Mod")]
        private string Mod;
        public string ModDir
        {
            set { Mod = value; buildCommonPaths(); }
        }

        [PathComponentAttributes(parentPathKey = "Mod")]
        private string Plugins = "Plugins";

        [PathComponentAttributes(parentPathKey = "Plugins", pathKey = "PluginSub")]
        private string pluginDataSubPath;

        [PathComponentAttributes(parentPathKey = "PluginSub")]
        private string PluginData = "PluginData";

        [PathComponentAttributes(parentPathKey = "Mod")]
        private string Assets = "Assets";

        [PathComponentAttributes(parentPathKey = "Assets")]
        private string Packages = "Packages";

        [PathComponentAttributes(parentPathKey = "Mod")]
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
            var commonFields = structure.GetMembersWithAttribute<PathComponentAttributes>();

            var isSetup = new Dictionary<string, bool>(commonFields.Count);
            var attribs = new Dictionary<string, PathComponentAttributes>(commonFields.Count);
            var fieldKeys = new List<string>(commonFields.Keys);
            var fieldsLeft = fieldKeys.Count;

            // preliminary pass to set up flags
            for( int i = 0; i < fieldKeys.Count; i++ )
            {
                var _key = fieldKeys[i];
                isSetup[_key] = false;
                attribs[_key] = commonFields[_key].GetAttributeOfType<PathComponentAttributes>();
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
                    PathComponent keyComponent = null;
                    
                    if (pathKey == null)
                        pathKey = commonFields[_key].MemberName;

                    if (parentPathKey == null) // gamedata relative
                    {
                        keyComponent = new PathComponent( GDRelative(
                            BuildPath( BuildPath( attribs[_key].pathComponents), commonFields[_key].GetValue(this) as string)));
                    }
                    else if (commonPaths.ContainsKey(parentPathKey))
                    {
                        keyComponent = new PathComponent(BuildPath(commonPaths[parentPathKey], commonFields[_key].GetValue(this) as string));
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

            commonPaths = new Dictionary<string, PathComponent>();
            setupStatic();

            buildCommonPaths();
        }
    }

    #endregion
    #endregion


    class AssetManagement
    {
    }
}
