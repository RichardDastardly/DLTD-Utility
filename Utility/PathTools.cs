using System;
using System.Collections.Generic;
using System.Text.RegularExpressions;
using System.IO;
using System.Reflection;
using KSPAssets;

namespace DLTD.Utility
{
    internal static partial class Constant
    {
        public const string mfg = "DLTD";
    }

    #region Paths
    #region PathContainer
    /// <summary>
    /// Use PathContainer to store directory paths and file paths where you won't want to operate with the plain filename
    /// </summary>
    public class PathContainer : IEquatable<string>
    {
        protected string pathcomponent;
        public string pathComponent
        {
            get { return pathcomponent; }
        }

        public virtual string path
        {
            get { return pathcomponent; }
        }

        public PathContainer(string path)
        {
            pathcomponent = Regex.Replace(path, @"[\\/]+", KSPPaths.pathSeparator.ToString());
        }

        public static implicit operator string(PathContainer c)
        {
            return c.path;
        }

        public string Absolute()
        {
            return Absolute(this);
        }

        public static string Absolute(PathContainer _path)
        {
            return KSPPaths.FullPath(_path);
        }

        public static string Absolute(string _path)
        {
            return KSPPaths.FullPath(_path);
        }

        public bool Equals(string other)
        {
            return path.Equals(other, StringComparison.OrdinalIgnoreCase);
        }

        public override string ToString()
        {
            return path;
        }
    }

    /// <summary>
    /// Use NameContainer when you want to operate with both bare filenames and file paths
    /// </summary>
    public class NameContainer : PathContainer
    {
        protected string name;
        public override string path
        {
            get { return KSPPaths.BuildPath(pathComponent, name); }
        }

        public NameContainer(string _path) : base(Path.GetDirectoryName(_path))
        {
            name = Path.GetFileName(_path);
        }

        public NameContainer(string _name, string _path) : base(_path)
        {
            name = _name;
        }

        public NameContainer(AssetDefinition ass) : this(ass.name, ass.path) { }

        public string fileName
        {
            get { return Path.GetFileNameWithoutExtension(name); }
        }

        public string Extension
        {
            get { return Path.GetExtension(name).Substring(1); }
        }

        public static implicit operator string(NameContainer c)
        {
            return c.name;
        }

        public static NameContainer FromAssetDefinition(AssetDefinition ass)
        {
            return new NameContainer(ass);
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


        protected Dictionary<string, PathContainer> commonPaths;
        public PathContainer this[string index]
        {
            get
            {
                if (commonPaths.ContainsKey(index))
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
            set { Mfg = value; buildCommonPaths(); }
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
                structure = new ClassStructure(typeof(KSPPaths), BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Instance);
            }
        }

        protected virtual void buildCommonPaths()
        {
            commonPaths.Clear();

            var commonFields = structure.GetMembersWithAttribute<PathElement>();

            var isSetup = new Dictionary<string, bool>(commonFields.Count);
            var attribs = new Dictionary<string, PathElement>(commonFields.Count);
            var fieldKeys = new List<string>(commonFields.Keys);
            var fieldsLeft = fieldKeys.Count;

            // preliminary pass to set up flags
            for (int i = 0; i < fieldKeys.Count; i++)
            {
                var _key = fieldKeys[i];
                isSetup[_key] = false;
                attribs[_key] = commonFields[_key].GetAttributeOfType<PathElement>();
            }

            while (fieldsLeft > 0)
            {
                for (int i = 0; i < fieldKeys.Count; i++)
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
                        keyComponent = new PathContainer(GDRelative(
                            BuildPath(BuildPath(attribs[_key].pathComponents), commonFields[_key].GetValue(this) as string)));
                    }
                    else if (commonPaths.ContainsKey(parentPathKey))
                    {
                        keyComponent = new PathContainer(BuildPath(commonPaths[parentPathKey], commonFields[_key].GetValue(this) as string));
                    }

                    if (keyComponent != null)
                    {
                        commonPaths[pathKey] = keyComponent;
                        isSetup[_key] = true;
                        fieldsLeft--;
                        continue;
                    }
                }
            }

        }

        public KSPPaths(string modName = null, string mfg = null, string pdl = null)
        {
            Mod = modName;
            if (mfg != null)
                Mfg = mfg;

            pluginDataSubPath = pdl;

            commonPaths = new Dictionary<string, PathContainer>();
            setupStatic();

            buildCommonPaths();
        }

        public PathContainer GetContainerForPath(string pathKey, params string[] path)
        {
            return new PathContainer(BuildPath(commonPaths[pathKey], BuildPath(path)));
        }
    }

    /// <summary>
    /// ValidatedKSPPaths checks each built path exists & removes nonexistant ones
    /// </summary>
    public class ValidatedKSPPaths : KSPPaths
    {
        private void ValidatePathKeys()
        {
            var keys = pathKeys; // pathKeys is a property which builds a new list, so cache the value...
            for (int i = 0; i < keys.Count; i++)
            {
                var prospectivePath = commonPaths[keys[i]];
                if (!Directory.Exists(prospectivePath.Absolute()))
                {
                    commonPaths[keys[i]] = null;
                }
            }
        }

        public ValidatedKSPPaths(string modName = null, string mfg = null, string pdl = null) : base(modName, mfg, pdl) { }

        protected override void buildCommonPaths()
        {
            base.buildCommonPaths();
            ValidatePathKeys();
        }
    }
    #endregion

    #endregion

}
