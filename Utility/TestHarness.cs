using UnityEngine;
using System.Collections.Generic;

namespace DLTD.Utility.TestHarness
{
    [KSPAddon(KSPAddon.Startup.Instantly, true)]
    class TestHarness : MonoBehaviour
    {
        public AssetManagement.KSPPaths pathTest;
        private DLTDLog dbg;

        public TestHarness()
        {
            dbg = new DLTDLog("[DLTD TestHarness] ");
        }

        public void Awake()
        {
            pathTest = new AssetManagement.KSPPaths();
            pathTest.Manufacturer = null;
            var pathKeys = pathTest.pathKeys;

            for( int i = 0; i < pathKeys.Count; i++ )
            {
                dbg.Print("path key " + pathKeys[i] + " " + pathTest[pathKeys[i]] + " " + pathTest[pathKeys[i]].Absolute());
            }
        }
    }

    [KSPAddon(KSPAddon.Startup.MainMenu, true)]
    class GDInvestigator : MonoBehaviour
    {
        private DLTDLog dbg;
        public GDInvestigator()
        {
            dbg = new DLTDLog("[GDBInvestigator] ");
        }

        public void LogGDCount<T>(List<T> list, string tag) where T : Object
        {
            dbg.Print("GameDatabase list database" + tag + " contains " + list.Count + " entries");
        }

        public void DumpContents<T>(List<T> list, string tag ) where T : Object
        {
            for (int i = 0; i < list.Count; i++)
                dbg.Print("List " + tag + "[" + i + "] " + list[i].name);
        }

        public void Awake()
        {
            var GDi = GameDatabase.Instance;
            LogGDCount(GDi.databaseAudio, "Audio");
            LogGDCount(GDi.databaseModel, "Model");
            LogGDCount(GDi.databaseShaders, "Shaders");

            dbg.Print("GameDatabase list databaseTexture contains " + GDi.databaseTexture.Count + " entries");
            dbg.Print("GameDatabase list databaseAudioFiles contains " + GDi.databaseAudioFiles.Count + " entries");
            dbg.Print("GameDatabase list databaseModelFiles contains " + GDi.databaseModelFiles.Count + " entries");

            DumpContents(GDi.databaseModel, "databaseModel");
            DumpContents(GDi.databaseAudio, "databaseAudio");
            DumpContents(GDi.databaseShaders, "databaseShaders");

            var assSources = AssetManagement.AssetManagement.assetSources;
            foreach( var s in assSources )
            {
                var alist = s.GetAssets();
                for(int i = 0; i < alist.Length; i++)
                    if ( alist[i] != null )
                        dbg.Print("AssetList " + tag + "[" + i + "] " + alist[i].UnityAsset.name);
            }
        }
    }
}
