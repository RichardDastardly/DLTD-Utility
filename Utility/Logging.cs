using UnityEngine;

namespace DLTD.Utility
{
    #region DLTDLog
    public class DLTDLog
    {
        // debugging stuff from the start! how novel
        // dump this when we're done
        private string dbgTag = "[DLTD Debug] ";

        public static bool Enabled = true;

        public static void Log( string dbgString )
        {
            if( Enabled )
                Debug.Log("[DLTD Debug] " + dbgString);
        }

        private static void writeLog( string dbgString )
        {
            if (Enabled)
                Debug.Log(dbgString);
        }

        private static void writeWarn(string dbgString)
        {
            if (Enabled)
                Debug.LogWarning(dbgString);
        }

        private static void writeErr(string dbgString)
        {
            if (Enabled)
                Debug.LogError(dbgString);
        }

        public DLTDLog() { }
        public DLTDLog(string tag)
        {
            dbgTag = tag;
        }

        public string DbgTag
        {
            get { return dbgTag; }
            set { dbgTag = value; }
        }

        public void Print(string dbgString)
        {
            writeLog(dbgTag + dbgString);
        }

        public void Warn(string tag, string dbgString)
        {
            writeWarn(tag + dbgString);
        }

        public void Err(string dbgString)
        {
            writeErr(dbgTag + dbgString);
        }
    }
    #endregion
}
