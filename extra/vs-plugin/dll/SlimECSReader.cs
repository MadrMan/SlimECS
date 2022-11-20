using Microsoft.VisualStudio.OLE.Interop;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;

namespace SlimECS
{
    internal class SlimECSReader
    {
        [StructLayout(LayoutKind.Sequential)]
        public class ECSInstance
        {
            ulong indexAndVersion; // id = 48, version = 16
            ulong ptrToManager;

            public ulong Index
            {
                get => indexAndVersion & 0xfffffffffffful;
            }

            public ulong Version
            {
                get => indexAndVersion >> 48;
            }
        }
    }
}
