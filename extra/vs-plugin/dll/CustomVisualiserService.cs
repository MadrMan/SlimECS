using Microsoft.VisualStudio.Debugger;
using Microsoft.VisualStudio.Debugger.CallStack;
using Microsoft.VisualStudio.Debugger.ComponentInterfaces;
using Microsoft.VisualStudio.Debugger.Evaluation;
using Microsoft.VisualStudio.Debugger.Native;
using Microsoft.VisualStudio.Shell.Interop;
using Microsoft.VisualStudio.TextManager.Interop;
using System;
using System.CodeDom;
using System.Collections.Generic;
using System.Runtime.InteropServices;

/*
 * For logging:
- Open a command prompt as an administrator
- Run: reg add HKLM\Software\Microsoft\VisualStudio\17.0\Debugger /v EngineDiagEnableState /t REG_DWORD /d 2 /f /reg:32 replacing '17.0' with whatever version of the remote debugger you want.
- When you are done, remove the key with: reg delete HKLM\Software\Microsoft\VisualStudio\17.0\Debugger /v EngineDiagEnableState /reg:32 /f

To view logs you can either have a native or mixed mode debugger attached to VS/msvsmon process that 
you are interested in and look in the Output Window, or run without a debugger and look at the %TMP%\vsdebugeng.dll.log file.
 */

namespace SlimECS
{
    public class GUIDs
    {
        public static readonly Guid DefaultGUID = new Guid("EDB7480E-9ED7-4C6F-AFDC-DA7AC9917A9E");
        public static readonly Guid VisualiserGUID = new Guid("51736b14-9fb4-4b6d-8aca-a10a2b7ae768");
    }

    public class CustomVisualiserService : IDkmCustomVisualizer
    {
        string fnGetEntityMetadata(string instance) => $"{instance}.m_ecsManager->m_ids[{instance}.m_parts.m_index]";
        string fnGetArchetype(string instance) => $"{instance}.m_ecsManager->m_archetypes[({fnGetEntityMetadata(instance)}).m_archetype]";
        string fnGetComponentHash(string instance, int index) => $"({fnGetArchetype(instance)})->m_chunks[{index}].m_componentID.m_hash";
        string fnGetComponentCount(string instance) => $"({fnGetArchetype(instance)})->m_chunks.capacity()";
        string fnGetComponentChunk(string instance, string chunkHash) => $"({fnGetArchetype(instance)})->GetChunkByHash({chunkHash})";
        string fnGetComponentChunkTypeVt(string instance, string chunkHash) => $"({fnGetComponentChunk(instance, chunkHash)})->m_type->__vfptr";
        string fnGetComponentData(string instance, string chunkHash, string componentType) => $"slimecs::ECSComponentType<{componentType}>::Get(({fnGetComponentChunk(instance, chunkHash)}), ({fnGetEntityMetadata(instance)}).m_idToChunk)";

        class CustomVisualiserDataItem : DkmDataItem
        {
            public string ecsInstanceFullName;
            public SlimECSReader.ECSInstance ecsInstance;
            public int componentCount = -1;
        }

        public unsafe void ReadAddressToObject<T>(out T obj, ulong address, DkmProcess targetProcess) where T : class, new()
        {
            T local = new T();

            GCHandle handle = GCHandle.Alloc(local, GCHandleType.Pinned);
            targetProcess.ReadMemory(address, DkmReadMemoryFlags.None, handle.AddrOfPinnedObject().ToPointer(), Marshal.SizeOf<T>());
            handle.Free();

            obj = local;
        }

        public unsafe string GetDerivedTypeFromVTable(DkmInspectionSession inspectionSession, ulong vtableAddress, DkmProcess targetProcess)
        {
            // First, get the first function out of the vtable
            ulong firstFunctionEntry;
            targetProcess.ReadMemory(vtableAddress, DkmReadMemoryFlags.None, &firstFunctionEntry, sizeof(ulong));

            // Find the module it's in
            var nativeModule = targetProcess.FindNativeModule(firstFunctionEntry);

            // Offset by module base and correct function to point to symbol start
            ulong rva = firstFunctionEntry - nativeModule.BaseAddress;

            DkmNativeInstructionSymbol s = DkmNativeInstructionSymbol.Create(nativeModule.Module, (uint)rva);
            string label = s.GetDisassemblyLabel(inspectionSession);

            return label;
        }

        public void EvaluateVisualizedExpression(DkmVisualizedExpression visualizedExpression, out DkmEvaluationResult resultObject)
        {
            var pointerValueHome = visualizedExpression.ValueHome as DkmPointerValueHome;
            if (pointerValueHome == null)
            {
                throw new System.NotImplementedException();
            }

            var rootVisualizedExpression = visualizedExpression as DkmRootVisualizedExpression;
            if (rootVisualizedExpression == null)
            {
                throw new System.NotImplementedException();
            }

            CustomVisualiserDataItem dataItem = new CustomVisualiserDataItem
            {
                ecsInstanceFullName = rootVisualizedExpression.FullName
            };

            DkmProcess targetProcess = visualizedExpression.RuntimeInstance.Process;
            ReadAddressToObject(out dataItem.ecsInstance, pointerValueHome.Address, targetProcess);

            DkmDataAddress address = DkmDataAddress.Create(visualizedExpression.RuntimeInstance, pointerValueHome.Address, null);
            
            resultObject = DkmSuccessEvaluationResult.Create(visualizedExpression.InspectionContext, visualizedExpression.StackFrame, rootVisualizedExpression.Name, rootVisualizedExpression.FullName,
                DkmEvaluationResultFlags.Expandable | DkmEvaluationResultFlags.ReadOnly, $"{{ id={dataItem.ecsInstance.Index} version={dataItem.ecsInstance.Version} }}", null, rootVisualizedExpression.Type, DkmEvaluationResultCategory.Data,
                DkmEvaluationResultAccessType.None, DkmEvaluationResultStorageType.None, DkmEvaluationResultTypeModifierFlags.None, address, null, null, null);

            visualizedExpression.SetDataItem(DkmDataCreationDisposition.CreateAlways, dataItem);
        }

        public void GetChildren(DkmVisualizedExpression visualizedExpression, int initialRequestSize, DkmInspectionContext inspectionContext, out DkmChildVisualizedExpression[] initialChildren, out DkmEvaluationResultEnumContext enumContext)
        {
            var eval = new DebuggerEval(visualizedExpression, inspectionContext);
            var dataItem = visualizedExpression.GetDataItem<CustomVisualiserDataItem>();

            if (dataItem.componentCount < 0)
            {
                var count = eval.Execute(fnGetComponentCount(dataItem.ecsInstanceFullName)) as DkmSuccessEvaluationResult;
                dataItem.componentCount = int.Parse(count.Value);
            }

            initialChildren = new DkmChildVisualizedExpression[0];
            int numberOfComponents = dataItem.componentCount;
            enumContext = DkmEvaluationResultEnumContext.Create(1 + numberOfComponents, visualizedExpression.StackFrame, inspectionContext, null);
        }

        public void GetItems(DkmVisualizedExpression visualizedExpression, DkmEvaluationResultEnumContext enumContext, int startIndex, int count, out DkmChildVisualizedExpression[] items)
        {
            var childList = new List<DkmChildVisualizedExpression>();
            var eval = new DebuggerEval(visualizedExpression, enumContext.InspectionContext);
            var dataItem = visualizedExpression.GetDataItem<CustomVisualiserDataItem>();

            {
                var r = eval.Execute(fnGetArchetype(dataItem.ecsInstanceFullName)) as DkmSuccessEvaluationResult;

                // Show default value for archetype
                DkmEvaluationResult archetypeResultObject = DkmSuccessEvaluationResult.Create(visualizedExpression.InspectionContext, visualizedExpression.StackFrame, "[Archetype]", r?.FullName,
                    r != null ? r.Flags : DkmEvaluationResultFlags.ReadOnly | DkmEvaluationResultFlags.Address, r?.Value ?? "<Invalid>", r?.EditableValue, r?.Type, DkmEvaluationResultCategory.Data,
                    DkmEvaluationResultAccessType.None, DkmEvaluationResultStorageType.None, DkmEvaluationResultTypeModifierFlags.None, r?.Address, null, null, null);
                childList.Add(DkmChildVisualizedExpression.Create(enumContext.InspectionContext, GUIDs.DefaultGUID, visualizedExpression.SourceId, visualizedExpression.StackFrame, null, archetypeResultObject, visualizedExpression, 0, null));
            }

            // Get all components
            for (int x = 0; x < dataItem.componentCount; x++)
            {
                string chunkHash = (eval.Execute(fnGetComponentHash(dataItem.ecsInstanceFullName, x)) as DkmSuccessEvaluationResult)?.Value;

                if (chunkHash != null)
                {
                    DkmDataAddress addr = (eval.Execute(fnGetComponentChunkTypeVt(dataItem.ecsInstanceFullName, chunkHash)) as DkmSuccessEvaluationResult)?.Address;
                    if (addr != null)
                    {
                        string label = GetDerivedTypeFromVTable(visualizedExpression.InspectionSession, addr.Value, addr.Process);
                        string componentName = $"Component#{x}";
                        string componentType = "void *";
                        string componentFullName = null;
                        string componentValue = "0x0";
                        DkmDataAddress componentAddress = DkmDataAddress.Create(visualizedExpression.RuntimeInstance, 0, null);

                        if (!string.IsNullOrEmpty(label))
                        {
                            // We got something in the form of slimcs::Component<Template>::Function
                            // "Template" could be anything (including subtemplates) but we assume the rest is controlled by us
                            int firstBracket = label.IndexOf("<") + 1;
                            string templateType = label.Substring(firstBracket, label.LastIndexOf(">") - firstBracket);

                            componentName = templateType;
                            componentFullName = fnGetComponentData(dataItem.ecsInstanceFullName, chunkHash, templateType);

                            if (eval.Execute(componentFullName) is DkmSuccessEvaluationResult r)
                            {
                                componentType = r.Type;
                                componentAddress = r.Address;
                                componentValue = r.Value;
                            }
                        }

                        DkmEvaluationResult componentEntry = DkmSuccessEvaluationResult.Create(visualizedExpression.InspectionContext, visualizedExpression.StackFrame, componentName, componentFullName,
                            DkmEvaluationResultFlags.Expandable | DkmEvaluationResultFlags.ReadOnly | DkmEvaluationResultFlags.Address, componentValue, null, componentType, DkmEvaluationResultCategory.Data,
                            DkmEvaluationResultAccessType.None, DkmEvaluationResultStorageType.None, DkmEvaluationResultTypeModifierFlags.None, componentAddress, null, null, null);
                        childList.Add(DkmChildVisualizedExpression.Create(enumContext.InspectionContext, GUIDs.DefaultGUID, visualizedExpression.SourceId, visualizedExpression.StackFrame, null, componentEntry, visualizedExpression, 0, null));
                    }
                }
            }

            // Return
            items = childList.ToArray();
        }

        public string GetUnderlyingString(DkmVisualizedExpression visualizedExpression)
        {
            throw new System.NotImplementedException();
        }

        public void SetValueAsString(DkmVisualizedExpression visualizedExpression, string value, int timeout, out string errorText)
        {
            throw new System.NotImplementedException();
        }

        public void UseDefaultEvaluationBehavior(DkmVisualizedExpression visualizedExpression, out bool useDefaultEvaluationBehavior, out DkmEvaluationResult defaultEvaluationResult)
        {
            if (visualizedExpression.VisualizerId != GUIDs.DefaultGUID)
            {
                useDefaultEvaluationBehavior = false;
                defaultEvaluationResult = null;

                return;
            }

            string fullName = null;

            if (visualizedExpression is DkmRootVisualizedExpression rootExpression)
            {
                fullName = rootExpression.FullName;
            }
            if (visualizedExpression is DkmChildVisualizedExpression childExpression)
            {
                fullName = childExpression.EvaluationResult?.FullName;
            }

            if (fullName == null)
            {
                throw new NotImplementedException();
            }

            var eval = new DebuggerEval(visualizedExpression, visualizedExpression.InspectionContext);
            useDefaultEvaluationBehavior = true;
            defaultEvaluationResult = eval.Execute(fullName);
        }
    }
}
