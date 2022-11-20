using Microsoft.VisualStudio.Debugger;
using Microsoft.VisualStudio.Debugger.CallStack;
using Microsoft.VisualStudio.Debugger.ComponentInterfaces;
using Microsoft.VisualStudio.Debugger.Evaluation;
using System.Runtime.InteropServices;

namespace SlimECS
{
    public class CustomVisualiserService : IDkmCustomVisualizer
    {
        public unsafe void ReadAddressToObject<T>(out T obj, ulong address, DkmProcess targetProcess) where T : class, new()
        {
            T local = new T();

            GCHandle handle = GCHandle.Alloc(local, GCHandleType.Pinned);
            targetProcess.ReadMemory(address, DkmReadMemoryFlags.None, handle.AddrOfPinnedObject().ToPointer(), Marshal.SizeOf<T>());
            handle.Free();

            obj = local;
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

            DkmProcess targetProcess = visualizedExpression.RuntimeInstance.Process;
            ReadAddressToObject(out SlimECSReader.ECSInstance ecsInstance, pointerValueHome.Address, targetProcess);

            DkmDataAddress address = DkmDataAddress.Create(visualizedExpression.RuntimeInstance, pointerValueHome.Address, null);

            resultObject = DkmSuccessEvaluationResult.Create(visualizedExpression.InspectionContext, visualizedExpression.StackFrame, rootVisualizedExpression.Name, rootVisualizedExpression.FullName,
                DkmEvaluationResultFlags.Expandable | DkmEvaluationResultFlags.ReadOnly, $"{ id={ecsInstance.Index} version={ecsInstance.Version} }", "Maybe", rootVisualizedExpression.Type, DkmEvaluationResultCategory.Property,
                DkmEvaluationResultAccessType.None, DkmEvaluationResultStorageType.None, DkmEvaluationResultTypeModifierFlags.None, address, null, null, null);
        }

        public void GetChildren(DkmVisualizedExpression visualizedExpression, int initialRequestSize, DkmInspectionContext inspectionContext, out DkmChildVisualizedExpression[] initialChildren, out DkmEvaluationResultEnumContext enumContext)
        {
            throw new System.NotImplementedException();
        }

        public void GetItems(DkmVisualizedExpression visualizedExpression, DkmEvaluationResultEnumContext enumContext, int startIndex, int count, out DkmChildVisualizedExpression[] items)
        {
            throw new System.NotImplementedException();
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
            throw new System.NotImplementedException();
        }
    }
}
