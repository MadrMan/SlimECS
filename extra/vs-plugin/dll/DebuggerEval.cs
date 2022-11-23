using Microsoft.VisualStudio.Debugger;
using Microsoft.VisualStudio.Debugger.Evaluation;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace SlimECS
{
    internal class DebuggerEval
    {
        DkmVisualizedExpression visualizedExpression;
        DkmInspectionContext inspectionContext;

        public DebuggerEval(DkmVisualizedExpression visualizedExpression, DkmInspectionContext inspectionContext)
        {
            this.visualizedExpression = visualizedExpression;
            
            // Clone inspection context but enable side effects, since we want to be able to reliably do function calls
            this.inspectionContext = DkmInspectionContext.Create(inspectionContext.InspectionSession, inspectionContext.RuntimeInstance,
                inspectionContext.Thread, inspectionContext.Timeout, inspectionContext.EvaluationFlags & ~DkmEvaluationFlags.NoSideEffects,
                inspectionContext.FuncEvalFlags, inspectionContext.Radix, inspectionContext.Language,
                inspectionContext.ReturnValue, inspectionContext.AdditionalVisualizationData,
                inspectionContext.AdditionalVisualizationDataPriority, inspectionContext.ReturnValues, inspectionContext.SymbolsConnection);
        }

        public DkmEvaluationResult Execute(string code)
        {
            var expression = DkmLanguageExpression.Create(inspectionContext.Language, DkmEvaluationFlags.TreatAsExpression, code, null);
            DkmEvaluationResult r;
            visualizedExpression.EvaluateExpressionCallback(inspectionContext, expression, visualizedExpression.StackFrame, out r);
            expression.Close();

            if (r is DkmFailedEvaluationResult)
            {
                var failed = (DkmFailedEvaluationResult)r;
                Debug.WriteLine($"Failed to evaluate: {code}: {failed.ErrorMessage}");
            }

            return r;
        }
    }
}
