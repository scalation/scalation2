
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Wed Oct 18 13:41:49 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Method handles required for providing the optimization logic in the FFM
 *  implementation of the Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS)
 *  for Bound constrained optimization (L-BFGS-B) algorithm.
 */

// Package definition.
package scalation
package optimization
package LBFGS_FFM

// General imports.
import java.lang.invoke.{MethodHandle, MethodHandles}

// Case class.
case class OptimizationMethodHandlesFFM(
    evaluateMethodHandle: MethodHandle,
    progressMethodHandle: Option[MethodHandle] = None
)

// Companion object.
case object OptimizationMethodHandlesFFM:
    // Public methods.
    def bindFromFunctionOptimizationFFM(
        functionOptimizationLogic: FunctionOptimizationFFM
    ): OptimizationMethodHandlesFFM =
        val evaluateMethodHandle: MethodHandle = MethodHandles.lookup.bind(
            functionOptimizationLogic,
            "evaluate",
            MethodTypes.EVALUATE_METHOD_TYPE
        )
        val progressMethodHandle: MethodHandle = MethodHandles.lookup.bind(
            functionOptimizationLogic,
            "progress",
            MethodTypes.PROGRESS_METHOD_TYPE
        )

        OptimizationMethodHandlesFFM(evaluateMethodHandle, Some(progressMethodHandle))

end OptimizationMethodHandlesFFM