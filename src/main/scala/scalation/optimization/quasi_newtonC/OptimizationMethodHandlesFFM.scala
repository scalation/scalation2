
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Wed Oct 18 13:41:49 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Method handles required for providing the optimization logic in the FFM
 *  implementation of the Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS)
 *  for unconstrained optimization (L-BFGS) algorithm.
 */

package scalation
package optimization
package quasi_newtonC

import java.lang.invoke.{MethodHandle, MethodHandles}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `OptimizationMethodHandlesFFM` case class ...
 */
case class OptimizationMethodHandlesFFM (evaluateMethodHandle: MethodHandle,
                                         progressMethodHandle: Option [MethodHandle] = None)


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `OptimizationMethodHandlesFFM` case object ...
 */
case object OptimizationMethodHandlesFFM:

    def bindFromFunctionOptimizationFFM (functionOptimizationLogic: FunctionOptimizationFFM):
            OptimizationMethodHandlesFFM =
        val evaluateMethodHandle: MethodHandle = MethodHandles.lookup.bind (
            functionOptimizationLogic,
            "evaluate",
            MethodTypes.EVALUATE_METHOD_TYPE)

        val progressMethodHandle: MethodHandle = MethodHandles.lookup.bind (
            functionOptimizationLogic,
            "progress",
            MethodTypes.PROGRESS_METHOD_TYPE)

        OptimizationMethodHandlesFFM (evaluateMethodHandle, Some(progressMethodHandle))
    end bindFromFunctionOptimizationFFM

end OptimizationMethodHandlesFFM

