
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Aug 21 13:51:22 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Callback data used in the native implementation of the Limited memory
 *  Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained optimization
 *  (L-BFGS-B) algorithm.
 */

// Package.
package scalation.optimization.L_BFGS_C

// Case class.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSCallbackData` class is used to group together the
 *  [[OptimizationLogicNative]] specified for a L-BFGS optimization done by the
 *  [[Native]] object with values that are the parameters for the methods of the
 *  [[OptimizationLogicNative]]. This allows the user to pass the optimization
 *  logic of the native L-BFGS optimization as a parameter to different methods
 *  and classes while retaining the ability to callback the methods of said
 *  logic with the correct parameters.
 *
 *  @param n                    The number of variables used in the
 *                              optimization.
 *  @param instance             User data provided for a given call of the
 *                              L-BFGS optimization done by `lbfgsMain` on the
 *                              [[Native]] object. Can have [[Any]] type defined
 *                              by the user as long as it is the same one
 *                              expected by the `optimizationLogic` parameter.
 *  @param optimizationLogic    [[OptimizationLogicNative]] that describes the
 *                              optimization steps for the L-BFGS optimization
 *                              done by the [[Native]] object.
 */
case class LBFGSCallbackData(
    n: Int,
    instance: Any,
    optimizationLogic: OptimizationLogicNative
)
