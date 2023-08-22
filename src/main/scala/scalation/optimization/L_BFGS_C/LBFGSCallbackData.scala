
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

// General imports.
import java.lang.foreign.MemorySegment

// Case class.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSCallbackData` class is used to group together the
 *  [[OptimizationLogic]] specified for a L-BFGS optimization with values that
 *  are parameters for the methods of the [[OptimizationLogic]]. This allows the
 *  user to pass the optimization logic of the L-BFGS optimization as a
 *  parameter to different methods and classes while retaining the ability to
 *  callback the methods of said logic with the correct parameters.
 *
 *  @param n                    The number of variables used in the
 *                              optimization.
 *  @param instance             [[MemorySegment]] containing the user data
 *                              provided for a given call of the L-BFGS
 *                              optimization. Must be compatible with the
 *                              `MemoryLayout` format expected from the
 *                              `optimizationLogic` parameter.
 *  @param optimizationLogic    [[OptimizationLogic]] that describes the
 *                              optimization steps for the L-BFGS optimization.
 */
case class LBFGSCallbackData(
    n: Int,
    instance: MemorySegment,
    optimizationLogic: OptimizationLogic
)
