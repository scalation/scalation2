
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Aug 22 13:45:22 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Results obtained from a call to a method implementing the Limited memory
 *  Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained optimization
 *  (L-BFGS-B) algorithm.
 */

// Package definition.
package scalation.optimization

// Project imports.
import scalation.mathstat.VectorD

// Case class.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSResults` class is used to group together all the outputs resulting
 *  from a call to a method implementing the L-BFGS optimization algorithm.
 *
 *  @param returnCode           [[LBFGSReturnCode]] that represents the outcome
 *                              of the L-BFGS optimization.
 *  @param optimizedVariables   [[VectorD]] that contains the optimized values
 *                              of the variables that were provided as inputs to
 *                              the L-BFGS optimization.
 *  @param finalFunctionValue   [[Option]] value that represents the final value
 *                              obtained for the objective function in the
 *                              L-BFGS optimization. If the objective function
 *                              was never evaluated due to errors in the
 *                              arguments provided by the user to the L-BFGS
 *                              method, this field will be set to [[None]] or
 *                              `Some(0)` depending on how the L-BFGS method was
 *                              implemented. For new L-BFGS implementations,
 *                              returning [[None]] is preferred over `Some(0)`
 *                              when the objective function is never evaluated.
 */
case class LBFGSResults(
    returnCode: LBFGSReturnCode,
    optimizedVariables: VectorD,
    finalFunctionValue: Option[Double]
)
