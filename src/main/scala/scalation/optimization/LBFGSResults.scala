
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
package scalation
package optimization

// Project imports.
import scalation.mathstat.VectorD

// Case class.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSResults` class is used to group together all the outputs resulting
 *  from a call to a method implementing the L-BFGS optimization algorithm.
 *
 *  @param returnCode                   [[LBFGSReturnCode]] that represents the
 *                                      outcome of the L-BFGS optimization.
 *  @param optimizedVariables           [[VectorD]] that contains the optimized
 *                                      values of the variables that were
 *                                      provided as inputs to the L-BFGS
 *                                      optimization.
 *  @param finalFunctionValue           [[Option]] value that represents the
 *                                      final value obtained for the objective
 *                                      function in the L-BFGS optimization. If
 *                                      the objective function was never
 *                                      evaluated due to errors in the arguments
 *                                      provided by the user to the L-BFGS
 *                                      method, this field will be set to
 *                                      [[None]] or `Some(0)` depending on how
 *                                      the L-BFGS method was implemented. For
 *                                      new L-BFGS implementations, returning
 *                                      [[None]] is preferred over `Some(0)`
 *                                      when the objective function is never
 *                                      evaluated.
 *  @param lineSearchIncompleteResults  [[Option]] value that represents the
 *                                      best incomplete results obtained by the
 *                                      line search algorithm before a failure
 *                                      occurred when performing a line search
 *                                      during the L-BFGS optimization. If the
 *                                      L-BFGS optimization is successful or
 *                                      produces an error that is not the result
 *                                      of a call to a line search algorithm,
 *                                      this value will be set to [[None]]. If
 *                                      the L-BFGS optimization is stopped due
 *                                      to an error produced by a call to a line
 *                                      search algorithm, this value will be set
 *                                      to [[Some]] with an instance of
 *                                      [[LBFGSLineSearchIncompleteResults]]
 *                                      that represents the best result obtained
 *                                      by the line search algorithm before the
 *                                      error occurred. Some L-BFGS
 *                                      implementations are incapable of
 *                                      returning this data and will always
 *                                      return [[None]], regardless of the
 *                                      circumstances.
 */
case class LBFGSResults(
    returnCode: LBFGSReturnCode,
    optimizedVariables: VectorD,
    finalFunctionValue: Option[Double],
    lineSearchIncompleteResults: Option[LBFGSLineSearchIncompleteResults]
)
