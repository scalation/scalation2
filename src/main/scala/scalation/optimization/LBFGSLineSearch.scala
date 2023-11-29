
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Fri Sep 8 10:20:06 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Trait that specifies the characteristics required in a line search algorithm
 *  used by the native implementation of the Limited memory
 *  Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained optimization
 *  (L-BFGS-B) algorithm.
 */

// Package definition.
package scalation
package optimization

// Project imports.
import scalation.mathstat.VectorD

// Type declarations.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSLineSearchReturn` type is a union type representing the return
 * value for the [[lineSearch]] method of line search algorithms used by
 * the native implementation of the L-BFGS algorithm.
 *
 * A successful execution should return [[LBFGSLineSearchStep]], while an
 * execution with errors should return a [[LBFGSReturnCode]] error code.
 * Returning a [[LBFGSReturnCode]] success code triggers undefined
 * behavior.
 */
type LBFGSLineSearchReturn = LBFGSLineSearchStep | LBFGSReturnCode

// Trait.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSLineSearch` trait specifies the requirements for a line search
 *  algorithm to be used in the native implementation of L-BFGS.
 *
 *  Classes mixing in this trait must implement the lineSearch method. The
 *  lineSearch method is used to find the optimal step, searching in a specific
 *  line, to be taken to minimize an objective function value.
 */
trait LBFGSLineSearch:
    // Public methods.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Searches for an optimal step to take along a given line in order to
     *  minimize the objective function value.
     *
     *  @param n                        The number of variables.
     *  @param x                        [[VectorD]] with the current values of
     *                                  the variables.
     *  @param f                        The current value of the objective
     *                                  function. Calculated with the variables
     *                                  in `x`.
     *  @param g                        [[VectorD]] with the current value of
     *                                  the gradient vector.
     *  @param s                        The line where the search for an optimal
     *                                  step will take place.
     *  @param stp                      The initial step to evaluate when
     *                                  searching for the optimal step along
     *                                  line `s`.
     *  @param cd                       [[LBFGSCallbackData]] to allow the line
     *                                  search algorithm to evaluate the
     *                                  objective function value and gradient
     *                                  vector for a given set of variable
     *                                  values.
     *  @param params                   [[LBFGSParameters]] representing the
     *                                  parameters chosen to control the L-BFGS
     *                                  optimization process, which includes
     *                                  some line step search parameters.
     *  @return LBFGSLineSearchReturn   The results of the line search method. A
     *                                  successful execution returns a
     *                                  [[LBFGSLineSearchStep]], while an
     *                                  execution with errors returns a
     *                                  [[LBFGSReturnCode]] error code.
     *
     */
    def lineSearch(
        n: Int,
        x: VectorD,
        f: Double,
        g: VectorD,
        s: VectorD,
        stp: Double,
        cd: LBFGSCallbackData,
        params: LBFGSParameters
    ): LBFGSLineSearchReturn
end LBFGSLineSearch
