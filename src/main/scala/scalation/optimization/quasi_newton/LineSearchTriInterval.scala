
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Sep 11 22:06:31 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Step Trial Interval Data Used by Line Search Algorithms
 */

package scalation
package optimization
package quasi_newton

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LineSearchTriInterval` case class represents a trial interval for
 *  determining an optimal step, including the next step that should be
 *  evaluated based on the current endpoints of the trial interval.  This class
 *  is used by the line search algorithm when attempting to minimize the
 *  objective function.  In each iteration of the line search algorithm, the
 *  trial interval will be updated until certain conditions are met that result
 *  in a line search step being selected.
 *
 *  @param x       Value of the endpoint in the step trial interval that yields
 *                 the least function value at the moment.
 *  @param fx      Objective function value obtained by using step `x`.
 *  @param dx      Derivative of the objective function obtained by using step `x`.
 *  @param y       Value of the second endpoint in the step trial interval (the
 *                 first endpoint being `x`).
 *  @param fy      Objective function value obtained by using step `y`.
 *  @param dy      Derivative of the objective function obtained by using step `y`.
 *  @param t       The new step chosen to be evaluated in an iteration of the
 *                 line search algorithm based on the trial interval information.
 *  @param brackt  Indicates if the trial value `t` is bracketed. If set to
 *                 true, the minimizer has been bracketed in an interval of
 *                 uncertainty with endpoints between `x` and `y`.
 *
 *  @see           Jorge J. More and David J. Thuente. Line search algorithm
 *                 with guaranteed sufficient decrease. ACM Transactions on
 *                 Mathematical Software (TOMS), Vol 20, No 3, pp. 286-307, 1994.
 */
case class LineSearchTriInterval (x: Double, fx: Double, dx: Double, y: Double,
                                  fy: Double, dy: Double, t: Double, brackt: Boolean)

end LineSearchTriInterval

