
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Aug 21 13:46:30 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Possible line search algorithms to be used to determine the step size in the
 *  Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained
 *  optimization (L-BFGS-B) algorithm..
 */

// Package definition.
package scalation.optimization

// Enumeration.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSLineSearchAlgorithm` enumeration describes possible line
 *  search algorithms to be used in the L-BFGS algorithm when determining
 *  the size of the step to be taken in gradient descent.
 *
 *  @param number   Numerical representation of the algorithm category.
 */
enum LBFGSLineSearchAlgorithm(val number: Int = 0):
    // Cases.
    /** The default algorithm (MoreThuente method). */
    case Default extends LBFGSLineSearchAlgorithm(0)

    /** MoreThuente method proposed by More and Thuente. */
    case MoreThuente extends LBFGSLineSearchAlgorithm(0)

    /** Backtracking method with the Armijo condition.
     *  The backtracking method finds the step length such that it satisfies the
     *  sufficient decrease (Armijo) condition:
     *  f(x + a * d) &le; f(x) + `LBFGSParameters.ftol` * a * g(x)^T^ d.
     *
     *  Here, ''x'' is the current point, ''d'' is the current search direction,
     *  and ''a'' is the step length.
     */
    case BacktrackingArmijo extends LBFGSLineSearchAlgorithm(1)

    /** The backtracking method with the default (regular Wolfe) condition. */
    case BacktrackingDefault extends LBFGSLineSearchAlgorithm(2)

    /** Backtracking method with regular Wolfe condition.
     *  The backtracking method finds the step length such that it satisfies
     *  both the Armijo condition (see
     *  [[LBFGSLineSearchAlgorithm.BacktrackingArmijo]]) and the curvature
     *  condition:
     *  g(x + a * d)^T^ d &ge; `LBFGSParameters.wolfe` * g(x)^T^ d.
     *
     *  Here, ''x'' is the current point, ''d'' is the current search direction,
     *  and ''a'' is the step length.
     */
    case BacktrackingWolfe extends LBFGSLineSearchAlgorithm(2)

    /** Backtracking method with strong Wolfe condition.
     *  The backtracking method finds the step length such that it satisfies
     *  both the Armijo condition (see
     *  [[LBFGSLineSearchAlgorithm.BacktrackingArmijo]]) and the following
     *  condition:
     *  |g(x + a * d)^T^ d| &le; `LBFGSParameters.wolfe` * |g(x)^T^ d|.
     *
     *  Here, ''x'' is the current point, ''d'' is the current search direction,
     *  and ''a'' is the step length.
     */
    case BacktrackingStrongWolfe extends LBFGSLineSearchAlgorithm(3)
    /** Backtracking method with Orthant-Wise. */
    case BacktrackingOrthantWise extends LBFGSLineSearchAlgorithm(4)
end LBFGSLineSearchAlgorithm
