
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Aug 21 13:46:30 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Possible Line Search Algorithms to be used with Quasi-Newton Algorithms
 */

package scalation
package optimization
package quasi_newton

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSLineSearchAlg` enumeration describes possible line
 *  search algorithms to be used in the L-BFGS algorithm when determining
 *  the size of the step to be taken in gradient descent.
 *
 *  @param number  nhmerical representation of the algorithm category
 */
enum LBFGSLineSearchAlg (val number: Int = 0):

    /** The default algorithm (MoreThuente method). */
    case Default extends LBFGSLineSearchAlg (0)

    /** MoreThuente method proposed by More and Thuente. */
    case MoreThuente extends LBFGSLineSearchAlg (0)

    /** Backtracking method with the Armijo condition.
     *  The backtracking method finds the step length such that it satisfies the
     *  sufficient decrease (Armijo) condition:
     *  f(x + a * d) &le; f(x) + `LBFGSPrms.ftol` * a * g(x)^T^ d.
     *
     *  Here, ''x'' is the current point, ''d'' is the current search direction,
     *  and ''a'' is the step length.
     */
    case BacktrackingArmijo extends LBFGSLineSearchAlg (1)

    /** The backtracking method with the default (regular Wolfe) condition. */
    case BacktrackingDefault extends LBFGSLineSearchAlg(2)

    /** Backtracking method with regular Wolfe condition.
     *  The backtracking method finds the step length such that it satisfies
     *  both the Armijo condition (see `LBFGSLineSearchAlg.BacktrackingArmijo`)\
     *  and the curvature condition:
     *  g(x + a * d)^T^ d &ge; `LBFGSPrms.wolfe` * g(x)^T^ d.
     *
     *  Here, ''x'' is the current point, ''d'' is the current search direction,
     *  and ''a'' is the step length.
     */
    case BacktrackingWolfe extends LBFGSLineSearchAlg (2)

    /** Backtracking method with strong Wolfe condition.
     *  The backtracking method finds the step length such that it satisfies
     *  both the Armijo condition (see `LBFGSLineSearchAlg.BacktrackingArmijo`)
     *  and the following condition:
     *  |g(x + a * d)^T^ d| &le; `LBFGSPrms.wolfe` * |g(x)^T^ d|.
     *
     *  Here, ''x'' is the current point, ''d'' is the current search direction,
     *  and ''a'' is the step length.
     */
    case BacktrackingStrongWolfe extends LBFGSLineSearchAlg (3)

    /** Backtracking method with Orthant-Wise. */
    case BacktrackingOrthantWise extends LBFGSLineSearchAlg (4)

end LBFGSLineSearchAlg

