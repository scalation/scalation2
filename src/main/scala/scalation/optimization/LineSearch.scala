
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Oct 24 18:32:24 EDT 2012
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Base Trait for Line Search Optimizers
 */

package scalation
package optimization

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LineSearch` trait specifies the basic methods for Line Search (LS) algorithms
 *  in classes extending this trait to implement.  Line search is for one dimensional
 *  optimization problems.  The algorithms perform line search to find an 'x'-value
 *  that minimizes a function 'f' that is passed into an implementing class.
 *  <p>
 *      x* = argmin f(x)
 *  <p>
 */
trait LineSearch:

    protected val EPSILON = 1E-7                        // number close to zero

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform Line Search (LS) using a line search algorithm.
     *  @param step  the initial step size
     */
    def search (step: Double): Double

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform Line Search (LS) using a line search algorithm over the interval
     *  [x1, xmax].  Note, some line search implementations allow for expansion
     *  of the search interval.
     *  @param xmax  a rough guess for the right end-point of the line search
     *  @param x1    the left (smallest) anchor point for the search (usually 0)
     */
    def lsearch (xmax: Double, x1: Double = 0.0): Double

end LineSearch

