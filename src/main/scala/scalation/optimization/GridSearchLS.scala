
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.6
 *  @date    Sat Feb  1 15:55:04 EST 2020
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Line Search Optimizer
 */

package scalation
package optimization

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GridSearchLS` class performs a line search on f(x) to find a minimal
 *  value for f.  It requires no derivatives and only one functional evaluation per
 *  iteration.  A search is conducted from x1 (often 0) to xmax.  A guess for xmax
 *  must be  given.  It works on scalar functions (see `gridSearchLSTest`).
 *  If starting with a vector function f(x), simply define a new function
 *  g(y) = x0 + direction * y (see `gridSearchLSTest2`).
 *  @param f  the scalar objective function to minimize
 */
class GridSearchLS (f: FunctionS2S)
      extends LineSearch:

    private val debug  = debugf ("GridSearchLS", true)                // debug function
    private val PLOT   = true                                         // show plot flag
    private val MAX_IT = 100                                          // maximum number of iterations

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an exact Line Search (LS) using the Grid Search Algorithm with defaults.
     *  @param step  the initial step size
     */
    def search (step: Double = 2.0): Double = lsearch (step)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an exact Line Search (LS) using the Grid Search Algorithm.
     *  @param xmax  a rough guess for the right end-point of the line search
     *  @param x1    the left (smallest) anchor point for the search (usually 0)
     */
    def lsearch (xmax: Double = 2.0, x1: Double = 0.0): Double =
        val d   = xmax - x1                                           // grid distance
        val xv  = VectorD.range (0, MAX_IT) * d / MAX_IT + x1         // vector of x values
        val fxv = new VectorD (xv.dim)                                // vector of f(x) values
        var fm  = Double.MaxValue                                     // min f(x) value
        var xm  = x1                                                  // value of x at min f(x) value

        for i <- xv.indices do
           val x  = xv(i)                                             // i-th value for x
           val fx = f(x)                                              // i-th value for function
           fxv(i) = fx                                                // record this value
           debug ("lsearch", s"for $i: x = $x, fx = $fx")
           if fx < fm then { xm = x; fm = fx }                        // found a new minumum?
        end for
        if PLOT then new Plot (xv, fxv, null, "fx vs x")
        xm                                                            // return the minimum x value
    end lsearch
 
end GridSearchLS


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `gridSearchLSTest` main function is used to test the `GridSearchLS` class on
 *  scalar functions.
 *  > runMain scalation.optimization.gridSearchLSTest
 */
@main def gridSearchLSTest (): Unit =

    def f (x: Double): Double = (x - 4.0) * (x - 4.0) + 1.0      // no expansion phase
    val solver = new GridSearchLS (f)
    println ("\nProblem 1: (x - 4)^2 + 1") 
    println ("optimal solution = " + solver.search (10.0))

end gridSearchLSTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `gridSearchLSTest2` main function is used to test the `GridSearchLS` class on
 *  vector functions.
 *  > runMain scalation.optimization.gridSearchLSTest
 */
@main def gridSearchLSTest2 (): Unit =

    val zo   = VectorD (0.0, 0.0)                       // zero vector, the origin
    val dir  = VectorD (1.0, 1.0)                       // direction to search in
    val ymax = 5.0
    var y    = 0.0
    var x    = zo

    def f (x: VectorD): Double  = (x(0) - 2.0) * (x(0) - 2.0) + (x(1) - 3.0) * (x(1) - 3.0) + 1.0
    def g (y: Double): Double = f(zo + dir * y)
    def f2 (x: VectorD): Double = x(0)/4.0 + 5.0*x(0)*x(0) + x(0)~^4 -
                                  9.0*x(0)*x(0)*x(1) + 3.0*x(1)*x(1) + 2.0*x(1)~^4
    def g2 (y: Double): Double = f2(zo + dir * y)

    val solver  = new GridSearchLS (g)
    val solver2 = new GridSearchLS (g2)

    println ("\nProblem 1: (x_0 - 2)^2 + (x_1 - 3)^2 + 1") 
    y = solver.search (ymax)
    println ("optimal y solution = " + y)
    x = zo + dir * y
    println ("optimal x solution = " + x)
    println ("optimal f solution = " + f(x))

    println ("\nProblem 4: x_0/4 + 5x_0^2 + x_0^4 - 9x_0^2 x_1 + 3x_1^2 + 2x_1^4")
    // @see http://math.fullerton.edu/mathews/n2003/gradientsearch/GradientSearchMod/Links/GradientSearchMod_lnk_5.html
    y = solver2.search (ymax)
    println ("optimal y solution = " + y)
    x = zo + dir * y
    println ("optimal x solution = " + x)
    println ("optimal f solution = " + f(x))

end gridSearchLSTest2

