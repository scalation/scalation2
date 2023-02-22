
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Aug 24 19:53:22 EDT 2011
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Line Search Optimizer
 *
 *  @see pages.cs.wisc.edu/~ferris/cs730/chap3.pdf
 *  @see Limited Memory BFGS for Nonsmooth Optimization
 */

package scalation
package optimization

import scala.math.abs
import scala.util.control.Breaks.{break, breakable}

import scalation.calculus.Differential.derivative
import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `WolfeLS` class performs an inexact line search on 'f' to find a point 'x'
 *  that exhibits
 *  (1) sufficient decrease ('f(x)' enough less that 'f(0)') and
 *  (2) the slope at x is less steep than the slope at 0.
 *  That is, the line search looks for a value for 'x' satisfying the two Wolfe conditions.
 *  
 *     f(x) <= f(0) + c1 * f'(0) * x      Wolfe condition 1 (Armijo condition)
 *  |f'(x)| <= |c2 * f'(0)|               Wolfe condition 2 (Strong version)
 *    f'(x) >= c2 * f'(0)                 Wolfe condition 2 (Weak version, more robust)
 *  
 *  It works on scalar functions (@see `wolfeLSTest`).  If starting with a vector function
 *  f(x), simply define a new function g(y) = x0 + direction * y (@see `wolfeLSTest2`).
 *  
 *  @param f   the scalar objective function to minimize
 *  @param c1  constant for sufficient decrease (Wolfe condition 1)
 *  @param c2  constant for curvature/slope constraint (Wolfe condition 2)
 */
class WolfeLS (f: FunctionS2S, c1: Double = .0001, c2: Double = .9)
      extends LineSearch:

    private val debug    = debugf ("WolfeLS", false)            // debug function
    private val MAX_ITER = 20                                   // maximum number of iterations
    private val f0       = f(0.0)                               // functional value at the origin
    private val df0      = derivative (f, 0.0)                  // derivative at the origin f'(0)
    private val c1_df0   = c1 * df0                             // pre-multiplication for Wolfe condition 1
    private val c2_df0   = c2 * df0                             // pre-multiplication for Wolfe condition 2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an inexact Line Search (LS) using the Wolfe approach with defaults.
     *  @param step  the initial step size
     */
    def search (step: Double = 1.0): Double = lsearch_ (step)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an inexact Line Search (LS) on the function 'f' to find a point
     *  'x' that satisfies the Wolfe Conditions 1 and 2.
     *  @param x0   the current point
     *  @param lo0  the lower bound for x
     */
    def lsearch (x0: Double = 1.0, lo0: Double = 0.0): Double = lsearch_ (x0, lo0)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an inexact Line Search (LS) on the function 'f' to find a point
     *  'x' that satisfies the Wolfe Conditions 1 and 2.
     *  @param x0    the current point
     *  @param lo0   the lower bound for x
     *  @param weak  whether to use the weak (true) or strong (false) Wolfe conditions
     */
    def lsearch_ (x0: Double = 1.0, lo0: Double = 0.0, weak: Boolean = true): Double =
        var lo  = lo0
        var x   = x0
        var hi  = Double.PositiveInfinity                       // upper bound for x
        var fx  = f(x)                                          // function at x, f(x)
        var dfx = derivative (f, x)                             // derivative at x, f'(x)

        breakable {
            for k <- 1 to MAX_ITER do
                if fx > f0 + c1_df0 * x then                        // Wolfe condition 1 fails
                    hi = x
                else if weak && dfx < c2_df0 then                   // Weak Wolfe condition 2 fails
                    lo = x
                else if ! weak && abs (dfx) > abs (c2_df0) then     // Strong Wolfe condition 2 fails
                    lo = x
                else
                    break ()                                        // both conditions satisfied
                end if

                x  = if hi < Double.PositiveInfinity then (lo + hi) / 2.0 else x + x
                fx = f(x); dfx = derivative (f, x)                  // recompute f(x) and f'(x)
                debug ("lsearch_", s"(k = $k) x = $x, f(x) = $fx")
            end for
        } // breakable
        x
    end lsearch_

end WolfeLS


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `wolfeLSTest` main function is used to test the `WolfeLS` class on scalar functions.
 *  > runMain scalation.optimization.wolfeLSTest
 */
@main def wolfeLSTest (): Unit =

    def f (x: Double): Double = (x - 4.0) * (x - 4.0) + 1.0        // no expansion phase
//  def f (x: Double): Double = (x - 40.0) * (x - 40.0) + 1.0      // requires expansion phase
    val solver = new WolfeLS (f)
    println ("\nProblem 1: (x - 40)^2 + 1") 
    println ("optimal solution = " + solver.search ())

end wolfeLSTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `wolfeLSTest2` main function is used to test the `WolfeLS` class on vector functions.
 *  > runMain scalation.optimization.wolfeLSTest2
 */
@main def wolfeLSTest2 (): Unit =

    val zo   = VectorD (0.0, 0.0)                       // zero vector, the origin
    val dir  = VectorD (1.0, 1.0)                       // direction to search in
    var y    = 0.0
    var x    = zo

    def f (x: VectorD): Double  = (x(0) - 2.0) * (x(0) - 2.0) + (x(1) - 3.0) * (x(1) - 3.0) + 1.0
    def g (y: Double): Double   = f(zo + dir * y)
    def f2 (x: VectorD): Double = x(0)/4 + 5*x(0)~^2 + x(0)~^4 - 9*x(0)~^2*x(1) + 3*x(1)~^2 + 2*x(1)~^4
    def g2 (y: Double): Double  = f2(zo + dir * y)

    val solver  = new WolfeLS (g)
    val solver2 = new WolfeLS (g2)

    println ("\nProblem 1: (x_0 - 2)^2 + (x_1 - 3)^2 + 1") 
    y = solver.search ()
    println ("optimal y solution = " + y)
    x = zo + dir * y
    println ("optimal x solution = " + x)
    println ("optimal f solution = " + f(x))

    println ("\nProblem 4: x_0/4 + 5x_0^2 + x_0^4 - 9x_0^2 x_1 + 3x_1^2 + 2x_1^4")
    // @see http://math.fullerton.edu/mathews/n2003/gradientsearch/GradientSearchMod/Links/GradientSearchMod_lnk_5.html
    y = solver2.search ()
    println ("optimal y solution = " + y)
    x = zo + dir * y
    println ("optimal x solution = " + x)
    println ("optimal f solution = " + f(x))

end wolfeLSTest2

