
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Jun  7 17:54:59 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Newton-Raphson Method to Find Roots or Minima for Scalar Functions
 *
 *  @see web.stanford.edu/class/cme304/docs/newton-type-methods.pdf
 */

package scalation
package optimization

import scala.math.abs

import scalation.calculus.Differential.{Ⅾ, ⅮⅮ}
import scalation.mathstat.VectorD

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NewtonRaphson` class is used to find roots (zeros) for a one-dimensional
 *  (scalar) function 'f'.  The solve method finds zeros for function 'f', while
 *  the optimize method finds local optima using the same logic, but applied
 *  to first and second derivatives.
 *  @param f  the scalar function to find roots/optima of
 */
class NewtonRaphson (f: FunctionS2S)
      extends Minimize:

    private val debug    = debugf ("NewtonRaphson", true)         // debug function

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for/find a root close to the starting point/guess 'x0'.
     *  This version passes in a function for the derivative.
     *  @param x0  the starting point/guess
     *  @param df  the derivative of the function
     */
    def solve (x0: Double, df: FunctionS2S): Double =
        var x    = x0                                             // current point
        var f_x  = f(x)                                           // function value at x
        var df_x = -0.0                                           // derivative value at x

        var it = 1
        cfor (it <= MAX_IT && abs (f_x) > EPS, it += 1) {
            df_x = maxmag (df(x), EPS)                            // make sure derivative is not too small
            debug ("solve", s"it = $it: f($x) = $f_x, df_x = $df_x")
            x  -= f_x / df_x * eta                                // subtract the ratio
            f_x = f(x)
        } // cfor

        printf ("solution x = %10.5f, f = %10.5f\n", x, f(x))
        x
    end solve

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for/find a root close to the starting point/guess 'x0'.
     *  This version numerically approximates the derivative.
     *  @param x0  the starting point/guess
     */
    def solve (x0: Double): Double =
        var x    = x0                                             // current point
        var f_x  = f(x)                                           // function value at x
        var df_x = -0.0                                           // derivative value at x

        var it = 1
        cfor (it <= MAX_IT && abs (f_x) > EPS, it += 1) {
            df_x = maxmag (Ⅾ (f)(x), EPS)                         // make sure derivative is not too small
            debug ("solve", s"it = $it: f($x) = $f_x, df_x = $df_x")
            x  -= eta * f_x / df_x                                // subtract the ratio
            f_x = f(x)
        } // cfor

        printf ("solution x = %10.5f, f = %10.5f\n", x, f(x))
        x
    end solve

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Optimize the function by finding a local optima close to the starting point/guess 'x0'.
     *  This version numerically approximates the first and second derivatives.
     *  @param x0  the starting point/guess
     */
    def optimize (x0: Double): (Double, Double) =
        var x    = x0                                             // current point
        var f_x  = f(x)                                           // function value at x
        var df_x = 1.0                                            // derivative value at x

        var it = 1
        cfor (it <= MAX_IT && abs (df_x) > EPS, it += 1) {
            df_x      = maxmag (Ⅾ (f)(x), EPS)                    // make sure derivative is not too small
            val d2f_x = maxmag (ⅮⅮ (f)(x), EPS)                   // make sure 2nd derivative is not too small
            debug ("solve", s"it = $it: f($x) = $f_x, df_x = $df_x")
            x  -= eta * df_x / d2f_x                              // subtract the ratio
            f_x = f(x)
        } // cfor

        printf ("optimal solution x = %10.5f, f = %10.5f\n", x, f(x))
        (f_x, x)
    end optimize

    def solve (x0: VectorD, α: Double = eta): FuncVec = 
        val res = optimize (x0(0))
        (res._1, VectorD (res._2))
    end solve

end NewtonRaphson


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `newtonRaphsonTest` main function is used to test the `NewtonRaphson` class.
 *  This test passes in a function for the derivative to find a root.
 *  > runMain scalation.optimization.newtonRaphsonTest
 */
@main def newtonRaphsonTest (): Unit =

//  def f (x: Double): Double = 2 * (x*x - 3)
    def f (x: Double): Double = (x - 4) * (x - 4) - 2
    def df (x: Double): Double = 2 * (x - 4)

    val nr = new NewtonRaphson (f)
    
    nr.solve (0.0, df)

end newtonRaphsonTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `newtonRaphsonTest2` main function is used to test the `NewtonRaphson` class.
 *  This test numerically approximates the derivative to find a root.
 *  > runMain scalation.optimization.newtonRaphsonTest2
 */
@main def newtonRaphsonTest2 (): Unit =

//  def f (x: Double): Double = 2 * (x*x - 3)
    def f (x: Double): Double = (x - 4) * (x - 4) - 2

    val nr = new NewtonRaphson (f)
   
    nr.solve (0.0)

end newtonRaphsonTest2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `newtonRaphsonTest3` main function is used to test the `NewtonRaphson` class.
 *  This test numerically approximates the derivatives to find minima.
 *  > runMain scalation.optimization.newtonRaphsonTest3
 */
@main def newtonRaphsonTest3 (): Unit =

//  def f (x: Double): Double = 2 * (x*x - 3)
    def f (x: Double): Double = (x - 4) * (x - 4) - 2

    val nr = new NewtonRaphson (f)
  
    nr.optimize (0.0)

end newtonRaphsonTest3

