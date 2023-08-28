
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Jun  7 17:54:59 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   BFGS, Quasi Method Method to Find Minima for Functions of Vectors
 *
 *  @see web.stanford.edu/class/cme304/docs/newton-type-methods.pdf
 *  @see `BFGS` for similar code that uses line-search
 */

package scalation
package optimization

import scala.math.abs

import scalation.calculus.Differential.∇
import scalation.mathstat._

import MatrixD.{outer, ⊗}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QNewton` object provides methods useful for Quasi Newton optimizers.
 */
object QNewton:

    private val EPS = Minimize.hp("eps").toDouble

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the change to the approximate Hessian inverse (aHi) matrix using the
     *  Sherman–Morrison formula.
     *  @see https://en.wikipedia.org/wiki/Broyden%E2%80%93Fletcher%E2%80%93Goldfarb%E2%80%93Shanno_algorithm
     *  @see https://mdav.ece.gatech.edu/ece-6270-spring2021/notes/09-bfgs.pdf
     *  @param aHi  the current value of the approximate Hessian inverse (aHi)
     *  @param s    the step vector (next point - current point)
     *  @param y    the difference in the gradients (next - current)
     */
    def aHi_inc (aHi: MatrixD, s: VectorD, y: VectorD): MatrixD =
        var sy = maxmag (s dot y, EPS)
        val ay = aHi * y
        (⊗ (s, s) * (sy + (y dot ay))) / sy~^2 - (⊗ (ay, s) + ⊗ (s, ay)) / sy
    end aHi_inc

end QNewton

import QNewton.aHi_inc

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BFGS_NoLS` class is used to find optima for functions of vectors.
 *  The solve method finds local optima using a Quasi Newton method that uses
 *  the BFGS update to approximate the inverse Hessian.
 * 
 *      min f(x)    where f: R^n -> R
 *
 *  @see `BFGS` for one that uses a different line search.
 *  @param f      the vector to scalar function to find optima of
 *  @param useLS  whether to use Line Search (LS)
 */
class BFGS_NoLS (f: FunctionV2S, useLS: Boolean = false)
      extends Minimize:

    private val debug    = debugf ("BFGS_NoLS", true)             // debug function
    private val gradDesc = false                                  // true = Gradient Descent
                                                                  // false => BFGS_NoLS Method

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for an optima by finding a local optima close to the starting point/guess 'x0'.
     *  This version numerically approximates the first derivatives.
     *  @param x0  the starting point/guess
     *  @param α   the current learning rate
     */
    def solve (x0: VectorD, α: Double = eta): FuncVec =
        val wls   = new WolfeLS2 (f, null)                        // Wolfe Line Search
        var x     = x0                                            // current point
        var f_x   = f(x)                                          // function value at x
        var df_x  = ∇ (f)(x)                                      // compute gradient, numerically
        var aHi   = MatrixD.eye (x.dim, x.dim)                    // approximate Hessian inverse
                                                                  //   start with identity matrix
        var it = 1                                                // iteration counter
        cfor (it <= MAX_IT && df_x.norm > EPS, it += 1) {
            debug ("solve", s"it = $it: f($x) = $f_x, df_x = $df_x, aHi = $aHi")

            val d = if gradDesc then df_x                         // direction = gradient
                    else aHi * df_x                               // use approximate Hessian inverse (aHi)

            val s = if useLS then d * -wls.lsearch (x, -d)._1     // step vector using line sesrch
                    else d * -α                                   // step vector using learning rate

            x        += s                                         // new point: add step
            val df_x_ = df_x                                      // save previous gradient
            df_x      = ∇ (f)(x)                                  // compute new gradient, numerically
            aHi += aHi_inc (aHi, s, df_x - df_x_)                 // update approximate Hessian inverse (aHi)
            f_x  = f(x)                                           // functional value
        } // cfor

        println (s"optimal solution x= $x, f = ${f(x)}")
        (f_x, x)
    end solve

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for an optima by finding a local optima close to the starting point/guess 'x0'.
     *  This version uses explicit functions for the gradient (partials derivatives)
     *  @param x0    the starting point/guess
     *  @param grad  the gradient as explicit functions for partials
     *  @param α     the current learning rate
     */
    def solve2 (x0: VectorD, grad: FunctionV2V, α: Double = eta): FuncVec =
        val wls   = new WolfeLS2 (f, grad)                        // Wolfe Line Search
        var x     = x0                                            // current point
        var f_x   = f(x)                                          // function value at x
        var df_x  = grad (x)                                      // compute gradient by function evaluation
        var aHi   = MatrixD.eye (x.dim, x.dim)                    // approximate Hessian inverse
                                                                  //   start with identity matrix
        var it = 1                                                // iteration counter
        cfor (it <= MAX_IT && df_x.norm > EPS, it += 1) {
            debug ("solve", s"it = $it: f($x) = $f_x, df_x = $df_x, aHi = $aHi")

            val d = if gradDesc then df_x                         // direction = gradient
                    else aHi * df_x                               // use approximate Hessian inverse (aHi)

            val s = if useLS then d * -wls.lsearch (x, -d)._1     // step vector using line sesrch
                    else d * -α                                   // step vector using learning rate

            x        += s                                         // update new x
            val df_x_ = df_x                                      // save previous gradient
            df_x      = grad (x)                                  // compute new gradient by function evaluation
            aHi += aHi_inc (aHi, s, df_x - df_x_)                 // update approximate Hessian inverse (aHi)
            f_x  = f(x)                                           // functional value
        } // cfor

        println (s"optimal solution x= $x, f = ${f(x)}")
        (f_x, x)
    end solve2

end BFGS_NoLS


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bFGS_NoLSTest` main function is used to test the `BFGS_NoLS` class.
 *  This test numerically approximates the derivatives to find minima.
 *      f(x) = (x_0 - 3)^2 + (x_1 - 4)^2 + 1
 *  > runMain scalation.optimization.bFGS_NoLSTest
 */
@main def bFGS_NoLSTest (): Unit =

    val eta = 0.5                                           // learning rate (may need adjustment)
    val n   = 2                                             // dimension of the search space
    val x0  = new VectorD (n)                               // starting point (0, 0)

    banner ("Minimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f (x: VectorD): Double = (x(0) - 3.0)~^2 + (x(1) - 4.0)~^2 + 1.0

    val optimizer = new BFGS_NoLS (f)                       // use learning rate - needs a good eta
//  val optimizer = new BFGS_NoLS (f, true)                 // use Line Search
    val opt = optimizer.solve (x0)
    println (s"][ optimal solution (f(x), x) = $opt")

end bFGS_NoLSTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bFGS_NoLSTest2` main function is used to test the `BFGS_NoLS` class.
 *  This tests use of functions for partial derivatives to find minima.
 *      f(x) = (x_0 - 3)^2 + (x_1 - 4)^2 + 1
 *  > runMain scalation.optimization.bFGS_NoLSTest2
 */
@main def bFGS_NoLSTest2 (): Unit =

    val eta = 0.5                                           // learning rate (may need adjustment)
    val n   = 2                                             // dimension of the search space
    val x0  = new VectorD (n)                               // starting point (0, 0)

    banner ("Minimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f (x: VectorD): Double = (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1

    def grad (x: VectorD): VectorD = VectorD (2 * x(0) - 6, 2 * x(1) - 8)

    val optimizer = new BFGS_NoLS (f)                       // use learning rate - needs a good eta
//  val optimizer = new BFGS_NoLS (f, true)                 // use Line Search
    val opt = optimizer.solve2 (x0, grad)
    println (s"][ optimal solution (f(x), x) = $opt")

end bFGS_NoLSTest2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bFGS_NoLSTest3` main function is used to test the `BFGS_NoLS` class.
 *  This test uses the Rosenbrock function.
 *      f(x) = (1 - x_0)^2 + 100 (x_1 - x_0^2)^2
 *  > runMain scalation.optimization.bFGS_NoLSTest3
 */
@main def bFGS_NoLSTest3 (): Unit =

    val eta = 0.5                                           // learning rate (may need adjustment)
    val n   = 2                                             // dimension of the search space
    val x0  = new VectorD (n)                               // starting point (0, 0)

    banner ("Minimize: (1 - x_0)^2 + 100 (x_1 - x_0^2)^2")
    def f (x: VectorD): Double = (1.0 - x(0))~^2 + 100.0 * (x(1) - x(0)~^2)~^2

    def grad (x: VectorD): VectorD = VectorD (-2.0 * (1 - x(0)) - 400.0 * x(0) * (x(1) - x(0)~^2),
                                              200.0 * (x(1) - x(0)~^2))

    val optimizer = new BFGS_NoLS (f)                       // use learning rate - needs a good eta
//  val optimizer = new BFGS_NoLS (f, true)                 // use Line Search
//  val opt = optimizer.solve (x0, eta)                     // use numerical partials
    val opt = optimizer.solve2 (x0, grad, eta)              // use functions for partials
    println (s"][ optimal solution (f(x), x) = $opt")

end bFGS_NoLSTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bFGS_NoLSTest4` main function is used to test the `BFGS_NoLS` class on f(x):
 *      f(x) = 1/x(0) + x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1
 *  > runMain scalation.optimization.bFGS_NoLSTest4
 */
@main def bFGS_NoLSTest4 (): Unit =

    val eta = 0.5                                           // learning rate (may need adjustment)
    val n   = 2                                             // dimension of the search space
    val x0  = VectorD (0.1, 0.0)                            // starting point (.1, 0)

    banner ("Minimize: 1/x(0) + x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f (x: VectorD): Double = 1/x(0) + x(0)~^4 + (x(0) - 3.0)~^2 + (x(1) - 4.0)~^2 + 1.0

    def grad (x: VectorD): VectorD = VectorD (-(x(0)~^(-2)) + 4.0 * x(0)~^3 + 2 * x(0) - 6, 2 * x(1) - 8)

//  val optimizer = new BFGS_NoLS (f)                       // use learning rate - needs a good eta
    val optimizer = new BFGS_NoLS (f, true)                 // use Line Search
//  var opt = optimizer.solve (x0, eta)                     // use numerical partials
    var opt = optimizer.solve2 (x0, grad, eta)              // use functions for partials
    println (s"][ optimal solution (f(x), x) = $opt")

//  opt = optimizer.resolve (n)                             // try multiple starting points
//  println (s"][ optimal solution (f(x), x) = $opt")

end bFGS_NoLSTest4

