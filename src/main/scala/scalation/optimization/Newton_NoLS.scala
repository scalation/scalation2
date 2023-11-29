
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Jun  7 17:54:59 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Newton Method to Find Minima for Functions of Vectors (with no Linear Search)
 *
 *  @see web.stanford.edu/class/cme304/docs/newton-type-methods.pdf
 */

package scalation
package optimization

import scalation.calculus.Differential.{array2f, eval, Ј, ∇, Η}
import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Newton_NoLS` class is used to find optima for functions of vectors.
 *  The solve method finds local optima using the Newton method that deflects
 *  the gradient using the inverse Hessian.
 *
 *      min f(x)    where f: R^n -> R
 *
 *  @see `Newton` for one that uses a different line search.
 *  @param f      the vector to scalar function to find optima of
 *  @param useLS  whether to use Line Search (LS)
 */
class Newton_NoLS (f: FunctionV2S, useLS: Boolean = false)
      extends Minimize:

    private val debug    = debugf ("Newton_NoLS", true)           // debug function
    private val gradDesc = false                                  // true = Gradient Descent
                                                                  // false => Newton Method

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for an optima by finding a local optima close to the starting point/guess 'x0'.
     *  This version numerically approximates the first and second derivatives.
     *  @param x0  the starting point/guess
     *  @param α   the current learning rate
     */
    def solve (x0: VectorD, α: Double = eta): FuncVec =
        val wls   = new WolfeLS2 (f, null)                        // Wolfe Line Search
        val x     = x0                                            // current point
        var f_x   = f(x)                                          // function value at x
        var df_x  = VectorD.one (x.dim)                           // initial dummy value for gradient

        var it = 1                                                // iteration counter
        cfor (it <= MAX_IT && df_x.norm > EPS, it += 1) {
            df_x      = ∇ (f)(x)                                  // compute gradient, numerically
            val d2f_x = Η (f, x)                                  // compute Hessian, numerically
            debug ("solve", s"it = $it: f($x) = $f_x, df_x = $df_x, d2f_x = $d2f_x")

            val d = if gradDesc then df_x                         // direction = gradient
//                  else Fac_LU.inverse (d2f_x)() * df_x          // deflected via inverse Hessian
                    else Fac_LU.solve_ (d2f_x, df_x)              // deflected via factorized Hessian

            val s = if useLS then d * -wls.lsearch (x, -d)._1     // step vector using line sesrch
                    else d * -α                                   // step vector using learning rate

            x  += s                                               // new point: add step
            f_x = f(x)                                            // functional value
        } // cfor

        println (s"optimal solution x = $x, f = ${f(x)}")
        (f_x, x)
    end solve

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for an optima by finding a local optima close to the starting point/guess 'x0'.
     *  This version uses explicit functions for the gradient (partials derivatives)
     *  @param x0    the starting point/guess
     *  @param grd   the gradient as explicit functions for partials (in array form)
     *  @param α     the current learning rate
     */
    def solve2 (x0: VectorD, grd: Array [FunctionV2S], α: Double = eta): FuncVec = 
        val grad  = array2f (grd)                                 // make function V2V
        val wls   = new WolfeLS2 (f, grad)                        // Wolfe Line Search
        val x     = x0                                            // current point
        var f_x   = f(x)                                          // function value at x
        var df_x  = VectorD.one (x.dim)                           // initial dummy value for gradient

        var it = 1                                                // iteration counter
        cfor (it <= MAX_IT && df_x.norm > EPS, it += 1) {
            df_x      = eval (grd, x)                             // compute gradient by functional evaluation
//          val d2f_x = Η (f, x)                                  // compute Hessian, numerically
            val d2f_x = Ј (grd, x)                                // compute Hessian, numerically as Jacobian of grad
            debug ("solve", s"it = $it: f($x) = $f_x, df_x = $df_x, d2f_x = $d2f_x")

            val d = if gradDesc then df_x                         // direction = gradient
//                  else Fac_LU.inverse (d2f_x)() * df_x          // deflected via inverse Hessian
                    else Fac_LU.solve_ (d2f_x, df_x)              // deflected via factorized Hessian

            val s = if useLS then d * -wls.lsearch (x, -d)._1     // step vector using line sesrch
                    else d * -α                                   // step vector using learning rate

            x  += s                                               // new point: add step
            f_x = f(x)                                            // functional value
        } // cfor

        println (s"optimal solution x = $x, f = ${f(x)}")
        (f_x, x)
    end solve2

end Newton_NoLS


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `newton_NoLSTest` main function is used to test the `Newton_NoLS` class.
 *  This test numerically approximates the first derivative (gradient) and the second
 *  derivative (Hessian) to find minima.
 *  > runMain scalation.optimization.newton_NoLSTest
 */
@main def newton_NoLSTest (): Unit =

    val n  = 2                                             // dimension of the search space
    val x0 = new VectorD (n)

    banner ("Minimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f (x: VectorD): Double = (x(0) - 3.0)~^2 + (x(1) - 4.0)~^2 + 1.0

    val optimizer = new Newton_NoLS (f)
    val opt = optimizer.solve (x0)
    println (s"][ optimal solution (f(x), x) = $opt")

end newton_NoLSTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `newton_NoLSTest2` main function is used to test the `Newton_NoLS` class.
 *  This test functionally evaluates the first derivative (gradient) and uses the
 *  Jacobian to numerically compute the second derivative (Hessian) from the gradient
 *  to find minima.
 *  > runMain scalation.optimization.newton_NoLSTest2
 */
@main def newton_NoLSTest2 (): Unit =

    val n  = 2                                             // dimension of the search space
    val x0 = new VectorD (n)

    banner ("Minimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f (x: VectorD): Double = (x(0) - 3.0)~^2 + (x(1) - 4.0)~^2 + 1.0

    val grd = Array [FunctionV2S] ((x: VectorD) => 2 * x(0) - 6,
                                   (x: VectorD) => 2 * x(1) - 8)

    val optimizer = new Newton_NoLS (f)
    val opt = optimizer.solve2 (x0, grd)
    println (s"][ optimal solution (f(x), x) = $opt")

end newton_NoLSTest2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `newton_NoLSTest3` main function is used to test the `Newton_NoLS` class.
 *  This test uses the Rosenbrock function.
 *  > runMain scalation.optimization.newton_NoLSTest3
 */
@main def newton_NoLSTest3 (): Unit =

    val eta = 0.006
    val n   = 2                                            // dimension of the search space
    val x0  = new VectorD (n)

    banner ("Minimize: (1 - x_0)^2 + 100 (x_1 - x_0^2)^2")
    def f (x: VectorD): Double = (1.0 - x(0))~^2 + 100.0 * (x(1) - x(0)~^2)~^2

    val grd = Array [FunctionV2S] ((x: VectorD) => -2.0 * (1 - x(0)) - 400.0 * x(0) * (x(1) - x(0)~^2),
                                   (x: VectorD) => 200.0 * (x(1) - x(0)~^2))

    val optimizer = new Newton_NoLS (f)                    // use learning rate - needs a good eta
//  val optimizer = new Newton_NoLS (f, true)              // use Line Search
    val opt = optimizer.solve2 (x0, grd, eta)
    println (s"][ optimal solution (f(x), x) = $opt")

end newton_NoLSTest3

