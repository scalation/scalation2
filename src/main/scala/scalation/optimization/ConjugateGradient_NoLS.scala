
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Sep 30 13:37:32 EDT 2011
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Polak-Ribiere Conjugate Gradient (PR-CG) with No Line Search
 *
 *  @see http://www.neos-guide.org/NEOS/index.php/Nonlinear_Conjugate_Gradient_Method
 */

package scalation
package optimization

import scala.math.max

import scalation.calculus.Differential.∇
import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ConjugateGradient_NoLS` class implements the Polak-Ribiere Conjugate Gradient (PR-CG)
 *  Algorithm for solving Non-Linear  Programming (NLP) problems.  PR-CG determines
 *  a search direction as a weighted combination of the steepest descent direction
 *  (-gradient) and the previous direction.  The weighting is set by the beta function,
 *  which for this implementation used the Polak-Ribiere technique.
 * 
 *      dir_k = - grad (x) + beta * dir_k-1 
 *
 *      min f(x)    where f: R^n -> R
 *
 *  This version does not use a line search algorithm (_NoLS)
 *  @see `ConjugateGradient` for one that uses line search.
 *  @param f the objective function to be minimized
 */
class ConjugateGradient_NoLS (f: FunctionV2S)
      extends Minimize:

    private val debug  = debugf ("ConjugateGradient_NoLS", true)   // debug function

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the beta function using the Polak-Ribiere (PR) technique.  The
     *  function determines how much of the prior direction is mixed in with -gradient.
     *  @param sd1  the search direction at the previous point
     *  @param sd2  the search direction at the current point
     */
    private inline def beta (sd1: VectorD, sd2: VectorD): Double =
        max (0.0, (sd2 dot (sd2 - sd1)) / (sd1.normSq + EPS))   // PR-CG (Polak-Ribiere)
    end beta

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the Non-Linear Programming (NLP) problem using the PR-CG algorithm.
     *  To use explicit functions for gradient, replace ∇(f, x) with gr(x).
     *  @param x0  the starting point/guess
     *  @param α   the current learning rate
     */
    def solve (x0: VectorD, α: Double = eta): FuncVec =
        var x    = x0                                         // current point
        var f_x  = f(x)                                       // objective function at current point
        val x_   = new VectorD (x.dim)                        // next point
        var f_x_ = 0.0                                        // objective function at next point
        var dir  = - ∇ (f)(x)                                 // initial direction is -gradient
        var dir0 = VectorD.nullv                              // keep the previous direction
        var dist = 1.0                                        // distance between current and next point
        var down = true                                       // moving down flag

        var it = 1
        cfor (it < MAX_IT && down && dist > EPS && dir.normSq > EPS, it += 1) {
            debug ("solve", s"iteration $it: f(x) = $f_x, x = $x")

            val s = dir * α                                   // compute step vector
            x_   += s                                         // determine the next point
            f_x_  = f(x_)                                     // objective function value for next point
            dir0  = dir                                       // save the current direction
            dir   = - ∇(f)(x_)                                // next search direction using Gradient Descent
            if it > 1 then dir += dir0 * beta (dir0, dir)     // modify search direction using PR-CG

            debug ("solve", s"it = $it, x_ = $x_, f_x_ = $f_x_, dir = $dir")

            dist = (x - x_).normSq                            // calc the distance between current and next point
            down = f_x_ < f_x                                 // still moving down?
            if down then { x = x_; f_x = f_x_ }               // make the next point, the current point
        } // cfor
        (f_x, x)                                              // return functional value and current point
    end solve

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for an optima by finding a local optima close to the starting point/guess 'x0'.
     *  This version uses explicit functions for the gradient (partials derivatives)
     *  @param x0    the starting point/guess
     *  @param grad  the gradient as explicit functions for partials
     *  @param α     the current learning rate
     */
    def solve2 (x0: VectorD, grad: FunctionV2V, α: Double = eta): FuncVec = ???   // FIX - code it

end ConjugateGradient_NoLS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `conjugateGradient_NoLSTest` main function is used to test the `ConjugateGradient_NoLS
 *  class.
 *      f(x) = (x_0 - 3)^2 + (x_1 - 4)^2 + 1
 *  > runMain scalation.optimization.conjugateGradient_NoLSTest
 */
@main def conjugateGradient_NoLSTest (): Unit =

    val n  = 2
    val x0 = new VectorD (n)

    banner ("Minimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")

    def f (x: VectorD): Double = (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1.0

    val optimizer = new ConjugateGradient_NoLS (f)
    val opt = optimizer.solve (x0)
    println (s"][ optimal solution (f(x), x) = $opt")

end conjugateGradient_NoLSTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `conjugateGradient_NoLSTest2` main function is used to test the `ConjugateGradient_NoLS`
 *  class.
 *      f(x) = x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1
 *  > runMain scalation.optimization.conjugateGradient_NoLSTest2
 */
@main def conjugateGradient_NoLSTest2 (): Unit =

    val n  = 2
    val x0 = new VectorD (n)

    banner ("Minimize: x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")

    def f (x: VectorD): Double = x(0)~^4 + (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1.0

    val optimizer = new ConjugateGradient_NoLS (f)
    val opt = optimizer.solve (x0)
    println (s"][ optimal solution (f(x), x) = $opt")

end conjugateGradient_NoLSTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `conjugateGradient_NoLSTest3` main function is used to test the `ConjugateGradient_NoLS`
 *  class.
 *      f(x) = 1/x_0 + x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1
 *  > runMain scalation.optimization.conjugateGradient_NoLSTest3
 */
@main def conjugateGradient_NoLSTest3 (): Unit =

    val x0 = VectorD (0.1, 0)

    banner ("Minimize: 1/x_0 + x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")

    def f (x: VectorD): Double = 1/x(0) + x(0)~^4 + (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1.0

    val optimizer = new ConjugateGradient_NoLS (f)
    val opt = optimizer.solve (x0)
    println (s"][ solve: optimal solution (f(x), x) = $opt")

end conjugateGradient_NoLSTest3

