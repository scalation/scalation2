
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Aug 24 19:53:22 EDT 2011
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Gradient Descent for Non-Linear Optimization
 */

package scalation
package optimization

import scalation.calculus.Differential.∇
import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GradientDescent` class solves unconstrained Non-Linear Programming (NLP)
 *  problems using the Gradient Descent algorithm.  Given a function f and a
 *  starting point x0, the algorithm computes the gradient and takes steps in
 *  the opposite direction.  The algorithm iterates until it converges.  The class
 *  assumes that partial derivative functions are not available unless explicitly
 *  given via the setDerivatives method.
 *
 *  dir_k = -gradient (x)
 *
 *  minimize    f(x)
 *
 *  @param f        the vector-to-scalar objective function
 *  @param exactLS  whether to use exact (e.g., `GoldenLS`)
 *                            or inexact (e.g., `WolfeLS`) Line Search
 */
class GradientDescent (f: FunctionV2S, exactLS: Boolean = true)
      extends Minimizer:

    private val debug  = debugf ("GradientDescent", true)     // debug function

    private var gr: FunctionV2V = null                        // gradient (vector of partial derivatives)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the partial derivative functions.  If these functions are available,
     *  they are more efficient and more accurate than estimating the values
     *  using difference quotients (the default approach).
     *  @param partials  the vector of partial derivatives
     */
    def setDerivatives (partials: FunctionV2V): Unit = gr = partials

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an exact `GoldenSectionLS` or inexact `WolfeLS` line search.
     *  Search in direction 'dir', returning the distance 'z' to move in that direction.
     *  @param x     the current point
     *  @param dir   the direction to move in
     *  @param step  the initial step size
     */
    def lineSearch (x: VectorD, dir: VectorD, step: Double = STEP): Double =
        def f_1D (z: Double): Double = f(x + dir * z)         // create a 1D function
        val ls = if exactLS then new GoldenSectionLS (f_1D)   // Golden Section line search
                 else new WolfeLS (f_1D, .0001, .1)           // Wolfe line search (c1 = .0001, c2 = .1)
        ls.search (step)                                      // perform a line search
    end lineSearch

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the Non-Linear Programming (NLP) problem using the Gradient Descent
     *  algorithm.
     *  @param x0     the starting point
     *  @param step   the initial step size
     *  @param toler  the tolerance
     */
    def solve (x0: VectorD, step: Double = STEP, toler: Double = EPSILON): FuncVec =
        var x    = x0                                         // current point
        var fx   = f(x)                                       // objective function at current point
        var y    = VectorD.nullv                              // next point
        var fy   = 0.0                                        // objective function at next point
        var dir  = if gr != null then - gr(x)                 // initial direction is -gradient: use partials
                   else - ∇(f)(x)                             //                                 estimate gradient
        var dist = 1.0                                        // distance between current and next point
        var down = true                                       // moving down flag

        for k <- 1 to MAX_IT if down && dist > toler && dir.normSq > toler do
            banner (s"solve iteration $k: fx = $fx, x = $x")

            y   = x + dir * lineSearch (x, dir, step)         // determine the next point
            fy = f(y)                                         // objective function value for next point
            dir = if gr != null then - gr(y)                  // next search direction: use partials
                  else - ∇(f)(y)                              //                        estimate gradient

            debug ("solve", s"k = $k, y = $y, fy = $fy, dir = $dir")

            dist = (x - y).normSq                             // calc the distance between current and next point
            down = fy < fx                                    // still moving down?
            if down then { x = y; fx = fy }                   // make the next point, the current point
        end for
        (fx, x)                                               // return functional vale and the current point
    end solve

end GradientDescent


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `gradientDescentTest` main function is used to test the `GradientDescent` class.
 *      f(x) = (x_0 - 3)^2 + (x_1 - 4)^2 + 1
 *  > runMain scalation.optimization.gradientDescentTest
 */
@main def gradientDescentTest (): Unit =

    val x0 = VectorD (0.0, 0.0)                               // starting point

    banner ("Minimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1") 
    def f (x: VectorD): Double = (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1

    val optimizer = new GradientDescent (f)
    var opt = optimizer.solve (x0)
    println (s"][ optimal solution f(x), x) = $opt")

    banner ("Minimize (with given partials)") 
    def gr (x: VectorD): VectorD = VectorD (2 * x(0) - 6, 2 * x(1) - 8)

    optimizer.setDerivatives (gr)
    opt = optimizer.solve (x0)
    println (s"][ optimal solution f(x), x) = $opt")

end gradientDescentTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `gradientDescentTest2` main function is used to test the `GradientDescent` class.
 *      f(x) = x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1
 *  > runMain scalation.optimization.gradientDescentTest2
 */
@main def gradientDescentTest2 (): Unit =

    val n  = 2                                             // dimension of the search space
    val x0 = new VectorD (n)

    banner ("Minimize: x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f (x: VectorD): Double = x(0)~^4 + (x(0) - 3.0)~^2 + (x(1) - 4.0)~^2 + 1.0

    val optimizer = new GradientDescent (f)
    val opt = optimizer.solve (x0)
    println (s"][ optimal solution (f(x), x) = $opt")

end gradientDescentTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `gradientDescentTest3` main function is used to test the `GradientDescent` class.
 *      f(x) = 1/x(0) + x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1
 *  > runMain scalation.optimization.gradientDescentTest3
 */
@main def gradientDescentTest3 (): Unit =

    val n  = 2                                             // dimension of the search space
    val x0 = VectorD (0.1, 0.0)                            // starting location

    banner ("Minimize: 1/x(0) + x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f (x: VectorD): Double = 1/x(0) + x(0)~^4 + (x(0) - 3.0)~^2 + (x(1) - 4.0)~^2 + 1.0

    val optimizer = new GradientDescent (f)
    var opt = optimizer.solve (x0)
    println (s"][ optimal solution (f(x), x) = $opt")

    opt = optimizer.resolve (n)
    println (s"][ optimal solution (f(x), x) = $opt")

end gradientDescentTest3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `gradientDescentTest4` main function is used to test the `GradientDescent` class.
 *       f(x) = x_0/4 + 5x_0^2 + x_0^4 - 9x_0^2 x_1 + 3x_1^2 + 2x_1^4
 *  @see math.fullerton.edu/mathews/n2003/gradientsearch/GradientSearchMod/Links/GradientSearchMod_lnk_5.html
 *  > runMain scalation.optimization.gradientDescentTest4
 */
@main def gradientDescentTest4 (): Unit =

    val x0 = VectorD (0.0, 0.0)                     // starting point

    banner ("Minimize: x_0/4 + 5x_0^2 + x_0^4 - 9x_0^2 x_1 + 3x_1^2 + 2x_1^4")
    def f (x: VectorD): Double = x(0)/4 + 5*x(0)~^2 + x(0)~^4 - 9*x(0)~^2*x(1) + 3*x(1)~^2 + 2*x(1)~^4

    val optimizer = new GradientDescent (f)
    val opt = optimizer.solve (x0)
    println (s"][ optimal solution f(x), x) = $opt")

end gradientDescentTest4

