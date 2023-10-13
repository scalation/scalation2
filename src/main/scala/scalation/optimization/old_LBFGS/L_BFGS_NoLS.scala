
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Jun  7 17:54:59 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Limited Memory BFGS Method to Find Minima for Functions of Vectors
 *
 *  @see web.stanford.edu/class/cme304/docs/newton-type-methods.pdf
 *  @see `L_BFGS` for similar code that uses line-search
 */

package scalation
package optimization

import scala.math.max

import scalation.calculus.Differential.∇
import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `L_BFGS_NoLS` class is used to find optima for functions of vectors.
 *  The solve method finds local optima using a Quasi Newton method,
 *  the Limited Memory BFGS Method that keeps track of the most recent m
 *  changes in x-positions and gradients.  The `Ring` class is used to
 *  store the most recent m vectors.
 * 
 *      min f(x)    where f: R^n -> R
 * 
 *  @see `L_BFGS` for one that uses a different line search.
 *  @param f      the vector to scalar function to find optima of
 *  @param m      the memory size or number of historical s and y vectors to maintain
 *  @param n      the dimensionality of the vectors
 *  @param useLS  whether to use Line Search (LS)
 */
class L_BFGS_NoLS (f: FunctionV2S, m: Int, n: Int, useLS: Boolean = false)
      extends Minimize:

    private val debug = debugf ("L_BFGS_NoLS", true)              // debug function
    private val zero  = new VectorD (n)
    private val s     = new Ring [VectorD] (m, zero)              // history of x-position changes
    private val y     = new Ring [VectorD] (m, zero)              // history of gradient changes
    private val p     = new Ring [Double] (m, 0.0)                // rho (p) vector for findDir
    private val a     = new Ring [Double] (m, 0.0)                // alpha (a) vector for findDir

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the deflected gradient by passing in the current gradient and using
     *  the last m steps or changes in x-position s and change in gradient y vectors.
     *  @see https://en.wikipedia.org/wiki/Limited-memory_BFGS
     *  @param g  the current gradient
     *  @param k  the k-th iteration 
     */
    def findDir (g: VectorD, k: Int): VectorD =
        println (s"k = $k, \t s = $s, \n\t\t y = $y")
        var q = g                                                 // start with current gradient 
        for i <- k-1 until max (-1, k-m) by -1 do
            a(i) = (s(i) dot q) * p(i)
            q -= y(i) * a(i)
        val ga = (s(k-1) dot y(k-1)) / y(k-1).normSq              // gamma
        var z = q * ga
        for i <- max (0, k-m) until k do
            val b = (y(i) dot z) * p(i)
            z += s(i) * (a(i) - b)
        z                                                         // return direction = deflected gradient 
    end findDir 

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

        var it = 0                                                // iteration counter
        cfor (it < MAX_IT && df_x.norm > EPS, it += 1) {
            debug ("solve", s"it = $it: f($x) = $f_x, df_x = $df_x")
            
            val d = if it == 0 then df_x                          // direction = gradient
                    else findDir (df_x, it)                       // find deflected gradient

            val s_ = if useLS then d * -wls.lsearch (x, -d)._1    // step vector using line sesrch
                     else d * -α                                  // step vector using learning rate

            x        += s_                                        // new point: add step
            val df_x_ = df_x                                      // save previous gradient
            df_x      = ∇ (f)(x)                                  // compute new gradient, numerically
            val y_    = df_x - df_x_                              // difference in gradients
            f_x       = f(x)                                      // functional value
            s.add (s_); y.add (y_)                                // add s_ and y_ to their rings (keeps latest m)
        } // cfor

        println (s"optimal solution x = $x, f = ${f(x)}")
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

        var it = 0                                                // iteration counter
        cfor (it < MAX_IT && df_x.norm > EPS, it += 1) {
            debug ("solve", s"it = $it: f($x) = $f_x, df_x = $df_x")

            val d = if it == 0 then df_x                          // direction = gradient
                    else findDir (df_x, it)                       // find deflected gradient

            val s_ = if useLS then d * -wls.lsearch (x, -d)._1    // step vector using line sesrch
                     else d * -α                                  // step vector using learning rate

            x        += s_                                        // new point: add step
            val df_x_ = df_x                                      // save previous gradient
            df_x      = grad (x)                                  // compute new gradient by function evaluation
            val y_    = df_x - df_x_                              // difference in gradients
            f_x       = f(x)                                      // functional value
            s.add (s_); y.add (y_)                                // add s_ and y_ to their rings (keeps latest m)
        } // cfor

        println (s"optimal solution x = $x, f = ${f(x)}")
        (f_x, x)
    end solve2

end L_BFGS_NoLS


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `l_BFGS_NoLSTest` main function is used to test the `L_BFGS_NoLS` class.
 *  This test numerically approximates the derivatives to find minima.
 *  > runMain scalation.optimization.l_BFGS_NoLSTest
 */
@main def l_BFGS_NoLSTest (): Unit =

    val m  = 4                                             // size of memory
    val n  = 2                                             // dimension of the search space
    val x0 = new VectorD (n)

    banner ("Minimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f (x: VectorD): Double = (x(0) - 3.0)~^2 + (x(1) - 4.0)~^2 + 1.0

    val optimizer = new L_BFGS_NoLS (f, m, n)
    val opt = optimizer.solve (x0)
    println (s"][ optimal solution (f(x), x) = $opt")

end l_BFGS_NoLSTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `l_BFGS_NoLSTest2` main function is used to test the `L_BFGS_NoLS` class.
 *  This tests use functions for partial derivatives to find minima.
 *  > runMain scalation.optimization.l_BFGS_NoLSTest2
 */
@main def l_BFGS_NoLSTest2 (): Unit =

    val m  = 4                                             // size of memory
    val n  = 2                                             // dimension of the search space
    val x0 = new VectorD (n)

    banner ("Minimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f (x: VectorD): Double = (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1

    def grad (x: VectorD): VectorD = VectorD (2 * x(0) - 6, 2 * x(1) - 8)

    val optimizer = new L_BFGS_NoLS (f, m, n)
    val opt = optimizer.solve2 (x0, grad)
    println (s"][ optimal solution (f(x), x) = $opt")

end l_BFGS_NoLSTest2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `l_BFGS_NoLSTest3` main function is used to test the `L_BFGS_NoLS` class.
 *  This test uses the Rosenbrock function.
 *  > runMain scalation.optimization.l_BFGS_NoLSTest3
 */
@main def l_BFGS_NoLSTest3 (): Unit =

    val eta = 0.5
    val m   = 4                                            // size of memory
    val n   = 2                                            // dimension of the search space
    val x0  = new VectorD (n)

    banner ("Minimize: (1 - x_0)^2 + 100 (x_1 - x_0^2)^2")
    def f (x: VectorD): Double = (1.0 - x(0))~^2 + 100.0 * (x(1) - x(0)~^2)~^2

    def grad (x: VectorD): VectorD = VectorD (-2.0 * (1 - x(0)) - 400.0 * x(0) * (x(1) - x(0)~^2),
                                              200.0 * (x(1) - x(0)~^2))

//  val optimizer = new L_BFGS_NoLS (f, m, n)              // use learning rate - needs a good eta
    val optimizer = new L_BFGS_NoLS (f, m, n, true)        // use Line Search
    val opt = optimizer.solve2 (x0, grad, eta)
    println (s"][ optimal solution (f(x), x) = $opt")

end l_BFGS_NoLSTest3

