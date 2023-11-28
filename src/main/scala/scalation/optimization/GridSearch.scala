
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Jul 28 13:32:52 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Grid Search Optimizer
 */

package scalation
package optimization

import scala.runtime.ScalaRunTime.stringOf

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GridSearch` companion object specifies default minimums and maximums for
 *  the grid's coordinate axes.
 */
object GridSearch:

    def minAxF (n: Int): VectorD = VectorD.fill (n)(-10.0)
    def maxAxF (n: Int): VectorD = VectorD.fill (n)(10.0)

end GridSearch

import GridSearch._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GridSearch` class performs grid search over an n-dimensional space
 *  to find a minimal objective value for f(x).
 *  @param f       the objective function to be minimized
 *  @param n       the number of dimensions in search space
 *  @param g       the constraint function to be satisfied, if any
 *  @param nSteps  the number of steps an axes is divided into to from the grid
 */
class GridSearch (f: FunctionV2S, n: Int, g: FunctionV2S = null, nSteps: Int = 200)
      extends Minimizer:

    private val debug = debugf ("GridSearch", true)               // debug function
    private val axes  = Array.ofDim [VectorD] (n)                 // n axes of the search space
    private val x     = new VectorD (n)                           // point in search space, initially zero
    private var best  = (f(x), x)                                 // location zero solution

    if g != null then println ("constraint function g is passed in")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create the n axes for the grid.  Must be called before the solve method.
     *  @param minAx  the minimum value along each axes (e.g., [-5.0, 0.0]
     *  @param MaxAx  the maximum value along each axes (e.g., [10.0, 5.0]
     */
    def setAxes (minAx: VectorD = minAxF (n), maxAx: VectorD = maxAxF (n)): Unit =
        for k <- axes.indices do
            val dist = maxAx(k) - minAx(k)
            axes(k) = VectorD.range (0, nSteps+1) * dist / nSteps + minAx(k)
        end for
        debug ("setAxes", s"axes = ${stringOf (axes)}")
    end setAxes

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The lineserarch method is not used.
     */
    def lineSearch (x: VectorD, dir: VectorD, step: Double = STEP): Double = ???

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursive grid search method.
     *  @param k  the current dimension to explore
     */
    private def gsearch (k: Int): Unit =
        for i <- 0 to nSteps do
            x(k)   = axes(k)(i)                                   // set the points k-th coordinate
            val fx = f(x)                                         // compute its functional value
            if fx < best._1 then best = (fx, x.copy)
            if k < n-1 then gsearch (k+1)                         // explore the next dimension (recursive call)
        end for
    end gsearch

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the Non-Linear Programming (NLP) problem using grid search.
     *  Return the optimal point/vector x and its objective function value.
     *  @param x0     the starting point
     *  @param step   the initial step size
     *  @param toler  the tolerance
     */
    def solve (x0: VectorD = null, step: Double = STEP, toler: Double = EPSILON): FuncVec =
        debug ("solve", s"zero solution (f(x), x) = $best")
        gsearch (0)
        debug ("solve", s"grid solution (f(x), x) = $best")
        best
    end solve
        
end GridSearch


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `gridSearchTest` main function is used to test the `GridSearch` class on f(x):
 *      f(x) = (x_0 - 3)^2 + (x_1 - 4)^2 + 1
 *  > runMain scalation.optimization.gridSearchTest
 */
@main def gridSearchTest (): Unit =

    val n  = 2                                             // dimension of the search space

    banner ("Minimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f (x: VectorD): Double = (x(0) - 3.0)~^2 + (x(1) - 4.0)~^2 + 1.0

    val optimizer = new GridSearch (f, n)
    optimizer.setAxes ()
    val opt = optimizer.solve ()
    println (s"][ optimal solution (f(x), x) = $opt")

end gridSearchTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `gridSearchTest2` main function is used to test the `GridSearch` class on f(x):
 *      f(x) = x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1
 *  > runMain scalation.optimization.gridSearchTest2
 */
@main def gridSearchTest2 (): Unit =

    val n  = 2                                             // dimension of the search space

    banner ("Minimize: x_0^4 + (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f (x: VectorD): Double = x(0)~^4 + (x(0) - 3.0)~^2 + (x(1) - 4.0)~^2 + 1.0

    val optimizer = new GridSearch (f, n)
    optimizer.setAxes ()
    val opt = optimizer.solve ()
    println (s"][ optimal solution (f(x), x) = $opt")

end gridSearchTest2

