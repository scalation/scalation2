
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Thu Jun 29 20:43:44 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Wolfe Conditions for Line Search Algorithms
 *
 *  @see reference.wolfram.com/language/tutorial/UnconstrainedOptimizationStepControl.html
 *  @see pages.cs.wisc.edu/~ferris/cs730/chap3.pdf
 *  @see Limited memory BFGS for Nonsmooth Optimization
 */

package scalation
package optimization

import scala.math.abs

import scalation.calculus.Differential.∇
import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `WolfeConditions` class specifies conditions for inexact line search
 *  algorithms to acceptable/near minimal point along a given search direction p
 *  that exhibits
 *      (1) SDC: sufficient decrease (f(x) enough less that f(0)) and
 *      (2) CC:  the slope at x is less steep than the slope at 0.
 *  That is, the line search looks for a value for x satisfying the two Wolfe conditions.
 *  
 *     f(x) <= f(0) + c1 * f'(0) * x      Wolfe condition 1 (Armijo condition)
 *    f'(x) >= c2 * f'(0)                 Wolfe condition 2 (Weak version, more robust)
 *  |f'(x)| <= c2 * |f'(0)|               Wolfe condition 2 (Strong version)
 *
 *  Note: c1 and c2 defaults below intended for Quasi Newton methods such as BFGS or L-BFGS
 *  
 *  @param f   the objective/loss function to minimize (vector-to-scalar)
 *  @param g   the gradient of the objective/loss function (vector-to-vector)
 *  @param c1  constant for sufficient decrease (Wolfe condition 1: .0001 to .001)
 *  @param c2  constant for curvature/slope constraint (Wolfe condition 2: .9 to .8)
 */
class WolfeConditions (f: FunctionV2S, var g: FunctionV2V, c1: Double = 0.0001, c2: Double = 0.9):

    if g == null then g = (x: VectorD) => ∇ (f)(x)                  // no function for gradient => use numerical

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether Wolfe condition 1, the Sufficient Decrease Condition (SDC) is satisfied.
     *  @param fx   the functional value of the original point
     *  @param fy   the functional value of the new point y = x + p * a
     *  @param a    the displacement in the search direction
     *  @param gxp  the dot product of the gradient vector g(x) and the search vector p
     */
    inline def wolfe1 (fx: Double, fy: Double, a: Double, gxp: Double): Boolean =
        fy <= fx + c1 * a * gxp

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether weak Wolfe condition 2, the Curvature Condition (CC) is satisfied.
     *  @param p    the search direction vector
     *  @param gy   the gradient at new point y = x + p * a
     *  @param gxp  the dot product of the gradient vector g(x) and the search vector p
     */
    inline def wolfe2 (p: VectorD, gy: VectorD, gxp: Double): Boolean =
        (gy dot p) >= c2 * gxp

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether strong Wolfe condition 2, the Curvature Condition (CC) is satisfied.
     *  @param p    the search direction vector
     *  @param gy   the gradient at new point y = x + p * a
     *  @param gxp  the dot product of the gradient vector g(x) and the search vector p
     */
    inline def wolfe2s (p: VectorD, gy: VectorD, gxp: Double): Boolean =
        abs (gy dot p) <= c2 * abs (gxp)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an inexact Line Search (LS) on the function f to find an approximate
     *  local minima from the point x moving distance a (alpha) in the search direction
     *  p, which satisfies both Wolfe Conditions, returning the displacement a and the
     *  new point y = x + p * a.
     *  @param x     the current point
     *  @param fx    the functional value at x, f(x)
     *  @param p     the current search direction
     *  @param step  the initial step length
     */
    def lsearch (x: VectorD, fx: Double, p: VectorD, step: Double = 1.0): (Double, VectorD) = ???

end WolfeConditions


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `wolfeConditionsTest` main function is used to test the `WolfeConditions` class.
 *  > runMain scalation.optimization.wolfeConditionsTest
 */
@main def wolfeConditionsTest (): Unit =

    val aa = new VectorD (60)
    val ff = new VectorD (60)
    val ww = new VectorD (60)

    val x = VectorD (0.0, 0.0)                                      // zero vector, the origin
    val p = VectorD (1.0, 1.0)                                      // direction to search in

    banner ("Problem: (x_0 - 2)^2 + (x_1 - 3)^2 + 1")

    def f(x: VectorD): Double  = (x(0) - 2.0) * (x(0) - 2.0) + (x(1) - 3.0) * (x(1) - 3.0) + 1.0
    def g(x: VectorD): VectorD = VectorD (2.0 * (x(0) - 2.0), 2.0 * (x(1) - 3.0))

    val wls = new WolfeLS2 (f, g)
    val fx  = f(x)
    val gx  = g(x)
    val gxp = gx dot p
    println (s"x = $x, fx = $fx, gx = $gx")

    for i <- 0 until 60 do
        val a  = 0.1 * i
        val y  = x + p * a
        val fy = f(y)
        val gy = g(y)
        val wolf1 = wls.wolfe1 (fx, fy, a, gxp)
        val wolf2 = wls.wolfe2 (p, gy, gxp)
        println (s"a = $a, y = $y, fy = $fy, wolf1 = $wolf1, wolf2 = $wolf2")
        aa(i) = a
        ff(i) = fy
        ww(i) = 2.0 + is (wolf1) + 2.0 * is (wolf2)
    end for
    new Plot (aa, ff, ww, "fy vs. a")

end wolfeConditionsTest

