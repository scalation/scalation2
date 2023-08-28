
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Jun 19 01:30:58 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Modified Wolfe Line Search Optimizer
 *
 *  @see reference.wolfram.com/language/tutorial/UnconstrainedOptimizationStepControl.html
 *  @see pages.cs.wisc.edu/~ferris/cs730/chap3.pdf
 *  @see Limited Memory BFGS for Nonsmooth Optimization
 */

package scalation
package optimization

//import scalation.calculus.Differential.Ⅾ
import scalation.calculus.Differential.∇
import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `WolfeLS2` class performs an inexact line search on f to find (1) a point x
 *  that exhibits
 *      (1) SDC: sufficient decrease (f(x) enough less that f(0)) and
 *      (2) CC:  the slope at x is less steep than the slope at 0.
 *  That is, the line search looks for a value for x satisfying the two Wolfe conditions.
 *  
 *     f(x) <= f(0) + c1 * f'(0) * x      Wolfe condition 1 (Armijo condition)
 *  |f'(x)| <= |c2 * f'(0)|               Wolfe condition 2 (Strong version)
 *    f'(x) >= c2 * f'(0)                 Wolfe condition 2 (Weak version, more robust)
 *
 *  The it uses bisection (or interpolative search) to find an approximate local minimal point.
 *  Currently, the strong version is not supported.
 *  Note: c1 and c2 defaults below intended for Quasi Newton methods such as BFGS or L-BFGS
 *  
 *  @param f   the objective/loss function to minimize
 *  @param g   the gradient of the objective/loss function
 *  @param c1  constant for sufficient decrease (Wolfe condition 1: .0001 to .001)
 *  @param c2  constant for curvature/slope constraint (Wolfe condition 2: .9 to .8)
 */
class WolfeLS2 (f: FunctionV2S, var g: FunctionV2V, c1: Double = 0.0001, c2: Double = 0.9):

    private val debug   = debugf ("WolfeLS2", true)                 // debug function
    private val POS_INF = Double.PositiveInfinity                   // Positive Infinity
    private val MAX_IT  = 30                                        // maximum number of iterations

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
    /** Return whether Wolfe condition 2, the Curvature Condition (CC) is satisfied.
     *  @param p    the search direction vector
     *  @param gy   the gradient at new point y = x + p * a
     *  @param gxp  the dot product of the gradient vector g(x) and the search vector p
     */
    inline def wolfe2 (p: VectorD, gy: VectorD, gxp: Double): Boolean =
        (gy dot p) >= c2 * gxp

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an inexact Line Search (LS) on the function f to find an approximate local
     *  minima from the point x moving distance a in the search direction p in two steps:
     *  (1) The START point is x (at a = 0), while the END point x_max (at a = a_max) is
     *  a distant point that satisfies the Wolfe Conditions 1 and 2.  Begin with a = 1
     *  and decrease it (e.g., * 0.5) until both conditions are satisfied to get a_max.
     *  (2) Perform bisection (or interpolative) search over the interval for a = (0, a_max)
     *  and return its solutions.
     *  @param x  the current point
     *  @param p  the current search direction
     */
    def lsearch (x: VectorD, p: VectorD, step: Double = 1.0): (Double, VectorD) =
        val (fx, gx) = (f(x), g(x))                                 // function and gradient at x: f(x), g(x)
        val gxp = gx dot p                                          // gradient dot search direction
        debug ("lsearch", s"x = $x, fx = $fx, gx = $gx, p = $p, gxp = $gxp")
        var a  = step                                               // start for a = step size

        //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Once a distant endpoint is found that satisfies the Wolfe conditions is
         *  found, bisection search will find a near optimal solution along the line.
         */
        def bisection (a1: Double, a2: Double): Double =
            val a = (a1 + a2) / 2.0
            debug ("bisection", s"a = $a, f_a = ${f(x + p * a1)}")
            if a2 - a1 < 0.001 then a
            else if f(x + p * a1) <= f(x + p * a2) then bisection (a1, a)
            else bisection (a, a2)
        end bisection

        var (go, it) = (true, 0)
//      while go && it < MAX_IT do                                  // find the end point for bisection search
        while it == 0 || go && it < MAX_IT do                       // find the end point for bisection search

            val y = x + p * a                                       // new point
            val (fy, gy) = (f(y), g(y))                             // function and gradient at y: f(y), g(y)

            debug ("lsearch", s"fy = $fy <= fx = $fx + c1 = $c1 * a = $a * gxp = $gxp")
            val wolf1 = wolfe1 (fx, fy, a, gxp)                     // Wolfe condition 1: SDC
            val wolf2 = wolfe2 (p, gy, gxp)                         // Wolfe condition 2: CC

            debug ("lsearch", s"(it = $it) a = $a, y = $y, fy = $fy, gy = $gy, wolf1 = $wolf1, wolf2 = $wolf2")

            go = ! (wolf1 && wolf2)                                 // Wolfe satisfied => termination condition

            if ! go && it == 0 then a *= 2                          // Wolfe satisfied at beginning => expand
            else if go then { a *= 0.5; it += 1 }                   // else shrink and increment
            else it += 1                                            // else increment

//          if go then a  *= 0.5                                    // use these 2 lines to avoid expansion phase
//          it += 1
        end while

        var xx = x + p * a
        println (s"final a = $a satisfies the Wolfe conditions: xx = $xx, fxx = ${f(xx)}")
//      a = bisection (0, a)                                        // comment out to skip bisection search
        xx = x + p * a
        println (s"final a = $a after bisection search: xx = $xx, fxx = ${f(xx)}")
        (a, xx)                                                     // return displacement and new point
    end lsearch

end WolfeLS2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `wolfeLS2Test` main function is used to test the `WolfeLS2` class on scalar functions.
 *  > runMain scalation.optimization.wolfeLS2Test
 */
@main def wolfeLS2Test (): Unit =

    val z0 = VectorD (0.0)                                          // zero vector, the origin
    val p  = VectorD (1.0)                                          // direction to search in

    def f(x: VectorD): Double = (x(0) - 4.0) * (x(0) - 4.0) + 1.0      // no expansion phase
//  def f(x: VectorD): Double = (x(0) - 40.0) * (x(0) - 40.0) + 1.0    // requires expansion phase

    def g(x: VectorD): VectorD = VectorD (2.0 * (x(0) - 4.0))

    val wls    = new WolfeLS2 (f, g)
    val (a, x) = wls.lsearch (z0, p)

    println ("\nProblem 1: (x - 4)^2 + 1") 
    println (s"start z0 = $z0, search direction = $p")
    println (s"displacement a     = $a")
    println (s"optimal x solution = $x")
    println (s"optimal f solution = ${f(x)}")

end wolfeLS2Test


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `wolfeLS2Test2` main function is used to test the `WolfeLS2` class on scalar functions.
 *  > runMain scalation.optimization.wolfeLS2Test2
 */
@main def wolfeLS2Test2 (): Unit =

    def f(x: VectorD): Double  = (x(0) - 2)~^2 + (x(1) - 3)~^2 + 1
    def g(x: VectorD): VectorD = VectorD (2 * x(0) - 4, 2 * x(1) - 6)

    val z0 = VectorD (0.0, 0.0)                                     // zero vector, the origin
    val p  = -g(z0)                                                 // the search direction (e.g., opposite g)
//  val p  = VectorD (1.0, 1.0)                                     // custom search direction

    val wls    = new WolfeLS2 (f, g)
    val (a, x) = wls.lsearch (z0, p)

    println ("\nProblem 1: (x_0 - 2)^2 + (x_1 - 3)^2 + 1")
    println (s"start z0 = $z0, search direction = $p")
    println (s"displacement a      = $a")
    println (s"optimal x solution = $x")
    println (s"optimal f solution = ${f(x)}")

    def f2(x: VectorD): Double  = x(0)/4 + 5*x(0)~^2 + x(0)~^4 - 9*x(0)~^2*x(1) + 3*x(1)~^2 + 2*x(1)~^4
    def g2(x: VectorD): VectorD = VectorD (0.25 + 10 * x(0) + 4.0 * x(0)~^3 - 18.0 * x(0) * x(1),
                                           9.0 * x(0)~^2 + 6.0 * x(1) + 8.0 * x(1)~^3)

    val wls2     = new WolfeLS2 (f2, g2)
    val (a2, x2) = wls2.lsearch (z0, p)

    // @see http://math.fullerton.edu/mathews/n2003/gradientsearch/GradientSearchMod/Links/GradientSearchMod_lnk_5.html

    println ("\nProblem 4: x_0/4 + 5x_0^2 + x_0^4 - 9x_0^2 x_1 + 3x_1^2 + 2x_1^4")
    println (s"start z0 = $z0, search direction = $p")
    println (s"displacement a     = $a2")
    println (s"optimal x solution = $x2")
    println (s"optimal f solution = ${f2(x2)}")

end wolfeLS2Test2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `wolfeLS2Test3` main function is used to test the `WolfeLS2` class on vector functions.
 *  This test uses the Rosenbrock function.
 *  @see https://mikl.dk/post/2019-wolfe-conditions/
 *  > runMain scalation.optimization.wolfeLS2Test3
 */ 
@main def wolfeLS2Test3 (): Unit =

    def f (x: VectorD): Double = (1.0 - x(0))~^2 + 100.0 * (x(1) - x(0)~^2)~^2

    def g (x: VectorD): VectorD = VectorD (-2.0 * (1 - x(0)) - 400.0 * x(0) * (x(1) - x(0)~^2),
                                           200 * (x(1) - x(0)~^2))

    val z0 = VectorD (-2.2, 3)
    val p  = VectorD (1625.6, 368)       // -gradient at z0

    val wls    = new WolfeLS2 (f, g)
    val (a, x) = wls.lsearch (z0, p)

    println ("\nProblem 1: (1 - x_0)^2 + 100 (x_1 - x_0^2)^2")
    println (s"start z0 = $z0, search direction = $p")
    println (s"displacement a     = $a")
    println (s"optimal x solution = $x")
    println (s"optimal f solution = ${f(x)}")

end wolfeLS2Test3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `wolfeLS2Test4` main function is used to test the `WolfeLS2` class on scalar functions.
 *  > runMain scalation.optimization.wolfeLSTest4
 */
@main def wolfeLS2Test4 (): Unit =

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

end wolfeLS2Test4

