
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Aug 24 19:53:22 EDT 2011
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Line Search Optimizer
 */

package scalation
package optimization

import scala.math.{abs, sqrt}
import scala.util.control.Breaks.{breakable, break}

import scalation.mathstat._

/** the golden ratio (1.618033988749895)
 */
val G_RATIO = (1.0 + sqrt (5.0)) / 2.0

/** the golden section number (0.6180339887498949)
 */
val G_SECTION = G_RATIO / (1.0 + G_RATIO)

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GoldenSectionLS` class performs a line search on 'f(x)' to find a minimal
 *  value for 'f'.  It requires no derivatives and only one functional evaluation per
 *  iteration.  A search is conducted from 'x1' (often 0) to 'xmax'.  A guess for 'xmax'
 *  must be  given, but can be made larger during the expansion phase, that occurs
 *  before the recursive golden section search is called.  It works on scalar functions
 *  (see `goldenSectionLSTest`).  If starting with a vector function 'f(x)', simply
 *  define a new function 'g(y) = x0 + direction * y' (see `goldenSectionLSTest2`).
 *  @param f  the scalar objective function to minimize
 *  @param τ  the tolerance for breaking the iterations
 */
class GoldenSectionLS (f: FunctionS2S, τ: Double = 1E-5)
      extends LineSearch:

    private val debug  = debugf ("GoldenSectionLS", false)     // debug flag
    private val MAX_IT = 200                                   // maximum number of expansion iterations

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** A recursive golden section search requiring only one functional evaluation
     *  per call.  It works by comparing two center points x2 (given) and x4 computed.
     *  @param left  whether to search left (true) or right (false) side of last interval
     *  @param x1    the left-most point
     *  @param x2    the center point (.618 across for left and .382 across for right)
     *  @param x3    the right-most point
     *  @param f2    the functional value for the x2 center point
     */
    def gsection (left: Boolean, x1: Double, x2: Double, x3: Double, f2: Double): Double =
        var x4 = x2
        var f4 = 0.0

        debug ("gsection", s"left = $left, x1 = $x1, x2 = $x2, x3 = $x3, f2 = $f2")
        val dist = x3 - x1
//      if dist < EPSILON then return (x1 + x3) / 2.0       // mid point
        if dist < τ * (abs (x2) + abs (x4)) || dist < EPSILON then return (x1 + x3) / 2.0    // mid point
        if left then
            x4 = x3 - G_SECTION * dist                      // search left:  x1 < x4 < x2 < x3
            f4 = f(x4)
            if f4 < f2 then gsection (true,  x1, x4, x2, f4)
            else gsection (false, x4, x2, x3, f2)
        else
            x4 = x1 + G_SECTION * dist                      // search right: x1 < x2 < x4 < x3
            f4 = f(x4)
            if f2 < f4 then gsection (true,  x1, x2, x4, f2)
            else gsection (false, x2, x4, x3, f4)
        end if
    end gsection

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an exact Line Search (LS) using the Golden Search Algorithm with defaults.
     *  @param step  the initial step size
     */
    def search (step: Double = 2.0): Double = lsearch (step)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform an exact Line Search (LS) using the Golden Search Algorithm.
     *  Two phases are used:  an expansion phase (moving the end-point) to find
     *  a down-up pattern, followed by a traditional golden section search.
     *  @param xmax  a rough guess for the right end-point of the line search
     *  @param x1    the left (smallest) anchor point for the search (usually 0)
     */
    def lsearch (xmax: Double = 2.0, x1: Double = 0.0): Double =
        val f1 = f(x1)
        var x2 = x1
        var f2 = 0.0
        var x3 = xmax
        var f3 = 0.0
        var matched = false

        breakable {
            for k <- 1 to MAX_IT do                         // expand right to try to find a down-up pattern
                val dist = x3 - x1
                x2 = x1 + G_SECTION * dist
                f2 = f(x2)
                f3 = f(x3)
                debug ("lsearch", s"x1 = $x1, f1 = $f1, x2 = $x2, f2 = $f2, x3 = $x3, f3 = $f3")
                if f1 > f2 && f2 < f3 then { matched = true; break () }     // found down-up pattern, e.g., 20, 10, 30
                x2 = x3
                x3 = x1 + G_RATIO * dist                    // increase upper bound
            end for
        } // breakable

        if matched then
            gsection (true, x1, x2, x3, f2)                 // apply golden section search on expanded interval
        else
            x2 = x1 + G_SECTION * (xmax - x1)
            f2 = f(x2)
            gsection (true, x1, x2, xmax, f2)               // apply golden section search on original interval
        end if
    end lsearch
 
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print the golden ratio and the golden section.
     */
    def printGolden (): Unit =
        println ("GOLDEN_RATIO   = " + G_RATIO)
        println ("GOLDEN_SECTION = " + G_SECTION)
    end printGolden

end GoldenSectionLS


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `goldenSectionLSTest` main function is used to test the `GoldenSectionLS` class
 *  on scalar functions.
 *  > runMain scalation.optimization.goldenSectionLSTest
 */
@main def goldenSectionLSTest (): Unit =

//  def f (x: Double): Double = (x - 4.0) * (x - 4.0) + 1.0      // no expansion phase
    def f (x: Double): Double = (x - 40.0) * (x - 40.0) + 1.0    // requires expansion phase
    val solver = new GoldenSectionLS (f)
    println ("\nProblem 1: (x - 4)^2 + 1") 
    println ("optimal solution = " + solver.search (10.0))

end goldenSectionLSTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `goldenSectionLSTest2` main function is used to test the `goldenSectionLS` class
 *  on vector functions.
 *  > runMain scalation.optimization.goldenSectionLSTest
 */
@main def goldenSectionLSTest2 (): Unit =

    val zo   = VectorD (0.0, 0.0)                       // zero vector, the origin
    val dir  = VectorD (1.0, 1.0)                       // direction to search in
    val ymax = 5.0
    var y    = 0.0
    var x    = zo

    def f (x: VectorD): Double  = (x(0) - 2.0) * (x(0) - 2.0) + (x(1) - 3.0) * (x(1) - 3.0) + 1.0
    def g (y: Double): Double   = f(zo + dir * y)
    def f2 (x: VectorD): Double = x(0)/4 + 5*x(0)~^2 + x(0)~^4 - 9*x(0)~^2*x(1) + 3*x(1)~^2 + 2*x(1)~^4
    def g2 (y: Double): Double  = f2(zo + dir * y)

    val solver  = new GoldenSectionLS (g)
    val solver2 = new GoldenSectionLS (g2)

    println ("\nProblem 1: (x_0 - 2)^2 + (x_1 - 3)^2 + 1") 
    y = solver.search (ymax)
    println ("optimal y solution = " + y)
    x = zo + dir * y
    println ("optimal x solution = " + x)
    println ("optimal f solution = " + f(x))

    println ("\nProblem 4: x_0/4 + 5x_0^2 + x_0^4 - 9x_0^2 x_1 + 3x_1^2 + 2x_1^4")
//  @see math.fullerton.edu/mathews/n2003/gradientsearch/GradientSearchMod/Links/GradientSearchMod_lnk_5.html
    y = solver2.search (ymax)
    println ("optimal y solution = " + y)
    x = zo + dir * y
    println ("optimal x solution = " + x)
    println ("optimal f solution = " + f(x))

end goldenSectionLSTest2

