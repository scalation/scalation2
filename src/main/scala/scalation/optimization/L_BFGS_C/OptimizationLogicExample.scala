
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Mar 28 10:49:34 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Example of an optimization logic that outlines a problem to be solved by the
 *  Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained
 *  optimization (L-BFGS-B) algorithm.
 */

// Package.
package scalation.optimization.L_BFGS_C

// General imports.
import java.lang.foreign.MemorySegment
import java.lang.foreign.ValueLayout.JAVA_DOUBLE
import scala.annotation.static
import scala.math.pow

// Object.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `OptimizationLogicExample` object provides an example of an
 *  optimization logic to test the functionality of the [[Wrapper]] class.
 *
 *  A companion class is provided for the object in order to allow for the
 *  latter to have its evaluate and progress methods annotated with [[static]]
 *  to ensure that they can be both converted to a `MethodHandle` by the
 *  `MethodHandles` class.
 *
 *  For this example logic, the `evaluate` method minimizes the following
 *  function for every even value of ''i'' that is smaller than `n`:
 *  f(x) = (x,,i,, - 2)^2^ + (x,,i+1,, - 3)^2^ + 1.
 *
 *  As such, the chosen `n` must be even. The `progress` method always returns 0
 *  and prints the iteration, function value, the value of each variable, the
 *  euclidean norms of the variables and the gradient vector and the step used
 *  in the line search in this iteration.
 */
object OptimizationLogicExample:
    // Static methods.
    @static
    def evaluate(
        instance: MemorySegment,
        x: MemorySegment,
        g: MemorySegment,
        n: Int,
        step: Double
    ): Double =
        var fx: Double = 0

        for i <- 0 until n by 2 do
            val xCurrentSlope: Double = x.getAtIndex(JAVA_DOUBLE, i) - 2
            val xNextSlope: Double = x.getAtIndex(JAVA_DOUBLE, i + 1) - 3

            g.setAtIndex(JAVA_DOUBLE, i, 2 * xCurrentSlope)
            g.setAtIndex(JAVA_DOUBLE, i + 1, 2 * xNextSlope)

            fx += pow(xCurrentSlope, 2) + pow(xNextSlope, 2) + 1

        fx

    @static
    def progress(
        instance: MemorySegment,
        x: MemorySegment,
        g: MemorySegment,
        fx: Double,
        xnorm: Double,
        gnorm: Double,
        step: Double,
        n: Int,
        k: Int,
        ls: Int
    ): Int =
        println()
        println(s"Iteration ${k}:")
        println(s"fx = ${fx}")

        for i <- 0 until n do
            println(s"x[${i}]: ${x.getAtIndex(JAVA_DOUBLE, i)}")

        println(s"xnorm = ${xnorm}, gnorm = ${gnorm}, step = ${step}\n")

        0
end OptimizationLogicExample

// Companion class.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** See the documentation for the accompanying companion object.
 */
class OptimizationLogicExample
