
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Fri Sep 29 14:08:16 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Example of an optimization logic that outlines a problem to be solved by the
 *  Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained
 *  optimization (L-BFGS-B) algorithm.
 */

// Package.
package scalation
package optimization
package L_BFGS_C

// General imports.
import java.lang.foreign.MemorySegment
import java.lang.foreign.ValueLayout.JAVA_DOUBLE
import scala.annotation.static

// Project imports.
import scalation.mathstat.VectorD

// Object.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `OptimizationLogicExample` object provides an example of an
 *  optimization logic to test the functionality of the [[Wrapper]] class.
 *
 *  A companion class is provided for the object in order to allow for the
 *  latter to have its evaluate and progress methods annotated with [[static]]
 *  to ensure that they can be both converted to a `MethodHandle` by the
 *  `MethodHandles` class.
 */
object OptimizationLogicWrapperExample:
    // Static methods.
    @static
    def evaluate(
        instance: MemorySegment,
        x: MemorySegment,
        g: MemorySegment,
        n: Int,
        step: Double
    ): Double =
        def fx(x: VectorD): Double = (1.0 - x(0))~^2 + 100.0 * (x(1) - x(0)~^2)~^2
        val xVectorD : VectorD = VectorD(
            x.getAtIndex(JAVA_DOUBLE, 0),
            x.getAtIndex(JAVA_DOUBLE, 1)
        )
        val gVectorD: VectorD = VectorD(
            -2.0 * (1 - xVectorD(0)) - 400.0 * xVectorD(0) * (xVectorD(1) - xVectorD(0) ~^ 2),
            200.0 * (xVectorD(1) - xVectorD(0) ~^ 2)
        )
        val fxObjectiveValue: Double = fx(xVectorD)

        g.setAtIndex(JAVA_DOUBLE, 0, gVectorD(0))
        g.setAtIndex(JAVA_DOUBLE, 1, gVectorD(1))

        fxObjectiveValue

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
        println(s"Iteration $k:")
        println(s"fx = $fx")

        for i <- 0 until n do
            println(s"x[$i]: ${x.getAtIndex(JAVA_DOUBLE, i)}")

        println(s"xnorm = $xnorm, gnorm = $gnorm, step = $step\n")

        0
end OptimizationLogicWrapperExample

// Companion class.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** See the documentation for the accompanying companion object.
 */
class OptimizationLogicWrapperExample
