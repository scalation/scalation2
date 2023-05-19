
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Mar 28 10:49:34 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained
 *  optimization (L-BFGS-B) algorithm optimization logic example.
 */

// Package.
package scalation.optimization.L_BFGS_C

// General imports.
import java.lang.foreign.MemorySegment
import java.lang.foreign.ValueLayout.JAVA_DOUBLE
import scala.annotation.static
import scala.math.pow

// Object.
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

// Companion class.
class OptimizationLogicExample
