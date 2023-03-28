
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Mar 28 10:49:34 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained
 *  optimization (L-BFGS-B) algorithm optimization logic.
 */

// Package.
package scalation.optimization.L_BFGS_C

// General imports.
import java.lang.foreign.MemoryAddress
import java.lang.foreign.ValueLayout.JAVA_DOUBLE
import scala.annotation.static
import scala.math.pow

// User imports.
import scalation.optimization.L_BFGS_C.Types.LBFGSfloatval

// Object.
object OptimizationLogic {
  // Static fields.
  @static
  val instance: MemoryAddress = MemoryAddress.NULL

  // Static methods.
  @static
  def evaluate(
    instance: MemoryAddress,
    x: MemoryAddress,
    g: MemoryAddress,
    n: Int,
    step: LBFGSfloatval
  ): LBFGSfloatval = {
    var fx: LBFGSfloatval = 0

    for (i <- 0 until n by 2) {
      val xCurrentSlope: LBFGSfloatval = x.getAtIndex(JAVA_DOUBLE, i) - 2
      val xNextSlope: LBFGSfloatval = x.getAtIndex(JAVA_DOUBLE, i + 1) - 3

      g.setAtIndex(JAVA_DOUBLE, i, 2 * xCurrentSlope)
      g.setAtIndex(JAVA_DOUBLE, i + 1, 2 * xNextSlope)

      fx += pow(xCurrentSlope, 2) + pow(xNextSlope, 2) + 1
    }

    fx
  }

  @static
  def progress(
    instance: MemoryAddress,
    x: MemoryAddress,
    g: MemoryAddress,
    fx: LBFGSfloatval,
    xnorm: LBFGSfloatval,
    gnorm: LBFGSfloatval,
    step: LBFGSfloatval,
    n: Int,
    k: Int,
    ls: Int
  ): Int = {
    println()
    println(s"Iteration ${k}:")
    println(s"fx = ${fx}")

    for (i <- 0 until n) {
      println(s"x[${i}]: ${x.getAtIndex(JAVA_DOUBLE, i)}")
    }

    println(s"xnorm = ${xnorm}, gnorm = ${gnorm}, step = ${step}\n")

    0
  }
}

// Companion class.
class OptimizationLogic
