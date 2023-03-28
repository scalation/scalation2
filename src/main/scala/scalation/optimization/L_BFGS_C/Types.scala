
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Fri Feb 24 15:22:30 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained
 *  optimization (L-BFGS-B) algorithm types and classes.
 */

// Package.
package scalation.optimization.L_BFGS_C

// General imports.
import java.lang.foreign.MemorySegment
import java.lang.foreign.ValueLayout.{JAVA_DOUBLE, JAVA_INT}

// User imports.
import scalation.optimization.L_BFGS_C.MemoryLayouts.{LBFGS_FLOATVAL_LAYOUT, LBFGS_PARAMETER_LAYOUT}

// Object.
object Types {
  // Type definitions.
  type LBFGSfloatval = Double

  // Case class definitions.
  case class LBFGSParameters(
    m: Int = 6,
    epsilon: LBFGSfloatval = 1e-5,
    past: Int = 0,
    delta: LBFGSfloatval = 1e-5,
    maxIterations: Int = 0,
    linesearch: Int = 0,
    maxLinesearch: Int = 40,
    minStep: LBFGSfloatval = 1e-20,
    maxStep: LBFGSfloatval = 1e20,
    ftol: LBFGSfloatval = 1e-4,
    wolfe: LBFGSfloatval = 0.9,
    gtol: LBFGSfloatval = 0.9,
    xtol: LBFGSfloatval = 1.0e-16,
    orthantwiseC: LBFGSfloatval = 0.0,
    orthantwiseStart: Int = 0,
    orthantwiseEnd: Int = -1
  ) {
    def copyToMemorySegment(destination: MemorySegment): Unit = {
      destination.set(JAVA_INT, 0, m)
      destination.set(JAVA_DOUBLE, 8, epsilon)
      destination.set(JAVA_INT, 16, past)
      destination.set(JAVA_DOUBLE, 24, delta)
      destination.set(JAVA_INT, 32, maxIterations)
      destination.set(JAVA_INT, 36, linesearch)
      destination.set(JAVA_INT, 40, maxLinesearch)
      destination.set(JAVA_DOUBLE, 48, minStep)
      destination.set(JAVA_DOUBLE, 56, maxStep)
      destination.set(JAVA_DOUBLE, 64, ftol)
      destination.set(JAVA_DOUBLE, 72, wolfe)
      destination.set(JAVA_DOUBLE, 80, gtol)
      destination.set(JAVA_DOUBLE, 88, xtol)
      destination.set(JAVA_DOUBLE, 96, orthantwiseC)
      destination.set(JAVA_INT, 104, orthantwiseStart)
      destination.set(JAVA_INT, 108, orthantwiseEnd)
    }
  }
}
