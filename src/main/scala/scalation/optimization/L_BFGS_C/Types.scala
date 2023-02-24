
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Fri Feb 24 15:23:00 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained
 *  optimization (L-BFGS-B) algorithm types and related case classes.
 */

// Package.
package scalation.optimization.L_BFGS_C

// Object.
object Types {
  // Type definitions.
  type LBFGSfloatval = Double

  // Case class definitions.
  case class L_BFGS_Parameters(
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
                              )
}
