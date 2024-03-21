
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Wed Nov 29 10:30:58 EST 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Results from running an evaluation logic used by the native implementation
 *  of the Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for
 *  unconstrained optimization (L-BFGS) algorithm.
 */

// Package definition.
package scalation
package optimization
package quasi_newton

// Project imports.
import scalation.mathstat.VectorD

// Case class.
case class LBFGSVariableEvaluationResults(
    objectiveFunctionValue: Double,
    gradientVector: VectorD                                     
)
