
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Thu Sep 21 09:45:03 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Results from running an evaluation logic used by the native implementation
 *  of the Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound
 *  constrained optimization (L-BFGS-B) algorithm.
 */

// Package definition.
package scalation
package optimization

// Project imports.
import scalation.mathstat.VectorD

// Case class.
case class LBFGSVariableEvaluationResults(
    objectiveFunctionValue: Double,
    gradientVector: VectorD                                     
)
