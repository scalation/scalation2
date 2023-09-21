
// Package.
package scalation.optimization.L_BFGS_C

// Project imports.
import scalation.mathstat.VectorD

// Case class.
case class LBFGSVariableEvaluationResults(
    objectiveFunctionValue: Double,
    gradientVector: VectorD                                     
)
