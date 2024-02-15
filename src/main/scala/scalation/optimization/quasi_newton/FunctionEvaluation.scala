
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Feb 05 16:11:37 EST 2024
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Case class to store the definition of a function evaluation in a format that
 *  adheres to the evaluation logic format used by the native implementation of
 *  the Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for unconstrained
 *  optimization (L-BFGS) algorithm.
 */

// Package definition.
package scalation
package optimization
package quasi_newton

// Project imports.
import scalation.calculus.Differential
import scalation.mathstat.{FunctionV2S, FunctionV2V, VectorD}

// Case class.
case class FunctionEvaluation(
    objectiveFunction: FunctionV2S,
    gradientFunction: FunctionV2V
) extends EvaluationLogic:
    // Constructor definitions.
    def this(objectiveFunction: FunctionV2S) = this(
        objectiveFunction,
        // Less accurate than hard-coded definition of gradient function.
        (x: VectorD) => Differential.grad(objectiveFunction, x)
    )
    
    // Public methods.
    def evaluate(
        instance: Any,
        x: VectorD,
        n: Int,
        step: Double
    ): LBFGSVariableEvaluationResults =
        LBFGSVariableEvaluationResults(objectiveFunction(x), gradientFunction(x))
end FunctionEvaluation

// Companion object.
case object FunctionEvaluation:
    // Public methods.
    def apply(objectiveFunction: FunctionV2S) =
        new FunctionEvaluation(objectiveFunction)
end FunctionEvaluation
