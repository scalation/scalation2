
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Wed Oct 11 14:03:06 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Case class to store the definition of a function optimization in a format
 *  that adheres to the optimization logic format used by the native
 *  implementation of the Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS)
 *  for unconstrained optimization (L-BFGS) algorithm.
 */

// Package definition.
package scalation
package optimization
package quasi_newton

// Project imports.
import scalation.calculus.Differential
import scalation.mathstat.{FunctionV2S, FunctionV2V, VectorD}

// Case class.
case class FunctionOptimization(
    objectiveFunction: FunctionV2S,
    gradientFunction: FunctionV2V
) extends OptimizationLogic:
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
end FunctionOptimization

// Companion object.
case object FunctionOptimization:
    // Public methods.
    def apply(objectiveFunction: FunctionV2S) =
        new FunctionOptimization(objectiveFunction)
end FunctionOptimization
