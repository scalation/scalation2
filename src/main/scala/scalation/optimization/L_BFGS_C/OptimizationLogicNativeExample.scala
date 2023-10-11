
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Fri Sep 29 13:55:03 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Example of an optimization logic that outlines the evaluation logic needed
 *  to solve a specific optimization problem and a progress reporting logic used
 *  between iterations, both of which are used in the native implementation of
 *  the Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound
 *  constrained optimization (L-BFGS-B) algorithm.
 */

// Package.
package scalation.optimization.L_BFGS_C

// General imports.
import scalation.~^

// Project imports.
import scalation.mathstat.VectorD

// Object.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `OptimizationLogicNativeExample` object provides an example of an
 *  optimization logic to test the functionality of the [[Native]] class.
 */
object OptimizationLogicNativeExample extends OptimizationLogicNative:
    // Public methods.
    def evaluate(
        instance: Any,
        x: VectorD,
        n: Int,
        step: Double
    ): LBFGSVariableEvaluationResults =
        def fx(x: VectorD): Double = (1.0 - x(0))~^2 + 100.0 * (x(1) - x(0)~^2)~^2
        val g: VectorD = VectorD(-2.0 * (1 - x(0)) - 400.0 * x(0) * (x(1) - x(0) ~^ 2),
            200.0 * (x(1) - x(0) ~^ 2))
        val fxObjectiveValue: Double = fx(x)

        LBFGSVariableEvaluationResults(fxObjectiveValue, g)
end OptimizationLogicNativeExample
