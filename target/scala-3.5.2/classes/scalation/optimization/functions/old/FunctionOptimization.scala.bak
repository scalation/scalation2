
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @date    Wed Oct 11 14:03:06 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Function Optimization Grouping an Objective Function and its Gradient
 */

package scalation
package optimization
package functions

import scalation.calculus.Differential
import scalation.mathstat.{FunctionV2S, FunctionV2V, VectorD}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FunctionOptimization` case class to store the definition of a function optimization
 *  in a format that adheres to the optimization logic format used by Quasi-Newton optimizers.
 *  @param objFunction   the objective function to be minimized
 *  @param gradFunction  the gradient function of the objective function
 */
case class FunctionOptimization (objFunction: FunctionV2S, gradFunction: FunctionV2V)
     extends OptimizationLogic:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    def evaluate (instance: Any, x: VectorD, n: Int, step: Double): FunctionEvaluationResults =
        FunctionEvaluationResults (objFunction (x), gradFunction (x))
    end evaluate

end FunctionOptimization


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FunctionOptimization` companion object contains a factory method.
 */
object FunctionOptimization:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a Function Optimization object using numerical approximation for the gradient.
     *  @param objFunction   the objective function to be minimized
     */
    def apply (objFunction: FunctionV2S) =
        new FunctionOptimization (objFunction, (x: VectorD) => Differential.grad (objFunction, x))
    end apply

end FunctionOptimization


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FunctionEvaluationResults` case class collects the results from running
 *  evaluation logic for a Quasi-Newton optimizer.
 *  @param objFunctionValue  the optimal value found for objective function
 *  @param gradVector        the corresponding value for the gradient
 */
case class FunctionEvaluationResults (objFunctionValue: Double, gradVector: VectorD)

