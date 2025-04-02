
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Feb 05 16:11:37 EST 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Definition of a Function Evaluation for the L-BFGS Algorithm
 *           to define the objective and gradient functions
 */

package scalation
package optimization
package quasi_newton

import scalation.calculus.Differential
import scalation.mathstat.{FunctionV2S, FunctionV2V, VectorD}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FunctionEvaluation` case class to store the definition of a function evaluation
 *  in a format that adheres to the evaluation logic format used by the implementation of
 *  the Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for unconstrained
 *  optimization (L-BFGS) algorithm.
 *  @param objFunction   the multi-variate objective function (vector -> scalar)
 *  @param gradFunction  the gradient vector-valued function  (vector -> vector)
 */
case class FunctionEvaluation (objFunction: FunctionV2S, gradFunction: FunctionV2V)
     extends EvaluationLogic:
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** This constructor uses numerical approximation for the gradient which is less
     *  accurate than hard-coded definition of gradient function.
     *  @param objFunction  the object finction to be optimized
     */
    def this (objFunction: FunctionV2S) =
        this (objFunction, (x: VectorD) => Differential.grad (objFunction, x)) 

    def evaluate (instance: Any, x: VectorD, n: Int, step: Double): LBFGSVarEvaluationResults =
        LBFGSVarEvaluationResults (objFunction (x), gradFunction (x))

end FunctionEvaluation


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FunctionEvaluation` companion object provides a factory method.
 */
case object FunctionEvaluation:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a function evaluation object.
     *  @param objFunction   the multi-variate objective function (vector -> scalar)
     */
    def apply (objFunction: FunctionV2S) = new FunctionEvaluation (objFunction)

end FunctionEvaluation

