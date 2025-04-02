
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @date    Fri Sep 22 16:15:18 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Specification of evaluation logic used by Quasi-Newton Optimizers.
 */

package scalation
package optimization
package functions

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `EvaluationLogic` trait specifies the requirements for the logic to
 *  used for evaluation of the objective function in Quasi-Newton optimzers.
 *  Classes mixing in this trait must implement the evaluate method, which is
 *  used to evaluate the gradients and objective function for a given state of
 *  the variables.
 */
trait EvaluationLogic:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluates the gradients and objective function according to the state of
     *  the variables during the minimization process.
     *
     *  @param instance                    User data provided by each call of the optimizers main method.
     *                                     Can have [[Any]] type defined by the user as long as the same
     *                                     type is utilized in other instances that rely on this
     *                                     `EvaluationLogic`.
     *  @param x                           [[VectorD]] with the current values of the variables.
     *  @param n                           The number of variables.
     *  @param step                        Current step chosen by the line search routine.
     *  @return FunctionEvaluationResults  Results obtained from evaluating the variables.
     */
    def evaluate (instance: Any, x: VectorD, n: Int, step: Double): FunctionEvaluationResults

end EvaluationLogic

