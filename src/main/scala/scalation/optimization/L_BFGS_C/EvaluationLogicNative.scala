
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Fri Sep 22 16:15:18 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Trait to specify the evaluation logic used by the native implementation of
 *  the Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound
 *  constrained optimization (L-BFGS-B) algorithm.
 */

// Package.
package scalation.optimization.L_BFGS_C

// Project imports.
import scalation.mathstat.VectorD

// Trait
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `EvaluationLogicNative` trait specifies the requirements for the logic
 *  to be used for variable evaluation against the objective function in the
 *  `lbfgsMain` method of the [[Native]] object. The methods provided in this
 *  trait are called directly by the code used by the [[Native]] class.
 *
 *  Classes mixing in this trait must implement the evaluate method, which is
 *  used to evaluate the gradients and objective function for a given state of
 *  the variables.
 */
trait EvaluationLogicNative:
    // Public methods.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluates the gradients and objective function according to the state of
     *  the variables during the minimization process.
     *
     *  @param instance                         User data provided by each call
     *                                          of the `lbfgsMain` method. Can
     *                                          have [[Any]] type defined by the
     *                                          user as long as the same type is
     *                                          utilized in other instances that
     *                                          rely on this
     *                                          `EvaluationLogicNative`.
     *  @param x                                [[VectorD]] with the current
     *                                          values of the variables.
     *  @param n                                The number of variables.
     *  @param step                             Current step chosen by the line
     *                                          search routine.
     *  @return LBFGSVariableEvaluationResults  Results obtained from evaluating
     *                                          the variables.
     *                                          
     */
    def evaluate(
        instance: Any,
        x: VectorD,
        n: Int,
        step: Double
    ): LBFGSVariableEvaluationResults
end EvaluationLogicNative
