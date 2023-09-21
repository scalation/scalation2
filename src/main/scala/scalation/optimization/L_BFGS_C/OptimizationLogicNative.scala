
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Sep 19 09:23:45 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Trait to specify the optimization logic used by the native implementation
 *  of the Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound
 *  constrained optimization (L-BFGS-B) algorithm.
 */

// Package.
package scalation.optimization.L_BFGS_C

// Project imports.
import scalation.mathstat.VectorD

// Trait
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `OptimizationLogicNative` trait specifies the requirements for the logic
 *  to be used in each step of a L-BFGS variable minimization done by the
 *  `lbfgsMain` method of the [[Native]] object. The methods provided in this
 *  trait are called directly by the code used by the [[Native]] class.
 *
 *  Classes mixing in this trait must implement two methods: evaluate and
 *  progress. The evaluate method is used to evaluate the gradients and
 *  objective function for a given state of the variables. The progress method
 *  is used to report on how the minimization process is progressing.
 */
trait OptimizationLogicNative:
    // Public methods.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluates the gradients and objective function according to the state of
     *  the variables during the minimization process.
     *
     *  @param instance                         User data provided by each call
     *                                          of the `lbfgsMain` method. Can
     *                                          have [[Any]] type defined by the
     *                                          user as long as the same type is
     *                                          utilized in the `progress`
     *                                          method implementation for the
     *                                          class extending this trait and
     *                                          on the corresponding `lbfgsMain`
     *                                          calls from the [[Native]] object
     *                                          that relies on this
     *                                          `OptimizationLogicNative`.
     *  @param x                                [[VectorD]] with the current
     *                                          values of the variables.
     *  @param n                                The number of variables.
     *  @param step                             Current step used by the line
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Receives the progress of each iteration of the optimization process. Can
     *  be used to display or record said progress and to determine if the
     *  optimization should continue or be cancelled.
     *
     *  @param instance User data provided by each call of the `lbfgsMain`
     *                  method of the [[Native]] object. Can have [[Any]] type defined by the user as
     *                  long as the same type is utilized in the `evaluate`
     *                  method implementation for the class extending this trait
     *                  and on the corresponding `lbfgsMain` calls from the
     *                  [[Native]] object that relies on this
     *                  `OptimizationLogicNative`.
     *  @param x        [[VectorD]] with the current values of the variables.
     *  @param g        [[VectorD]] with the current value of the gradient
     *                  vector.
     *  @param fx       Current value of the objective function.
     *  @param xnorm    Euclidean norm of the variables.
     *  @param gnorm    Euclidean norm of the gradient vector.
     *  @param step     Step used by the line search routine in this iteration.
     *  @param n        The number of variables.
     *  @param k        Iteration count.
     *  @param ls       The number of evaluations called for this iteration.
     *  @return int     Determines if optimization should continue. Zero
     *                  continues optimization. Non-zero values cancel the
     *                  optimization.
     */
    def progress(
        instance: Any,
        x: VectorD,
        g: VectorD,
        fx: Double,
        xnorm: Double,
        gnorm: Double,
        step: Double,
        n: Int,
        k: Int,
        ls: Int
    ): Int
end OptimizationLogicNative
