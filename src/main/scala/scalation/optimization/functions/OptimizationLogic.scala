
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @date    Tue Sep 19 09:23:45 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Specification of Optimization Logic used by Quasi-Newton Optimizers
 */

package scalation
package optimization
package functions

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `OptimizationLogicNative` trait specifies the requirements for the logic
 *  to be used in each step of Quasi-Newton optimizers.
 *  Classes mixing in this trait must implement two methods: evaluate and
 *  progress. The evaluate method is used to evaluate the gradients and
 *  objective function for a given state of the variables. The progress method
 *  is used to report on how the minimization process is progressing.
 */
trait OptimizationLogic
      extends EvaluationLogic:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Receives the progress of each iteration of the optimization process. Can
     *  be used to display or record said progress and to determine if the
     *  optimization should continue or be cancelled. A default implementation
     *  is provided to just print the contents of the current iteration of the
     *  optimization.
     *
     *  @param instance User data provided by each call of the optimizer's invocation
     *                  method. Can have [[Any]] type defined by the user as long as
     *                  the same type is utilized in the `evaluate` method implementation
     *                  for the class extending this trait.
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
     *  @return ReturnCode Determines if optimization should continue. Zero continues
     *                     optimization. Non-zero values cancel the optimization.
     */
    def progress (instance: Any, x: VectorD, g: VectorD, fx: Double,
                  xnorm: Double, gnorm: Double, step: Double,
                  n: Int, k: Int, ls: Int): ReturnCode =
        println (s"""
    Iteration $k:
    x \t\t= $x
    g \t\t= $g
    fx \t\t= $fx
    xnorm \t= $xnorm
    gnorm \t= $gnorm
    step \t= $step
    n \t\t= $n
    ls \t\t= $ls
        """)
        ReturnCode.Success
    end progress

end OptimizationLogic

