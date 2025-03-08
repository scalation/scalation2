
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Wed Apr 19 14:38:29 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Trait to specify the optimization logic used by the wrapper implementation
 *  of the Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for
 *  unconstrained optimization (L-BFGS) algorithm.
 */

package scalation
package optimization
package quasi_newtonC

import java.lang.foreign.MemorySegment
import java.lang.foreign.ValueLayout.JAVA_DOUBLE

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `OptimizationLogicWrapper` trait specifies the requirements for the
 *  logic to be used in each step of a L-BFGS variable minimization done by the
 *  `lbfgsMain` method of the `LBFGS_FFM` object. The methods provided in this
 *  trait are called as a `MethodHandle` by the L-BFGS C library shared object,
 *  such that pointer arguments are declared with the `MemorySegment` type.
 *
 *  Classes extending this trait must implement two methods: evaluate and
 *  progress. The evaluate method is used to evaluate the gradients and
 *  objective function for a given state of the variables. The progress method
 *  is used to report on how the minimization process is progressing.
 */
trait OptimizationLogicFFM:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluates the gradients and objective function according to the state of
     *  the variables during the minimization process.
     *
     *  @param instance User data provided by each call of the `lbfgsMain`
     *                  method of the `LBFGS_FFM` object. Can have any
     *                  `MemoryLayout` defined by the user as long as the same
     *                  layout is utilized in the `progress` method
     *                  implementation for the class extending this trait and on
     *                  the corresponding `lbfgsMain` calls from the
     *                  `LBFGS_FFM` object that relies on this
     *                  `OptimizationLogicWrapper`.
     *  @param x        Current values of the variables presented in a
     *                  `MemorySegment` containing `n` elements with the
     *                  `ValueLayout` of `JAVA_DOUBLE`.
     *  @param g        Return location for the gradient vector that will be
     *                  calculated with the current variables (this parameter is
     *                  used to RETURN values, NOT to RECEIVE them). Empty
     *                  `MemorySegment` with capacity for `n` elements with
     *                  the `ValueLayout` of `JAVA_DOUBLE`.
     *  @param n        The number of variables. Also, the number of elements in
     *                  the `MemorySegment` parameters `x` and `g`.
     *  @param step     Current step used by the line search routine.
     *  @return Double  Value of the objective function computed with the given
     *                  variables.
     */
    def evaluate (instance: MemorySegment, x: MemorySegment, g: MemorySegment,
                  n: Int, step: Double): Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Receives the progress of each iteration of the optimization process. Can
     *  be used to display or record said progress and to determine if the
     *  optimization should continue or be cancelled.
     *  
     *  The default implementation of this method always returns 0 and prints
     *  the iteration, function value, the value of each variable, the euclidean
     *  norms of the variables and the gradient vector and the step used in the
     *  line search in this iteration.
     *
     *  @param instance User data provided by each call of the `lbfgsMain`
     *                  method of the `LBFGS_FFM` object. Can have any
     *                  `MemoryLayout` defined by the user as long as the same
     *                  layout is utilized in the `evaluate` method
     *                  implementation for the class extending this trait and on
     *                  the corresponding `lbfgsMain` calls from the
     *                  `LBFGS_FFM` object that relies on this
     *                  `OptimizationLogicWrapper`.
     *  @param x        Current values of the variables presented in a
     *                  `MemorySegment` containing `n` elements with the
     *                  `ValueLayout` of `JAVA_DOUBLE`.
     *  @param g        Current value of the gradient vector presented in a
     *                  `MemorySegment` containing `n` elements with the
     *                  `ValueLayout` of `JAVA_DOUBLE`.
     *  @param fx       Current value of the objective function.
     *  @param xnorm    Euclidean norm of the variables.
     *  @param gnorm    Euclidean norm of the gradient vector.
     *  @param step     Step used by the line search routine in this iteration.
     *  @param n        The number of variables. Also, the number of elements in
     *                  the `MemorySegment` parameters `x` and `g`.
     *  @param k        Iteration count.
     *  @param ls       The number of evaluations called for this iteration.
     *  @return int     Determines if optimization should continue. Zero
     *                  continues optimization. Non-zero values cancel the
     *                  optimization.
     */
    def progress (instance: MemorySegment, x: MemorySegment, g: MemorySegment,
                  fx: Double, xnorm: Double, gnorm: Double, step: Double,
                  n: Int, k: Int, ls: Int): Int =
        println ()
        println (s"Iteration $k:")
        println (s"fx = $fx")

        for i <- 0 until n do
            println (s"x[$i]: ${x.getAtIndex(JAVA_DOUBLE, i)}")

        println (s"xnorm = $xnorm, gnorm = $gnorm, step = $step\n")

        0
    end progress

end OptimizationLogicFFM

