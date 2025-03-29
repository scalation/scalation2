
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Wed Oct 11 14:03:06 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Definition of a Function Optimization for the L-BFGS) Algorithm.
 */

package scalation
package optimization
package quasi_newton

import scalation.calculus.Differential
import scalation.mathstat.{FunctionV2S, FunctionV2V, VectorD}
import scalation.optimization.functions.BenchmarkFunction

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FunctionOptimization` case class to store the definition of a function optimization
 *  in a format that adheres to the optimization logic format used by the implementation of the
 *  Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for unconstrained optimization (L-BFGS) algorithm.
 */
case class FunctionOptimization (objFunction: FunctionV2S, gradFunction: FunctionV2V)
     extends OptimizationLogic:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** This constructor uses numerical approximation for the gradient which is less
     *  accurate than hard-coded definition of gradient function.
     *  @param objFunction  the object finction to be optimized
     */
    def this (objFunction: FunctionV2S) = 
        this (objFunction, (x: VectorD) => Differential.grad (objFunction, x))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    def this (benchmarkFunction: BenchmarkFunction) =
        this (benchmarkFunction.objFunction, benchmarkFunction.gradFunction)
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    def evaluate (instance: Any, x: VectorD, n: Int, step: Double): LBFGSVarEvaluationResults =
        LBFGSVarEvaluationResults (objFunction (x), gradFunction (x))

end FunctionOptimization


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The FunctionOptimization` companion object provides two factory methods.
 */
case object FunctionOptimization:

    def apply (objFunction: FunctionV2S) =
        new FunctionOptimization (objFunction)
        
    def apply (benchmarkFunction: BenchmarkFunction) =
        new FunctionOptimization (benchmarkFunction)

end FunctionOptimization

