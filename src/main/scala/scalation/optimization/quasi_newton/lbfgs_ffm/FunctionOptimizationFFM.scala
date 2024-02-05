
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Wed Oct 11 14:21:26 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Case class to store the definition of a function optimization in a format
 *  that adheres to the optimization logic format used by the FFM implementation
 *  of the Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for
 *  unconstrained optimization (L-BFGS) algorithm.
 */

// Package definition.
package scalation
package optimization
package quasi_newton
package lbfgs_ffm

// General imports.
import java.lang.foreign.MemorySegment

// Project imports.
import scalation.calculus.Differential
import scalation.mathstat.{FunctionV2S, FunctionV2V, VectorD}

// Case class.
case class FunctionOptimizationFFM(
    objectiveFunction: FunctionV2S,
    gradientFunction: FunctionV2V                            
) extends OptimizationLogicFFM:
    // Constructor definitions.
    def this(objectiveFunction: FunctionV2S) = this(
        objectiveFunction,
        // Less accurate than hard-coded definition of gradient function.
        (x: VectorD) => Differential.grad(objectiveFunction, x)
    )
    
    // Public methods.
    def evaluate(
        instance: MemorySegment,
        x: MemorySegment,
        g: MemorySegment,
        n: Int,
        step: Double
    ): Double =
        val xVectorD = VectorD.fromMemorySegment(x, n)
        val gVectorD = gradientFunction(xVectorD)

        gVectorD.copyToMemorySegment(g)
        
        objectiveFunction(xVectorD)
end FunctionOptimizationFFM

// Companion object.
case object FunctionOptimizationFFM:
    // Public methods.
    def apply(objectiveFunction: FunctionV2S) =
        new FunctionOptimizationFFM(objectiveFunction)
end FunctionOptimizationFFM
