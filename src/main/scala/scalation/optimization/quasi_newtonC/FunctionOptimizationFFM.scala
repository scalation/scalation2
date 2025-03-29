
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
 *
 *  @see @see foreign-function-and-memory-api
 */

package scalation
package optimization
package quasi_newtonC

import java.lang.foreign.MemorySegment
import java.lang.foreign.ValueLayout.JAVA_DOUBLE

import scalation.calculus.Differential
import scalation.mathstat.{FunctionV2S, FunctionV2V, VectorD}
import scalation.optimization.functions.BenchmarkFunction

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FunctionOptimizationFFM` case class ...
 */
case class FunctionOptimizationFFM (objFunction: FunctionV2S, gradFunction: FunctionV2V)
     extends OptimizationLogicFFM:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    // Less accurate than hard-coded definition of gradient function.
    def this (objFunction: FunctionV2S) =
        this (objFunction, (x: VectorD) => Differential.grad (objFunction, x))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    def this (benchmarkFunction: BenchmarkFunction) =
        this (benchmarkFunction.objFunction, benchmarkFunction.gradFunction)
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    def evaluate (instance: MemorySegment, x: MemorySegment, g: MemorySegment,
                  n: Int, step: Double): Double =
        val xVectorD = FunctionOptimizationFFM.fromMemorySegment (x, n)
        val gVectorD = gradFunction (xVectorD)
        FunctionOptimizationFFM.copyToMemorySegment (gVectorD, g)
        objFunction (xVectorD)
    end evaluate

end FunctionOptimizationFFM


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FunctionOptimizationFFM` companion case object ...
 */
case object FunctionOptimizationFFM:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    def apply (objectiveFunction: FunctionV2S) =
        new FunctionOptimizationFFM(objectiveFunction)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    def apply (benchmarkFunction: BenchmarkFunction) =
        new FunctionOptimizationFFM(benchmarkFunction)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Copy the contents of a `VectorD` to the `destination` `MemorySegment`.
     *  Assumes `destination` was allocated with the memory layout of a sequence
     *  layout of JAVA_DOUBLE with size bigger or equal to `dim`.
     *  @param vec   the vector whose contents are to be copied to the memory segment
     *  @param dest  `MemorySegment` where `VectorD` contents are copied to.
     */
    def copyToMemorySegment (vec: VectorD, dest: MemorySegment): Unit =
        val arr = vec.toArray
        for i <- 0 until vec.dim do dest.setAtIndex (JAVA_DOUBLE, i, arr(i))
    end copyToMemorySegment

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `VectorD` `vec` from the `source` `MemorySegment` by copying the contents
     *  of the latter.  Assumes `source` was allocated with the memory layout of a
     *  sequence layout of JAVA_DOUBLE.
     *  @param source  `MemorySegment` whose content is copied to initialize a new `VectorD`
     *  @param n       the number of JAVA_DOUBLE elements in `source`.
     */
    def fromMemorySegment (source: MemorySegment, n: Int): VectorD =
        val vec = new VectorD (n)
        for i <- 0 until n do vec(i) = source.getAtIndex (JAVA_DOUBLE, i)
        vec
    end fromMemorySegment

end FunctionOptimizationFFM

