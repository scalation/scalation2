
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Feb 7 16:42:00 EST 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Foreign Function and Memory (FFM) wrapper for the C library shared object
 *  implementation of the Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS)
 *  for unconstrained optimization (L-BFGS) algorithm. See the following link
 *  for the C library implementation of the algorithm used by this object.
 *
 *  @see github.com/chokkan/liblbfgs
 *  @see https://docs.oracle.com/en/java/javase/21/core/foreign-function-and-memory-api.html
 *  @see https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/lang/foreign/package-summary.html
 */

package scalation
package optimization
package quasi_newtonC

import java.lang.foreign.{Arena, Linker, MemoryLayout, MemorySegment, StructLayout, SymbolLookup}
import java.lang.foreign.ValueLayout.{JAVA_DOUBLE, JAVA_INT}
import java.lang.invoke.MethodHandle
import java.nio.file.Path

//import scala.annotation.static
import scala.util.{Failure, Success, Try, Using}

import scalation.mathstat.VectorD

import functions.*
import quasi_newton.{LBFGSLineSearchPrms, LBFGSPrms, LBFGSResults, LBFGSReturnCode, OrthantWisePrms}

import FunctionDescriptors.*

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGS_FFM` object provides a set of functions to interact with the
 *  L-BFGS C library shared object through the use of Java's FFM abstractions,
 *  allowing the user to call the C implementation of the L-BFGS algorithm
 *  directly from Scala code.
 */
object LBFGS_FFM:

    // This variable is only needed for the LBFGS_FFM object implementation. If
    // you are NOT working with LBFGS_FFM, feel free to disregard it.
//  @static
    val memoryLayout: StructLayout = MemoryLayout.structLayout (
        JAVA_INT.withName ("m"),
        MemoryLayout.paddingLayout (32),
        JAVA_DOUBLE.withName ("epsilon"),
        JAVA_INT.withName ("past"),
        MemoryLayout.paddingLayout (32),
        JAVA_DOUBLE.withName ("delta"),
        JAVA_INT.withName ("max_iterations"),
        JAVA_INT.withName ("linesearch"),
        JAVA_INT.withName ("max_linesearch"),
        MemoryLayout.paddingLayout (32),
        JAVA_DOUBLE.withName ("default_step"),
        JAVA_DOUBLE.withName ("min_step"),
        JAVA_DOUBLE.withName ("max_step"),
        JAVA_DOUBLE.withName ("ftol"),
        JAVA_DOUBLE.withName ("wolfe"),
        JAVA_DOUBLE.withName ("gtol"),
        JAVA_DOUBLE.withName ("xtol"),
        JAVA_DOUBLE.withName ("orthantwise_c"),
        JAVA_INT.withName ("orthantwise_start"),
        JAVA_INT.withName ("orthantwise_end")).withName ("lbfgs_parameter_t")

    private val LBFGS_LIBRARY_PATH_STRING = "src/main/scala/scalation/optimization/quasi_newtonC/lib/C/lbfgs/lbfgs.so"

    // Linker and library configuration.
    private val linker: Linker = Linker.nativeLinker

    private val lbfgsLookup: SymbolLookup = SymbolLookup.libraryLookup (
        Path.of (LBFGS_LIBRARY_PATH_STRING), Arena.global)
//      SegmentScope.auto ())

    private val lbfgsMainHandle: MethodHandle = linker.downcallHandle (
        lbfgsLookup.find ("lbfgs").orElseThrow(),
        LBFGS_MAIN_FUNCTION_DESCRIPTOR)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** L-BFGS main function FFM wrapper for the L-BFGS C library shared object
     *  implementation.  Calls a C shared object library to perform the L-BFGS
     *  optimization that minimizes variables according to the parameters
     *  specified by the user.
     *
     *  @param n                          The number of variables.
     *  @param x                          `VectorD` with the initial values of the variables.
     *  @param optimizationMethodHandles  `OptimizationMethodHandlesFFM` class providing the
     *                                    method handles for the `evaluate` and `progress`
     *                                    methods needed for performing the variable optimization.
     *  @param instanceMemorySegment      `MemorySegment` with user data to be provided to the
     *                                    `evaluate` and `progress` methods.  Can be set to
     *                                    `MemorySegment.NULL` if no user data is required on
     *                                    the `evaluate` or `progress` method calls.  If not
     *                                    set to `MemorySegment.NULL` it must be encoded in the
     *                                    same `MemoryLayout` as the one expected by the
     *                                    implementations contained in the `evaluate` and
     *                                    `progress` methods.
     *  @param params                     `LBFGSPrms` class representing the parameters
     *                                    chosen to control the L-BFGS optimization.
     *                                    The default parameters used are the defaults of the
     *                                    `LBFGSPrms` constructor.
     *  @return LBFGSResults              Results for the L-BFGS optimization.  In this
     *                                    implementation, if the objective function is never
     *                                    evaluated due to errors in the arguments from the
     *                                    method call, the `finalFunctionValue` returned will
     *                                    be `Some(0)`.  Also, this implementation is currently
     *                                    not capable of determining the
     *                                    `lineSearchIncompleteResults` value, which will always
     *                                    be set to `None` regardless of how the L-BFGS 
     *                                    optimization terminates.
     */
    def lbfgsMain (n: Int, x: VectorD,
        optimizationMethodHandles: OptimizationMethodHandlesFFM,
        instanceMemorySegment: MemorySegment = MemorySegment.NULL,
        params: LBFGSPrms = LBFGSPrms ()): LBFGSResults =

        val result: Try [LBFGSResults] = Using (Arena.ofConfined ()) { arena =>

            val xMemorySegment:  MemorySegment = Arena.global ().allocate (n, 8)
            val fxMemorySegment: MemorySegment = Arena.global ().allocate (1, 8)
//          val fxMemorySegment: MemorySegment = MemorySegment.allocateNative (JAVA_DOUBLE, arena.scope())

            val evaluateMemorySegment: MemorySegment = linker.upcallStub (
                optimizationMethodHandles.evaluateMethodHandle,
                LBFGS_EVALUATE_FUNCTION_DESCRIPTOR, arena)            // .scope ())

            val progressMemorySegment: MemorySegment = optimizationMethodHandles.progressMethodHandle match
                case None => MemorySegment.NULL
                case Some(progressMethodHandle) => linker.upcallStub (
                    progressMethodHandle, LBFGS_PROGRESS_FUNCTION_DESCRIPTOR, arena)     // .scope ())

            val paramsMemorySegment: MemorySegment = Arena.global ().allocate (memoryLayout)
//              MemorySegment.allocateNative (memoryLayout, arena.scope())

            FunctionOptimizationFFM.copyToMemorySegment (x, xMemorySegment)
            copyToMemorySegment (params, paramsMemorySegment)

            val optimizationReturnCode = lbfgsMainHandle.invokeWithArguments (
                n, xMemorySegment, fxMemorySegment, evaluateMemorySegment, progressMemorySegment,
                instanceMemorySegment, paramsMemorySegment).asInstanceOf [Int]

            val xFinalValues: VectorD = FunctionOptimizationFFM.fromMemorySegment (xMemorySegment, n)
            val fx = fxMemorySegment.getAtIndex (JAVA_DOUBLE, 0)

            LBFGSResults (LBFGSReturnCode.fromCode (optimizationReturnCode), xFinalValues, Some(fx), None)}

        result match
            case Success (v) => v
            case Failure (e) => throw e
    end lbfgsMain

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Copies the data in this `LBFGSPrs` instance to a `destination` `MemorySegment`.
     *
     *  It is expected by this method that the `destination` memory segment was
     *  allocated using the `memoryLayout` `MemoryLayout`.
     *
     *  This method is only needed for the LBFGS_FFM object implementation. If
     *  you are NOT working with LBFGS_FFM, feel free to disregard it.
     *
     *  @param prm          the LBFGS Parameters
     *  @param destination  `MemorySegment` that will receive the data from this `LBFGSPrms` instance.
     *                      Must have been allocated using the `memoryLayout` `MemoryLayout`.
     *
     */
    def copyToMemorySegment (prm: LBFGSPrms, destination: MemorySegment): Unit =
        val orthantWisePrms = prm.orthantWise.getOrElse (OrthantWisePrms (0, 0, Some(-1)))

        destination.set (JAVA_INT, 0,      prm.m)
        destination.set (JAVA_DOUBLE, 8,   prm.epsilon)
        destination.set (JAVA_INT, 16,     prm.past)
        destination.set (JAVA_DOUBLE, 24,  prm.delta)
        destination.set (JAVA_INT, 32,     prm.maxIterations)
        destination.set (JAVA_INT, 36,     prm.lineSearch.number)
        destination.set (JAVA_INT, 40,     prm.lineSearchPrms.maxLineSearch)
        destination.set (JAVA_DOUBLE, 48,  prm.lineSearchPrms.defaultStep)
        destination.set (JAVA_DOUBLE, 56,  prm.lineSearchPrms.minStep)
        destination.set (JAVA_DOUBLE, 64,  prm.lineSearchPrms.maxStep)
        destination.set (JAVA_DOUBLE, 72,  prm.lineSearchPrms.ftol)
        destination.set (JAVA_DOUBLE, 80,  prm.lineSearchPrms.wolfe)
        destination.set (JAVA_DOUBLE, 88,  prm.lineSearchPrms.gtol)
        destination.set (JAVA_DOUBLE, 96,  prm.lineSearchPrms.xtol)
        destination.set (JAVA_DOUBLE, 104, orthantWisePrms.c)
        destination.set (JAVA_INT, 112,    orthantWisePrms.start)
        destination.set (JAVA_INT, 116,    orthantWisePrms.end.getOrElse(-1))
    end copyToMemorySegment

end LBFGS_FFM


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `boothFunctionLBFGS_FFMTest` main function uses the Booth Function to
 *  test the `lbfgsMain` method provided by the `LBFGS_FFM` object.  Multiple
 *  tests are performed with different values for the variables.
 *
 *  The Booth Function can be described as follows:
 *
 *  - Input dimension: 2;
 *
 *  - Function domain: -10 &le; x,,i,, &le; 10;
 *
 *  - Function definition: f(x) = (x,,0,, + 2 * x,,1,, - 7)^2^ + (2 * x,,0,, +
 *  x,,1,, - 5)^2^;
 *
 *  - Global minimum: x* = (1, 3); f(x*) = 0;
 *
 *  This test function can be run on the sbt shell with the following command:
 *  > runMain scalation.optimization.quasi_newtonC.boothFunctionLBFGS_FFMTest
 */
@main def boothFunctionLBFGS_FFMTest (): Unit =

    val functionOptimizationLogic = FunctionOptimizationFFM (BoothFunction)
    val optimizationMethodHandles = OptimizationMethodHandlesFFM.bindFromFunctionOptimizationFFM (
        functionOptimizationLogic)

//  println (LBFGS_FFM.lbfgsMain (2, VectorD(1, 3), optimizationMethodHandles))
    println (LBFGS_FFM.lbfgsMain (2, VectorD(2, 3.5), optimizationMethodHandles))
//  println (LBFGS_FFM.lbfgsMain (2, VectorD(0, 0), optimizationMethodHandles))
//  println (LBFGS_FFM.lbfgsMain (2, VectorD(-4, 7), optimizationMethodHandles))

end boothFunctionLBFGS_FFMTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bohachevsky2FunctionLBFGS_FFMTest` main function uses the Bohachevsky 2 Function
 *  to test the `lbfgsMain` method provided by the `LBFGS_FFM` object.  Multiple
 *  tests are performed with different values for the variables.
 *  > runMain scalation.optimization.quasi_newtonC.bohachevsky2FunctionLBFGS_FFMTest
 */
@main def bohachevsky2FunctionLBFGS_FFMTest (): Unit =

    val functionOptimizationLogic = FunctionOptimizationFFM (Bohachevsky2Function)
    val optimizationMethodHandles = OptimizationMethodHandlesFFM.bindFromFunctionOptimizationFFM (
        functionOptimizationLogic)

//  println (LBFGS_FFM.lbfgsMain(2, VectorD(1, 3), optimizationMethodHandles))
//  println (LBFGS_FFM.lbfgsMain(2, VectorD(2, 3.5), optimizationMethodHandles))
//  println (LBFGS_FFM.lbfgsMain(2, VectorD(0, 0), optimizationMethodHandles))
    println (LBFGS_FFM.lbfgsMain(2, VectorD(10, -10), optimizationMethodHandles,
             params = LBFGSPrms (lineSearchPrms = LBFGSLineSearchPrms (maxLineSearch = 2))))

end bohachevsky2FunctionLBFGS_FFMTest

