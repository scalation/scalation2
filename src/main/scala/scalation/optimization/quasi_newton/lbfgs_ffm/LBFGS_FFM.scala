
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
 */

// Package definition.
package scalation
package optimization
package quasi_newton
package lbfgs_ffm

// General imports.
import java.lang.foreign.{Arena, Linker, MemoryLayout, MemorySegment, SegmentScope, SymbolLookup}
import java.lang.foreign.ValueLayout.JAVA_DOUBLE
import java.lang.invoke.MethodHandle
import java.nio.file.Path
import scala.util.{Failure, Success, Try, Using}

// Project imports.
import scalation.mathstat.VectorD
import scalation.optimization.functions.*

// Module imports.
import FunctionDescriptors.*

// Object.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGS_FFM` object provides a set of functions to interact with the
 *  L-BFGS C library shared object through the use of Java's FFM abstractions,
 *  allowing the user to call the C implementation of the L-BFGS algorithm
 *  directly from Scala code.
 */
object LBFGS_FFM:
    // Library constants.
    private val LBFGS_LIBRARY_PATH_STRING = "src/main/scala/scalation/optimization/quasi_newton/lbfgs_ffm/lib/C/lbfgs/lbfgs.so"

    // Linker and library configuration.
    private val linker: Linker = Linker.nativeLinker
    private val lbfgsLookup: SymbolLookup = SymbolLookup.libraryLookup(
        Path.of(LBFGS_LIBRARY_PATH_STRING),
        SegmentScope.auto()
    )
    private val lbfgsMainHandle: MethodHandle = linker.downcallHandle(
        lbfgsLookup.find("lbfgs").orElseThrow(),
        LBFGS_MAIN_FUNCTION_DESCRIPTOR
    )

    // Public methods.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** L-BFGS main function FFM wrapper for the L-BFGS C library shared object
     *  implementation. Calls a C shared object library to perform the L-BFGS
     *  optimization that minimizes variables according to the parameters
     *  specified by the user.
     *
     *  @param n                            The number of variables.
     *  @param x                            [[VectorD]] with the initial values
     *                                      of the variables.
     *  @param optimizationMethodHandles    [[OptimizationMethodHandlesFFM]]
     *                                      class providing the method handles
     *                                      for the `evaluate` and `progress`
     *                                      methods needed for performing the
     *                                      variable optimization.
     *  @param instanceMemorySegment        [[MemorySegment]] with user data to
     *                                      be provided to the `evaluate` and
     *                                      `progress` methods. Can be set to
     *                                      [[MemorySegment.NULL]] if no user
     *                                      data is required on the `evaluate`
     *                                      or `progress` method calls. If not
     *                                      set to [[MemorySegment.NULL]], it
     *                                      must be encoded in the same
     *                                      [[MemoryLayout]] as the one expected
     *                                      by the implementations contained in
     *                                      the `evaluate` and `progress`
     *                                      methods.
     *  @param params                       [[LBFGSParameters]] class
     *                                      representing the parameters chosen
     *                                      to control the L-BFGS optimization.
     *                                      The default parameters used are the
     *                                      defaults of the [[LBFGSParameters]]
     *                                      constructor.
     *  @return LBFGSResults                Results for the L-BFGS optimization.
     *                                      In this implementation, if the
     *                                      objective function is never
     *                                      evaluated due to errors in the
     *                                      arguments from the method call, the
     *                                      `finalFunctionValue` returned will
     *                                      be `Some(0)`. Also, this
     *                                      implementation is currently not
     *                                      capable of determining the
     *                                      `lineSearchIncompleteResults` value,
     *                                      which will always be set to [[None]]
     *                                      regardless of how the L-BFGS
     *                                      optimization terminates.
     */
    def lbfgsMain(
        n: Int,
        x: VectorD,
        optimizationMethodHandles: OptimizationMethodHandlesFFM,
        instanceMemorySegment: MemorySegment = MemorySegment.NULL,
        params: LBFGSParameters = LBFGSParameters()
    ): LBFGSResults =
        // Method logic.
        val result: Try[LBFGSResults] = Using(Arena.openConfined()) { arena =>

            val xMemorySegment: MemorySegment = MemorySegment.allocateNative(
                MemoryLayout.sequenceLayout(n, JAVA_DOUBLE),
                arena.scope()
            )
            val fxMemorySegment: MemorySegment = MemorySegment.allocateNative(JAVA_DOUBLE, arena.scope())
            val evaluateMemorySegment: MemorySegment = linker.upcallStub(
                optimizationMethodHandles.evaluateMethodHandle,
                LBFGS_EVALUATE_FUNCTION_DESCRIPTOR,
                arena.scope()
            )
            val progressMemorySegment: MemorySegment = optimizationMethodHandles.progressMethodHandle match
                case None => MemorySegment.NULL
                case Some(progressMethodHandle) => linker.upcallStub(
                    progressMethodHandle,
                    LBFGS_PROGRESS_FUNCTION_DESCRIPTOR,
                    arena.scope()
                )
            val paramsMemorySegment: MemorySegment = MemorySegment.allocateNative(
                LBFGSParameters.memoryLayout,
                arena.scope()
            )

            x.copyToMemorySegment(xMemorySegment)
            params.copyToMemorySegment(paramsMemorySegment)

            val optimizationReturnCode = lbfgsMainHandle.invokeWithArguments(
                n,
                xMemorySegment,
                fxMemorySegment,
                evaluateMemorySegment,
                progressMemorySegment,
                instanceMemorySegment,
                paramsMemorySegment
            ).asInstanceOf[Int]

            val xFinalValues: VectorD = VectorD.fromMemorySegment(xMemorySegment, n)
            val fx = fxMemorySegment.getAtIndex(JAVA_DOUBLE, 0)

            LBFGSResults(LBFGSReturnCode.fromCode(optimizationReturnCode), xFinalValues, Some(fx), None)
        }

        result match
            case Success(v) => v
            case Failure(e) => throw e
end LBFGS_FFM

// Test functions.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `boothFunctionLBFGS_FFMTest` main function uses the Booth Function to
 *  test the `lbfgsMain` method provided by the [[LBFGS_FFM]] object. Multiple
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
 *  {{{
 *  > runMain scalation.optimization.L_BFGS_C.boothFunctionLBFGS_FFMTest
 *  }}}
 */
@main def boothFunctionLBFGS_FFMTest(): Unit =
    // Variable declaration.
    val functionOptimizationLogic = FunctionOptimizationFFM(BoothFunction)
    val optimizationMethodHandles = OptimizationMethodHandlesFFM.bindFromFunctionOptimizationFFM(
        functionOptimizationLogic
    )

    // Testing.
//    println(LBFGS_FFM.lbfgsMain(2, VectorD(1, 3), optimizationMethodHandles))
    println(LBFGS_FFM.lbfgsMain(2, VectorD(2, 3.5), optimizationMethodHandles))
//    println(LBFGS_FFM.lbfgsMain(2, VectorD(0, 0), optimizationMethodHandles))
//    println(LBFGS_FFM.lbfgsMain(2, VectorD(-4, 7), optimizationMethodHandles))
end boothFunctionLBFGS_FFMTest

@main def bohachevsky2FunctionLBFGS_FFMTest(): Unit =
    // Variable declaration.
    val functionOptimizationLogic = FunctionOptimizationFFM(Bohachevsky2Function)
    val optimizationMethodHandles = OptimizationMethodHandlesFFM.bindFromFunctionOptimizationFFM(
        functionOptimizationLogic
    )

    // Testing.
//    println(LBFGS_FFM.lbfgsMain(2, VectorD(1, 3), optimizationMethodHandles))
//    println(LBFGS_FFM.lbfgsMain(2, VectorD(2, 3.5), optimizationMethodHandles))
//    println(LBFGS_FFM.lbfgsMain(2, VectorD(0, 0), optimizationMethodHandles))
    println(LBFGS_FFM.lbfgsMain(2, VectorD(10, -10), optimizationMethodHandles, params = LBFGSParameters(lineSearchParams=LBFGSLineSearchParameters(maxLineSearch = 2))))
end bohachevsky2FunctionLBFGS_FFMTest
