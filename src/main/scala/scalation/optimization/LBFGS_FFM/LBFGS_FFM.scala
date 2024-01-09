
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Feb 7 16:42:00 EST 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Foreign Function and Memory (FFM) wrapper for the C library shared object
 *  implementation of the Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS)
 *  for Bound constrained optimization (L-BFGS-B) algorithm. Originally proposed
 *  by Byrd et. al in 1995. See the first two links for the original paper and
 *  authors' software (written in Fortran) distribution site, respectively. See
 *  the last link for the C library implementation of the algorithm used by this
 *  class.
 *
 *  @see www.ece.northwestern.edu/~nocedal/PSfiles/limited.ps.gz
 *  @see users.iems.northwestern.edu/~nocedal/lbfgsb.html
 *  @see github.com/chokkan/liblbfgs
 */

// Package definition.
package scalation
package optimization
package LBFGS_FFM

// General imports.
import java.lang.foreign.{Arena, Linker, MemoryLayout, MemorySegment, SegmentScope, SymbolLookup}
import java.lang.foreign.ValueLayout.JAVA_DOUBLE
import java.lang.invoke.MethodHandle
import java.nio.file.Path
import scala.util.{Failure, Success, Try, Using}

// Project imports.
import scalation.mathstat.VectorD

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
    private val LBFGS_LIBRARY_PATH_STRING = "src/main/scala/scalation/optimization/LBFGS_FFM/lib/C/lbfgs/lbfgs.so"

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
     *                                      be `Some(0)`.
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

            LBFGSResults(LBFGSReturnCode.fromCode(optimizationReturnCode), xFinalValues, Some(fx))
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
    // Function definitions.
    def objectiveFunction(x: VectorD): Double = (x(0) + 2 * x(1) - 7) ~^ 2 + (2 * x(0) + x(1) - 5) ~^ 2
    def gradientFunction(x: VectorD): VectorD = VectorD(10 * x(0) + 8 * x(1) - 34, 8 * x(0) + 10 * x(1) - 38)

    // Variable declaration.
    val functionOptimizationLogic = FunctionOptimizationFFM(objectiveFunction, gradientFunction)
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
    // Function definitions.
    def objectiveFunction(x: VectorD): Double = x(0) ~^ 2 + 2 * x(1) ~^ 2 - 0.3 * math.cos(3 * math.Pi * x(0)) * math.cos(4 * math.Pi * x(1)) + 0.3
    def gradientFunction(x: VectorD): VectorD = VectorD(2 * x(0) + 0.3 * 3 * math.Pi * math.sin(3 * math.Pi * x(0)) * math.cos(4 * math.Pi * x(1)),
        4 * x(1) - 0.3 * 4 * math.Pi * math.cos(3 * math.Pi * x(0)) * math.sin(4 * math.Pi * x(1)))

    // Variable declaration.
    val functionOptimizationLogic = FunctionOptimizationFFM(objectiveFunction, gradientFunction)
    val optimizationMethodHandles = OptimizationMethodHandlesFFM.bindFromFunctionOptimizationFFM(
        functionOptimizationLogic
    )

    // Testing.
//    println(LBFGS_FFM.lbfgsMain(2, VectorD(1, 3), optimizationMethodHandles))
//    println(LBFGS_FFM.lbfgsMain(2, VectorD(2, 3.5), optimizationMethodHandles))
//    println(LBFGS_FFM.lbfgsMain(2, VectorD(0, 0), optimizationMethodHandles))
    println(LBFGS_FFM.lbfgsMain(2, VectorD(10, -10), optimizationMethodHandles, params = LBFGSParameters(maxLineSearch = 2)))
end bohachevsky2FunctionLBFGS_FFMTest
