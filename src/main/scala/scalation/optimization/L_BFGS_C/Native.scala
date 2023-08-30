
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Aug 22 15:39:53 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Native Scala implementation of the Limited memory
 *  Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained optimization
 *  (L-BFGS-B) algorithm. Originally proposed by Byrd et. al in 1995. See the
 *  first two links for the original paper and authors' software (written in
 *  Fortran) distribution site, respectively. This Scala implementation was made
 *  based on the C implementation of the same algorithm found in the last link.
 *  @see www.ece.northwestern.edu/~nocedal/PSfiles/limited.ps.gz
 *  @see users.iems.northwestern.edu/~nocedal/lbfgsb.html
 *  @see github.com/chokkan/liblbfgs
 */

// Package.
package scalation.optimization.L_BFGS_C

// General imports.
import java.lang.foreign.{Arena, Linker, MemoryLayout, MemorySegment, SegmentScope, SymbolLookup}
import java.lang.foreign.ValueLayout.JAVA_DOUBLE
import java.lang.invoke.{MethodHandle, MethodHandles, MethodType}
import java.nio.file.Path
import scala.util.{Failure, Success, Try, Using}

// Project imports.
import scalation.mathstat.VectorD

// Module imports.
import FunctionDescriptors.LBFGS_EVALUATE_FUNCTION_DESCRIPTOR
import FunctionDescriptors.LBFGS_NATIVE_STUB_FUNCTION_DESCRIPTOR
import FunctionDescriptors.LBFGS_PROGRESS_FUNCTION_DESCRIPTOR

// Object.
object Native:

    // Library constants.
    private val LBFGS_LIBRARY_PATH_STRING = "src/main/scala/scalation/optimization/L_BFGS_C/lib/C/lbfgs/lbfgs.so"

    // Method downcall handle configuration (testing stub during development).
    private val lbfgsLookup: SymbolLookup = SymbolLookup.libraryLookup(
        Path.of(LBFGS_LIBRARY_PATH_STRING),
        SegmentScope.auto()
    )
    private val linker: Linker = Linker.nativeLinker

    private val lbfgsNativeStubHandle: MethodHandle = linker.downcallHandle(
        lbfgsLookup.find("lbfgs_native_stub").orElseThrow(),
        LBFGS_NATIVE_STUB_FUNCTION_DESCRIPTOR
    )

    // Public methods.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Performs the L-BFGS optimization that optimizes variables to minimize a
     *  function value.
     *
     *  @param n                        The number of variables.
     *  @param x                        [[VectorD]] with the initial values of
     *                                  the variables.
     *  @param evaluateMethodHandle     [[MethodHandle]] to perform gradient
     *                                  evaluation on the values of the
     *                                  variables. Method signature must follow
     *                                  the one outlined for the `evaluate`
     *                                  method in [[OptimizationLogic]].
     *  @param progressMethodHandle     [[MethodHandle]] to report the progress
     *                                  on the minimization of the variables.
     *                                  Can be set to `null` if a progress
     *                                  report is not required. If not `null`,
     *                                  the method signature must follow the one
     *                                  outlined for the `progress` method in
     *                                  [[OptimizationLogic]].
     *  @param instanceMemorySegment    [[MemorySegment]] with user data to be
     *                                  provided to the `evaluate` and
     *                                  `progress` methods. Can be set to
     *                                  [[MemorySegment.NULL]] if no user data
     *                                  is required on the `evaluate` or
     *                                  `progress` method calls. If not set to
     *                                  [[MemorySegment.NULL]], it must be
     *                                  encoded in the same [[MemoryLayout]] as
     *                                  the one expected by the implementations
     *                                  contained in the `evaluate` and
     *                                  `progress` method handles.
     *  @param params                   [[LBFGSParameters]] class representing
     *                                  the parameters chosen to control the
     *                                  L-BFGS optimization. The default
     *                                  parameters used are the defaults of the
     *                                  [[LBFGSParameters]] constructor.
     *  @return LBFGSResults            Results for the L-BFGS optimization. The
     *                                  `optimizedVariables` field represents
     *                                  the values of `x` that have been
     *                                  optimized to minimize the objective
     *                                  function. In this implementation, if the
     *                                  objective function is never evaluated
     *                                  due to errors in the arguments from the
     *                                  method call, the `finalFunctionValue`
     *                                  returned will be [[None]].
     */
    def lbfgsMain(
         n: Int,
         x: VectorD,
         evaluateMethodHandle: MethodHandle,
         progressMethodHandle: MethodHandle = null,
         instanceMemorySegment: MemorySegment = MemorySegment.NULL,
         params: LBFGSParameters = LBFGSParameters()
    ): LBFGSResults =
        checkLBFGSArgumentsForErrors(n, params) match
            case Some(errorReturnCode) => return LBFGSResults(errorReturnCode, x, None)
            case None =>

        adjustLBFGSArguments(n, params)

        val result: Try[LBFGSResults] = Using(Arena.openConfined()) { arena =>

            val xMemorySegment: MemorySegment = MemorySegment.allocateNative(
                MemoryLayout.sequenceLayout(n, JAVA_DOUBLE),
                arena.scope()
            )
            val fxMemorySegment: MemorySegment = MemorySegment.allocateNative(JAVA_DOUBLE, arena.scope())
            val evaluateMemorySegment: MemorySegment = linker.upcallStub(
                evaluateMethodHandle,
                LBFGS_EVALUATE_FUNCTION_DESCRIPTOR,
                arena.scope()
            )
            val progressMemorySegment: MemorySegment = progressMethodHandle match
                case null => MemorySegment.NULL
                case _ => linker.upcallStub(
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

            val optimizationReturnCode = lbfgsNativeStubHandle.invokeWithArguments(
                n,
                xMemorySegment,
                fxMemorySegment,
                evaluateMemorySegment,
                progressMemorySegment,
                instanceMemorySegment,
                paramsMemorySegment
            ).asInstanceOf[Int]

            val xFinalValues: VectorD = VectorD.fromMemorySegment(xMemorySegment)
            val fx = fxMemorySegment.getAtIndex(JAVA_DOUBLE, 0)

            LBFGSResults(LBFGSReturnCode.fromCode(optimizationReturnCode), xFinalValues, Some(fx))
        }

        result match
            case Success(v) => v
            case Failure(e) => throw e

    // Private methods.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Adjusts the L-BFGS optimization arguments so that the L-BFGS
     *  optimization will work correctly.
     *
     *  @param n        The number of variables.
     *  @param params   [[LBFGSParameters]] class representing the parameters
     *                  chosen to control the L-BFGS optimization.
     */
    private def adjustLBFGSArguments(n: Int, params: LBFGSParameters): Unit =
        if params.orthantwiseEnd < 0 then params.orthantwiseEnd = n

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Checks if the L-BFGS optimization arguments have an error and returns an
     *  [[Option]] with the [[LBFGSReturnCode]] that represents the first error
     *  found.
     *
     *  @param n                        The number of variables.
     *  @param params                   [[LBFGSParameters]] class representing
     *                                  the parameters chosen to control the
     *                                  L-BFGS optimization.
     *  @return Option[LBFGSReturnCode] [[Option]] value with a
     *                                  [[LBFGSReturnCode]] return code that
     *                                  represents the first error found in the
     *                                  `params` argument. If there are no
     *                                  errors in the `params` argument,
     *                                  [[None]] is returned.
     */
    private def checkLBFGSArgumentsForErrors(n: Int, params: LBFGSParameters): Option[LBFGSReturnCode] =
        if n <= 0 then return Some(LBFGSReturnCode.InvalidN)
        if params.epsilon < 0.0 then return Some(LBFGSReturnCode.InvalidEpsilon)
        if params.past < 0 then return Some(LBFGSReturnCode.InvalidTestPeriod)
        if params.delta < 0.0 then return Some(LBFGSReturnCode.InvalidDelta)
        if params.minStep < 0.0 then return Some(LBFGSReturnCode.InvalidMinStep)
        if params.maxStep < params.minStep then return Some(LBFGSReturnCode.InvalidMaxStep)
        if params.ftol < 0.0 then return Some(LBFGSReturnCode.InvalidFTOL)
        if params.lineSearch == LBFGSLineSearchAlgorithm.BacktrackingWolfe ||
            params.lineSearch == LBFGSLineSearchAlgorithm.BacktrackingStrongWolfe then
            if params.wolfe <= params.ftol || 1.0 <= params.wolfe then return Some(LBFGSReturnCode.InvalidWolfe)
        end if
        if params.gtol < 0.0 then return Some(LBFGSReturnCode.InvalidGTOL)
        if params.xtol < 0.0 then return Some(LBFGSReturnCode.InvalidXTOL)
        if params.maxLineSearch <= 0 then return Some(LBFGSReturnCode.InvalidMaxLineSearch)
        if params.orthantwiseC < 0.0 then return Some(LBFGSReturnCode.InvalidOrthantwise)
        if params.orthantwiseStart < 0 || n < params.orthantwiseStart then
            return Some(LBFGSReturnCode.InvalidOrthantwiseStart)
        end if
        if n < params.orthantwiseEnd then return Some(LBFGSReturnCode.InvalidOrthantwiseEnd)
        if params.orthantwiseC != 0.0 &&
            params.lineSearch != LBFGSLineSearchAlgorithm.BacktrackingDefault &&
            params.lineSearch != LBFGSLineSearchAlgorithm.BacktrackingWolfe
        then
            return Some(LBFGSReturnCode.InvalidLineSearch)
        end if

        None
end Native

// Test functions.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `lbfgsMainNativeTest` main function tests the `lbfgsMain` method
 *  provided by the [[Native]] object. Multiple tests are performed with
 *  different values for the variables, dimensions for the variables vector and
 *  L-BFGS optimization parameters, but always using the evaluate and progress
 *  methods provided in [[OptimizationLogicExample]].
 *
 *  This test function can be run on the sbt shell with the following command:
 *  {{{
 *  > runMain scalation.optimization.L_BFGS_C.lbfgsMainNativeTest
 *  }}}
 */
@main def lbfgsMainNativeTest(): Unit =
    // Variable declaration.
    val instance: MemorySegment = MemorySegment.NULL

    // Setup Scala method handles.
    val evaluateHandle: MethodHandle = MethodHandles.lookup.findStatic(
        classOf[OptimizationLogicExample],
        "evaluate",
        MethodType.methodType(
            classOf[Double],
            classOf[MemorySegment],
            classOf[MemorySegment],
            classOf[MemorySegment],
            classOf[Int],
            classOf[Double]
        )
    )

    val progressHandle: MethodHandle = MethodHandles.lookup.findStatic(
        classOf[OptimizationLogicExample],
        "progress",
        MethodType.methodType(
            classOf[Int],
            classOf[MemorySegment],
            classOf[MemorySegment],
            classOf[MemorySegment],
            classOf[Double],
            classOf[Double],
            classOf[Double],
            classOf[Double],
            classOf[Int],
            classOf[Int],
            classOf[Int]
        )
    )

    println(Native.lbfgsMain(2, VectorD(-1.2, 1.0), evaluateHandle, progressHandle))
    println(Native.lbfgsMain(2, VectorD(-35.2, -128.43), evaluateHandle, progressHandle))
    println(Native.lbfgsMain(
        2,
        VectorD(-35.2, -128.43),
        evaluateHandle,
        progressHandle,
        instance,
        LBFGSParameters(minStep = 5, maxStep = 4)
    ))
    println(Native.lbfgsMain(
        4,
        VectorD(-35.2, -128.43, 0, -44),
        evaluateHandle,
        progressHandle,
        instance,
        LBFGSParameters(lineSearch = LBFGSLineSearchAlgorithm.BacktrackingStrongWolfe)
    ))
end lbfgsMainNativeTest
