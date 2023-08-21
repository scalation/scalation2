
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Feb 7 16:42:00 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained
 *  optimization (L-BFGS-B) algorithm wrapper. Originally proposed by Byrd et. al
 *  in 1995. See the first two links for the original paper and authors' software
 *  (written in Fortran) distribution site, respectively. This code provides a
 *  wrapper for the C implementation found in the last link.
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
import scala.math.pow
import scala.util.{Failure, Success, Try, Using}

// Project imports.
import scalation.mathstat.VectorD

// Module imports.
import FunctionDescriptors.*
import Types.{LBFGSLineSearchAlgorithm, LBFGSParameters, LBFGSReturnCode}

// Object.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Wrapper` object provides a set of functions to interact with the L-BFGS
 *  C library shared object through the use of Java's FFM abstractions, allowing
 *  the user to call the C implementation of the L-BFGS algorithm directly from
 *  Scala code.
 */
object Wrapper:
    // Library constants.
    private val LBFGS_LIBRARY_PATH_STRING = "src/main/scala/scalation/optimization/L_BFGS_C/lib/C/lbfgs/lbfgs.so"

    // Linker and library configuration.
    private val lbfgsLookup: SymbolLookup = SymbolLookup.libraryLookup(
        Path.of(LBFGS_LIBRARY_PATH_STRING),
        SegmentScope.auto()
    )
    private val linker: Linker = Linker.nativeLinker

    // Method downcall handles.
    private val lbfgsFreeHandle: MethodHandle = linker.downcallHandle(
        lbfgsLookup.find("lbfgs_free").orElseThrow(),
        LBFGS_FREE_FUNCTION_DESCRIPTOR
    )
    private val lbfgsMallocHandle: MethodHandle = linker.downcallHandle(
        lbfgsLookup.find("lbfgs_malloc").orElseThrow(),
        LBFGS_MALLOC_FUNCTION_DESCRIPTOR
    )
    private val lbfgsMainHandle: MethodHandle = linker.downcallHandle(
        lbfgsLookup.find("lbfgs").orElseThrow(),
        LBFGS_MAIN_FUNCTION_DESCRIPTOR
    )
    private val lbfgsParameterInitHandle: MethodHandle = linker.downcallHandle(
        lbfgsLookup.find("lbfgs_parameter_init").orElseThrow(),
        LBFGS_PARAMETER_INIT_FUNCTION_DESCRIPTOR
    )
    private val lbfgsStrErrorHandle: MethodHandle = linker.downcallHandle(
        lbfgsLookup.find("lbfgs_strerror").orElseThrow(),
        LBFGS_STRERROR_FUNCTION_DESCRIPTOR
    )
    private val reducedLbfgsMainHandle: MethodHandle = linker.downcallHandle(
        lbfgsLookup.find("reduced_lbfgs").orElseThrow(),
        REDUCED_LBFGS_MAIN_FUNCTION_DESCRIPTOR
    )
    private val reducedLbfgsMain2Handle: MethodHandle = linker.downcallHandle(
        lbfgsLookup.find("reduced_lbfgs2").orElseThrow(),
        REDUCED_LBFGS_MAIN2_FUNCTION_DESCRIPTOR
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
     *  @return                         Tuple with [[LBFGSReturnCode]] status,
     *                                  [[VectorD]] with optimized variable
     *                                  values and [[Option]] with the final
     *                                  function value.
     */
    def lbfgsMain(
        n: Int,
        x: VectorD,
        evaluateMethodHandle: MethodHandle,
        progressMethodHandle: MethodHandle = null,
        instanceMemorySegment: MemorySegment = MemorySegment.NULL,
        params: LBFGSParameters = LBFGSParameters()
    ): (LBFGSReturnCode, VectorD, Option[Double]) =
        checkLBFGSArgumentsForErrors(n, params) match
            case Some(errorReturnCode) => return (errorReturnCode, x, None)
            case None =>

        adjustLBFGSArguments(n, params)

        val result: Try[(LBFGSReturnCode, VectorD, Option[Double])] = Using(Arena.openConfined()) { arena =>

            val xMemorySegment: MemorySegment = MemorySegment.allocateNative(
                MemoryLayout.sequenceLayout(n, JAVA_DOUBLE),
                arena.scope()
            )

            for i <- 0 until n do
                xMemorySegment.setAtIndex(JAVA_DOUBLE, i, x(i))

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
            params.copyToMemorySegment(paramsMemorySegment)

            val returnStatusCode = lbfgsMainHandle.invokeWithArguments(
                n,
                xMemorySegment,
                fxMemorySegment,
                evaluateMemorySegment,
                progressMemorySegment,
                instanceMemorySegment,
                paramsMemorySegment
            ).asInstanceOf[Int]

            val xFinalValues: VectorD = new VectorD(n)

            for i <- 0 until n do
                xFinalValues(i) = xMemorySegment.getAtIndex(JAVA_DOUBLE, i)

            val fx = fxMemorySegment.getAtIndex(JAVA_DOUBLE, 0)

            (LBFGSReturnCode.fromCode(returnStatusCode), xFinalValues, Some(fx))
        }

        result match
            case Success(v) => v
            case Failure(e) => throw e

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** L-BFGS main function wrapper for the L-BFGS C library implementation.
     *  Calls a C shared object library to perform the L-BFGS optimization that
     *  minimizes variables according to the parameters specified by the user.
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
     *  @return                         Tuple with [[LBFGSReturnCode]] status,
     *                                  [[VectorD]] with optimized variable
     *                                  values and [[Option]] with the final
     *                                  function value.
     */
    def lbfgsMainCWrapper(
        n: Int,
        x: VectorD,
        evaluateMethodHandle: MethodHandle,
        progressMethodHandle: MethodHandle = null,
        instanceMemorySegment: MemorySegment = MemorySegment.NULL,
        params: LBFGSParameters = LBFGSParameters()
    ): (LBFGSReturnCode, VectorD, Option[Double]) =
        // Method logic.
        val result: Try[(LBFGSReturnCode, VectorD, Option[Double])] = Using(Arena.openConfined()) { arena =>

            val xMemorySegment: MemorySegment = MemorySegment.allocateNative(
                MemoryLayout.sequenceLayout(n, JAVA_DOUBLE),
                arena.scope()
            )

            for i <- 0 until n do
                xMemorySegment.setAtIndex(JAVA_DOUBLE, i, x(i))

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
            params.copyToMemorySegment(paramsMemorySegment)

            val returnStatusCode = lbfgsMainHandle.invokeWithArguments(
                n,
                xMemorySegment,
                fxMemorySegment,
                evaluateMemorySegment,
                progressMemorySegment,
                instanceMemorySegment,
                paramsMemorySegment
            ).asInstanceOf[Int]

            val xFinalValues: VectorD = new VectorD(n)

            for i <- 0 until n do
                xFinalValues(i) = xMemorySegment.getAtIndex(JAVA_DOUBLE, i)

            val fx = fxMemorySegment.getAtIndex(JAVA_DOUBLE, 0)

            (LBFGSReturnCode.fromCode(returnStatusCode), xFinalValues, Some(fx))
        }

        result match
            case Success(v) => v
            case Failure(e) => throw e

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** L-BFGS error message function wrapper for the L-BFGS C library
     *  implementation. Calls a C shared object library to determine the error
     *  message corresponding to the L-BFGS error code provided as an integer
     *  parameter.
     *
     *  @param err      Integer representing a L-BFGS error code.
     *  @return String  The error message corresponding to the given error code.
     */
    def lbfgsStrError(err: Int): String =
        val errorStringMemorySegment: MemorySegment = lbfgsStrErrorHandle.invokeWithArguments(err)
            .asInstanceOf[MemorySegment]
        val errorString: String = errorStringMemorySegment.getUtf8String(0)

        errorString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** 1st L-BFGS reduction function wrapper for the L-BFGS C library
     *  implementation. Calls a C shared object library to perform the L-BFGS
     *  optimization that minimizes variables using the 1st reduced signature of
     *  the L-BFGS function.
     *
     *  @param n    The number of variables.
     *  @param x    [[VectorD]] with the initial values of the variables.
     *  @return     Tuple with [[LBFGSReturnCode]] status, [[VectorD]] with
     *              optimized variable values and [[Option]] with the final
     *              function value.
     */
    def reducedLbfgsMain(n: Int, x: VectorD): (LBFGSReturnCode, VectorD, Option[Double]) =
        // Method logic.
        val result: Try[(LBFGSReturnCode, VectorD, Option[Double])] = Using(Arena.openConfined()) { arena =>

            val xMemorySegment: MemorySegment = MemorySegment.allocateNative(
                MemoryLayout.sequenceLayout(n, JAVA_DOUBLE),
                arena.scope()
            )

            for i <- 0 until n do
                xMemorySegment.setAtIndex(JAVA_DOUBLE, i, x(i))

            val fxMemorySegment: MemorySegment = MemorySegment.allocateNative(JAVA_DOUBLE, arena.scope())

            val returnStatusCode = reducedLbfgsMainHandle.invokeWithArguments(
                n,
                xMemorySegment,
                fxMemorySegment
            ).asInstanceOf[Int]

            val xFinalValues: VectorD = new VectorD(n)

            for i <- 0 until n do
                xFinalValues(i) = xMemorySegment.getAtIndex(JAVA_DOUBLE, i)

            val fx = fxMemorySegment.getAtIndex(JAVA_DOUBLE, 0)

            (LBFGSReturnCode.fromCode(returnStatusCode), xFinalValues, Some(fx))
        }

        result match
            case Success(v) => v
            case Failure(e) => throw e

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** 2nd L-BFGS reduction function wrapper for the C library implementation.
     *  Calls a C shared object library to perform the L-BFGS optimization that
     *  minimizes variables using the 2nd reduced signature of the L-BFGS
     *  function.
     *
     *  @param n        The number of variables.
     *  @param x        [[VectorD]] with the initial values of the variables.
     *  @param params   [[LBFGSParameters]] class representing the parameters
     *                  chosen to control the L-BFGS optimization. The default
     *                  parameters used are the defaults of the
     *                  [[LBFGSParameters]] constructor.
     *  @return         Tuple with [[LBFGSReturnCode]] status, [[VectorD]] with
     *                  optimized variable values and [[Option]] with the final
     *                  function value.
     */
    def reducedLbfgsMain2(
        n: Int,
        x: VectorD,
        params: LBFGSParameters = LBFGSParameters()
    ): (LBFGSReturnCode, VectorD, Option[Double]) =
        // Method logic.
        val result: Try[(LBFGSReturnCode, VectorD, Option[Double])] = Using(Arena.openConfined()) { arena =>

            val xMemorySegment: MemorySegment = MemorySegment.allocateNative(
                MemoryLayout.sequenceLayout(n, JAVA_DOUBLE),
                arena.scope()
            )

            for i <- 0 until n do
                xMemorySegment.setAtIndex(JAVA_DOUBLE, i, x(i))

            val fxMemorySegment: MemorySegment = MemorySegment.allocateNative(JAVA_DOUBLE, arena.scope())

            val paramsMemorySegment: MemorySegment = MemorySegment.allocateNative(
                LBFGSParameters.memoryLayout,
                arena.scope()
            )
            params.copyToMemorySegment(paramsMemorySegment)

            val returnStatusCode = reducedLbfgsMain2Handle.invokeWithArguments(
                n,
                xMemorySegment,
                fxMemorySegment,
                paramsMemorySegment
            ).asInstanceOf[Int]

            val xFinalValues: VectorD = new VectorD(n)

            for i <- 0 until n do
                xFinalValues(i) = xMemorySegment.getAtIndex(JAVA_DOUBLE, i)

            val fx = fxMemorySegment.getAtIndex(JAVA_DOUBLE, 0)

            (LBFGSReturnCode.fromCode(returnStatusCode), xFinalValues, Some(fx))
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
     *                                  [[LBFGSReturnCode]] status that
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

        None

// Test functions.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `lbfgsMainTest` main function tests the `lbfgsMain` method provided by
 *  the [[Wrapper]] object. Multiple tests are performed with different values
 *  for the variables, dimensions for the variables vector and L-BFGS
 *  optimization parameters, but always using the evaluate and progress methods
 *  provided in [[OptimizationLogicExample]].
 *
 *  This test function can be run on the sbt shell with the following command:
 *  {{{
 *  > runMain scalation.optimization.L_BFGS_C.lbfgsMainTest
 *  }}}
 */
@main def lbfgsMainTest(): Unit =
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

    println(Wrapper.lbfgsMain(2, VectorD(-1.2, 1.0), evaluateHandle, progressHandle))
    println(Wrapper.lbfgsMain(2, VectorD(-35.2, -128.43), evaluateHandle, progressHandle))
    println(Wrapper.lbfgsMain(
        2,
        VectorD(-35.2, -128.43),
        evaluateHandle,
        progressHandle,
        instance,
        LBFGSParameters(minStep = 5, maxStep = 4)
    ))
    println(Wrapper.lbfgsMain(
        4,
        VectorD(-35.2, -128.43, 0, -44),
        evaluateHandle,
        progressHandle,
        instance,
        LBFGSParameters(lineSearch = LBFGSLineSearchAlgorithm.BacktrackingStrongWolfe)
    ))
end lbfgsMainTest

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `lbfgsMainCWrapperTest` main function tests the `lbfgsMainCWrapper`
 *  method provided by the [[Wrapper]] object. Multiple tests are performed with
 *  different values for the variables, dimensions for the variables vector and
 *  L-BFGS optimization parameters, but always using the evaluate and progress
 *  methods provided in [[OptimizationLogicExample]].
 *
 *  This test function can be run on the sbt shell with the following command:
 *  {{{
 *  > runMain scalation.optimization.L_BFGS_C.lbfgsMainCWrapperTest
 *  }}}
 */
@main def lbfgsMainCWrapperTest(): Unit =
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

    println(Wrapper.lbfgsMainCWrapper(2, VectorD(-1.2, 1.0), evaluateHandle, progressHandle))
    println(Wrapper.lbfgsMainCWrapper(2, VectorD(-35.2, -128.43), evaluateHandle, progressHandle))
    println(Wrapper.lbfgsMainCWrapper(
        2,
        VectorD(-35.2, -128.43),
        evaluateHandle,
        progressHandle,
        instance,
        LBFGSParameters(minStep = 5, maxStep = 4)
    ))
    println(Wrapper.lbfgsMainCWrapper(
        4,
        VectorD(-35.2, -128.43, 0, -44),
        evaluateHandle,
        progressHandle,
        instance,
        LBFGSParameters(lineSearch = LBFGSLineSearchAlgorithm.BacktrackingStrongWolfe)
    ))
end lbfgsMainCWrapperTest

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `lbfgsStrErrorTest` main function tests the `lbfgsStrError` method
 *  provided by the [[Wrapper]] object. Multiple tests are performed with
 *  different L-BFGS error code integers.
 *
 *  This test can be run on the sbt shell with the following command:
 *  {{{
 *  > runMain scalation.optimization.L_BFGS_C.lbfgsStrErrorTest
 *  }}}
 */
@main def lbfgsStrErrorTest(): Unit =
    println(Wrapper.lbfgsStrError(-1024))
    println(Wrapper.lbfgsStrError(-1023))
end lbfgsStrErrorTest

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `reducedLbfgsMainTest` main function tests the `reducedLbfgsMain` method
 *  provided by the [[Wrapper]] object. Multiple tests are performed with
 *  different values for the variables and dimensions for the variables vector.
 *  Other possible parameters besides the ones previously mentioned are constant
 *  for any call of the 1st reduction of the L-BFGS main method and, therefore,
 *  are not taken into account for this test function.
 *
 *  This test function can be run on the sbt shell with the following command:
 *  {{{
 *  > runMain scalation.optimization.L_BFGS_C.reducedLbfgsMainTest
 *  }}}
 */
@main def reducedLbfgsMainTest(): Unit =
    println(Wrapper.reducedLbfgsMain(2, VectorD(-1.2, 1.0)))
    println(Wrapper.reducedLbfgsMain(2, VectorD(-35.2, -128.43)))
    println(Wrapper.reducedLbfgsMain(4, VectorD(-35.2, -128.43, 0, -44)))
    println(Wrapper.reducedLbfgsMain(
        16,
        VectorD(-35.2, -128.43, 0, -44, 89, 23423.2, 1000, 3, 2, 2, -89234, 23, -4, 9, 10, 18)
    ))
end reducedLbfgsMainTest

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `reducedLbfgsMain2Test` main function tests the `reducedLbfgsMain2`
 *  method provided by the [[Wrapper]] object. Multiple tests are performed with
 *  different values for the variables, dimensions for the variables vector and
 *  L-BFGS optimization parameters. The evaluate and progress logic for any call
 *  of the 2nd reduction of the L-BFGS main method is constant and given by the
 *  C function implementation.
 *
 *  This test function can be run on the sbt shell with the following command:
 *  {{{
 *  > runMain scalation.optimization.L_BFGS_C.reducedLbfgsMain2Test
 *  }}}
 */
@main def reducedLbfgsMain2Test(): Unit =
    println(Wrapper.reducedLbfgsMain2(2, VectorD(-1.2, 1.0)))
    println(Wrapper.reducedLbfgsMain2(2, VectorD(-35.2, -128.43), LBFGSParameters(maxIterations = 1, past = 1)))
    println(Wrapper.reducedLbfgsMain2(4, VectorD(-35.2, -128.43, 0, -44), LBFGSParameters(lineSearch = LBFGSLineSearchAlgorithm.BacktrackingDefault)))
    println(Wrapper.reducedLbfgsMain2(
        16,
        VectorD(-35.2, -128.43, 0, -44, 89, 23423.2, 1000, 3, 2, 2, -89234, 23, -4, 9, 10, 18),
        LBFGSParameters(m = 3, epsilon = 1e-10)
    ))
end reducedLbfgsMain2Test
