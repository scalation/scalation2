
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

// User imports.
import scalation.optimization.L_BFGS_C.FunctionDescriptors.*
import scalation.optimization.L_BFGS_C.OptimizationLogic
import scalation.optimization.L_BFGS_C.Types.LBFGSParameters

// Object.
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
  private val reducedLbfgsMainHandle2: MethodHandle = linker.downcallHandle(
    lbfgsLookup.find("reduced_lbfgs2").orElseThrow(),
    REDUCED_LBFGS2_MAIN_FUNCTION_DESCRIPTOR
  )

  // Public methods.
  def lbfgsMain(
    n: Int,
    x: List[Double],
    evaluateMethodHandle: MethodHandle,
    progressMethodHandle: MethodHandle = null,
    instanceMemorySegment: MemorySegment = MemorySegment.NULL,
    params: LBFGSParameters = LBFGSParameters()
  ): (Int, Double) =
    val result: Try[(Int, Double)] = Using(Arena.openConfined()) { arena =>

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
      val fx = fxMemorySegment.getAtIndex(JAVA_DOUBLE, 0)

      (returnStatusCode, fx)
    }

    result match
      case Success(v) => v
      case Failure(e) => throw e

  def lbfgsStrError(err: Int): String =
    val errorStringMemorySegment: MemorySegment = lbfgsStrErrorHandle.invokeWithArguments(err)
      .asInstanceOf[MemorySegment]
    val errorString: String = errorStringMemorySegment.getUtf8String(0)

    errorString

  def reducedLbfgsMain(n: Int, x: List[Double]): (Int, Double) =
    val result: Try[(Int, Double)] = Using(Arena.openConfined()) { arena =>

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
      val fx = fxMemorySegment.getAtIndex(JAVA_DOUBLE, 0)

      (returnStatusCode, fx)
    }

    result match
      case Success(v) => v
      case Failure(e) => throw e

  def reducedLbfgsMain2(n: Int, x: List[Double], params: LBFGSParameters = LBFGSParameters()): (Int, Double) =
    val result: Try[(Int, Double)] = Using(Arena.openConfined()) { arena =>

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

      val returnStatusCode = reducedLbfgsMainHandle2.invokeWithArguments(
        n,
        xMemorySegment,
        fxMemorySegment,
        paramsMemorySegment
      ).asInstanceOf[Int]
      val fx = fxMemorySegment.getAtIndex(JAVA_DOUBLE, 0)

      (returnStatusCode, fx)
    }

    result match
      case Success(v) => v
      case Failure(e) => throw e

// Test functions.
@main def lbfgsMainTest(): Unit =

  val evaluateHandle: MethodHandle = MethodHandles.lookup.findStatic(
    classOf[OptimizationLogic],
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
    classOf[OptimizationLogic],
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

  println(Wrapper.lbfgsMain(2, List(-1.2, 1.0), evaluateHandle, progressHandle))
  println(Wrapper.lbfgsMain(2, List(-35.2, -128.43), evaluateHandle, progressHandle))
  println(Wrapper.lbfgsMain(
    4,
    List(-35.2, -128.43, 0, -44),
    evaluateHandle,
    progressHandle,
    OptimizationLogic.instance
  ))
end lbfgsMainTest

@main def lbfgsStrErrorTest(): Unit =
  println(Wrapper.lbfgsStrError(-1024))
  println(Wrapper.lbfgsStrError(-1023))
end lbfgsStrErrorTest

@main def reducedLbfgsMainTest(): Unit =
  println(Wrapper.reducedLbfgsMain(2, List(-1.2, 1.0)))
  println(Wrapper.reducedLbfgsMain(2, List(-35.2, -128.43)))
  println(Wrapper.reducedLbfgsMain(4, List(-35.2, -128.43, 0, -44)))
  println(Wrapper.reducedLbfgsMain(
    16,
    List(-35.2, -128.43, 0, -44, 89, 23423.2, 1000, 3, 2, 2, -89234, 23, -4, 9, 10, 18)
  ))
  println()
end reducedLbfgsMainTest

@main def reducedLbfgsMainTest2(): Unit =
  println(Wrapper.reducedLbfgsMain2(2, List(-1.2, 1.0)))
  println(Wrapper.reducedLbfgsMain2(2, List(-35.2, -128.43), LBFGSParameters(maxIterations=1, past=1)))
  println(Wrapper.reducedLbfgsMain2(4, List(-35.2, -128.43, 0, -44), LBFGSParameters(linesearch=2)))
  println(Wrapper.reducedLbfgsMain2(
    16,
    List(-35.2, -128.43, 0, -44, 89, 23423.2, 1000, 3, 2, 2, -89234, 23, -4, 9, 10, 18),
    LBFGSParameters(m=3, epsilon=1e-10)
  ))
  println()
end reducedLbfgsMainTest2
