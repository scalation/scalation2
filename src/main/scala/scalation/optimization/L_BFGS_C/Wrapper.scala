
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
import java.lang.foreign.{Linker, MemoryAddress, MemoryLayout, MemorySegment}
import java.lang.foreign.{MemorySession, SymbolLookup}
import java.lang.foreign.ValueLayout.{ADDRESS, JAVA_DOUBLE, JAVA_INT}
import java.lang.invoke.MethodHandle
import java.nio.file.Path
import scala.math.pow
import scala.util.{Failure, Success, Try, Using}

// User imports
import scalation.optimization.L_BFGS_C.FunctionDescriptors.*
import scalation.optimization.L_BFGS_C.MemoryLayouts.*
import scalation.optimization.L_BFGS_C.Types.{LBFGSfloatval, LBFGSParameters}

// Object.
object Wrapper {
  // Library constants.
  private val LBFGS_LIBRARY_PATH_STRING = "src/main/scala/scalation/optimization/L_BFGS_C/lib/C/lbfgs/lbfgs.so"

  // Linker and library configuration.
  private val lbfgsLookup: SymbolLookup = SymbolLookup.libraryLookup(
    Path.of(LBFGS_LIBRARY_PATH_STRING),
    MemorySession.openImplicit
  )
  private val linker: Linker = Linker.nativeLinker

  // Method downcall handles.
  private val lbfgsFreeHandle: MethodHandle = linker.downcallHandle(
    lbfgsLookup.lookup("lbfgs_free").orElseThrow(),
    LBFGS_FREE_FUNCTION_DESCRIPTOR
  )
  private val lbfgsMallocHandle: MethodHandle = linker.downcallHandle(
    lbfgsLookup.lookup("lbfgs_malloc").orElseThrow(),
    LBFGS_MALLOC_FUNCTION_DESCRIPTOR
  )
  private val lbfgsMainHandle: MethodHandle = linker.downcallHandle(
    lbfgsLookup.lookup("lbfgs").orElseThrow(),
    LBFGS_MAIN_FUNCTION_DESCRIPTOR
  )
  private val lbfgsParameterInitHandle: MethodHandle = linker.downcallHandle(
    lbfgsLookup.lookup("lbfgs_parameter_init").orElseThrow(),
    LBFGS_PARAMETER_INIT_FUNCTION_DESCRIPTOR
  )
  private val lbfgsStrErrorHandle: MethodHandle = linker.downcallHandle(
    lbfgsLookup.lookup("lbfgs_strerror").orElseThrow(),
    LBFGS_STRERROR_FUNCTION_DESCRIPTOR
  )
  private val reducedLbfgsMainHandle: MethodHandle = linker.downcallHandle(
    lbfgsLookup.lookup("reduced_lbfgs").orElseThrow(),
    REDUCED_LBFGS_MAIN_FUNCTION_DESCRIPTOR
  )
  private val reducedLbfgsMainHandle2: MethodHandle = linker.downcallHandle(
    lbfgsLookup.lookup("reduced_lbfgs2").orElseThrow(),
    REDUCED_LBFGS2_MAIN_FUNCTION_DESCRIPTOR
  )

  // Public methods.  
  def lbfgsStrError(err: Int): String = {
    val returnAddress: MemoryAddress = lbfgsStrErrorHandle.invokeWithArguments(err).asInstanceOf[MemoryAddress]
    val errorString: String = returnAddress.getUtf8String(0)

    errorString
  }

  def reducedLbfgsMain(n: Int, x: List[Double]): (Int, Double) = {
    val result: Try[(Int, Double)] = Using(MemorySession.openConfined()) { session =>

      val xAddress: MemoryAddress = MemorySegment.allocateNative(
        MemoryLayout.sequenceLayout(n, JAVA_DOUBLE),
        session
      ).address()
      for(i <- 0 until n) {
        xAddress.setAtIndex(JAVA_DOUBLE, i, x(i))
      }

      val fxAddress: MemoryAddress = MemorySegment.allocateNative(JAVA_DOUBLE, session).address()

      val returnStatusCode = reducedLbfgsMainHandle.invokeWithArguments(n, xAddress, fxAddress).asInstanceOf[Int]
      val fx = fxAddress.getAtIndex(JAVA_DOUBLE, 0)

      (returnStatusCode, fx)
    }

    result match {
      case Success(v) => v
      case Failure(e) => throw e
    }
  }

  def reducedLbfgsMain2(n: Int, x: List[Double], params: LBFGSParameters = LBFGSParameters()): (Int, Double) = {
    val result: Try[(Int, Double)] = Using(MemorySession.openConfined()) { session =>

      val xAddress: MemoryAddress = MemorySegment.allocateNative(
        MemoryLayout.sequenceLayout(n, JAVA_DOUBLE),
        session
      ).address()
      for (i <- 0 until n) {
        xAddress.setAtIndex(JAVA_DOUBLE, i, x(i))
      }

      val fxAddress: MemoryAddress = MemorySegment.allocateNative(JAVA_DOUBLE, session).address()

      val paramsSegment: MemorySegment = MemorySegment.allocateNative(
        LBFGS_PARAMETER_LAYOUT,
        session
      )
      params.copyToMemorySegment(paramsSegment)

      val returnStatusCode = reducedLbfgsMainHandle2.invokeWithArguments(
        n,
        xAddress,
        fxAddress,
        paramsSegment.address()
      ).asInstanceOf[Int]
      val fx = fxAddress.getAtIndex(JAVA_DOUBLE, 0)

      (returnStatusCode, fx)
    }

    result match {
      case Success(v) => v
      case Failure(e) => throw e
    }
  }
}

// Test functions.
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
