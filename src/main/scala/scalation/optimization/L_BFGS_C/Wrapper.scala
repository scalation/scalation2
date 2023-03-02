
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

// Imports.
import java.lang.foreign.*
import java.lang.foreign.ValueLayout.{ADDRESS, JAVA_DOUBLE, JAVA_INT}
import java.lang.invoke.MethodHandle
import java.nio.file.Path
import scalation.optimization.L_BFGS_C.Types.{L_BFGS_Parameters, LBFGSfloatval}
import scala.math.pow
import scala.util.{Try, Using}

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

  // Memory layouts.
  private val LBFGS_FLOATVAL_LAYOUT: ValueLayout = JAVA_DOUBLE
  private val LBFGS_PARAMETER_LAYOUT: GroupLayout = MemoryLayout.structLayout(
    LBFGS_FLOATVAL_LAYOUT.withName("epsilon"),
    LBFGS_FLOATVAL_LAYOUT.withName("delta"),
    LBFGS_FLOATVAL_LAYOUT.withName("min_step"),
    LBFGS_FLOATVAL_LAYOUT.withName("max_step"),
    LBFGS_FLOATVAL_LAYOUT.withName("ftol"),
    LBFGS_FLOATVAL_LAYOUT.withName("wolfe"),
    LBFGS_FLOATVAL_LAYOUT.withName("gtol"),
    LBFGS_FLOATVAL_LAYOUT.withName("xtol"),
    LBFGS_FLOATVAL_LAYOUT.withName("orthantwise_c"),
    JAVA_INT.withName("m"),
    JAVA_INT.withName("past"),
    JAVA_INT.withName("max_iterations"),
    JAVA_INT.withName("linesearch"),
    JAVA_INT.withName("max_linesearch"),
    JAVA_INT.withName("orthantwise_start"),
    JAVA_INT.withName("orthantwise_end")
  ).withName("lbfgs_parameter_t")

  // Function descriptors.
  private val LBFGS_EVALUATE_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.of(
    LBFGS_FLOATVAL_LAYOUT,
    ADDRESS,
    ADDRESS,
    ADDRESS,
    JAVA_INT,
    LBFGS_FLOATVAL_LAYOUT
  )
  private val LBFGS_FREE_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.ofVoid(
    ADDRESS
  )
  private val LBFGS_MALLOC_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.of(
    ADDRESS,
    JAVA_INT
  )
  private val LBFGS_MAIN_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.of(
    JAVA_INT,
    JAVA_INT,
    ADDRESS,
    ADDRESS,
    ADDRESS,
    ADDRESS,
    ADDRESS,
    ADDRESS
  )
  private val LBFGS_PARAMETER_INIT_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.ofVoid(
    ADDRESS
  )
  private val LBFGS_PROGRESS_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.of(
    JAVA_INT,
    ADDRESS,
    ADDRESS,
    ADDRESS,
    LBFGS_FLOATVAL_LAYOUT,
    LBFGS_FLOATVAL_LAYOUT,
    LBFGS_FLOATVAL_LAYOUT,
    LBFGS_FLOATVAL_LAYOUT,
    JAVA_INT,
    JAVA_INT,
    JAVA_INT
  )
  private val LBFGS_STRERROR_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.of(
    ADDRESS,
    JAVA_INT
  )
  private val REDUCED_LBFGS_MAIN_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.of(
    JAVA_INT,
    JAVA_INT,
    ADDRESS,
    ADDRESS
  )

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

  def lbfgsStrError(err: Int): String = {
    val returnAddress: MemoryAddress = lbfgsStrErrorHandle.invokeWithArguments(err).asInstanceOf[MemoryAddress]
    val errorString: String = returnAddress.getUtf8String(0)

    errorString
  }

  def reducedLbfgsMain(n: Int, x: List[Double]): (Int, Double) = {
    val xAddress: MemoryAddress = lbfgsMallocHandle.invokeWithArguments(n).asInstanceOf[MemoryAddress]
    for(i <- 0 until n) {
      xAddress.setAtIndex(JAVA_DOUBLE, i, x(i))
    }

    val fxAddress: MemoryAddress = MemorySegment.allocateNative(JAVA_DOUBLE, MemorySession.openImplicit()).address()

    val returnStatusCode = reducedLbfgsMainHandle.invokeWithArguments(n, xAddress, fxAddress).asInstanceOf[Int]
    val fx = fxAddress.getAtIndex(JAVA_DOUBLE, 0)

    return (returnStatusCode, fx)
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
end reducedLbfgsMainTest
