
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Mar 14 14:51:56 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained
 *  optimization (L-BFGS-B) algorithm function descriptors.
 */

// Package.
package scalation.optimization.L_BFGS_C

// General imports.
import java.lang.foreign.FunctionDescriptor
import java.lang.foreign.ValueLayout.{ADDRESS, JAVA_INT}

// User imports.
import scalation.optimization.L_BFGS_C.MemoryLayouts.LBFGS_FLOATVAL_LAYOUT

// Object.
object FunctionDescriptors {
  val LBFGS_EVALUATE_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.of(
    LBFGS_FLOATVAL_LAYOUT,
    ADDRESS,
    ADDRESS,
    ADDRESS,
    JAVA_INT,
    LBFGS_FLOATVAL_LAYOUT
  )
  val LBFGS_FREE_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.ofVoid(
    ADDRESS
  )
  val LBFGS_MALLOC_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.of(
    ADDRESS,
    JAVA_INT
  )
  val LBFGS_MAIN_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.of(
    JAVA_INT,
    JAVA_INT,
    ADDRESS,
    ADDRESS,
    ADDRESS,
    ADDRESS,
    ADDRESS,
    ADDRESS
  )
  val LBFGS_PARAMETER_INIT_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.ofVoid(
    ADDRESS
  )
  val LBFGS_PROGRESS_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.of(
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
  val LBFGS_STRERROR_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.of(
    ADDRESS,
    JAVA_INT
  )
  val REDUCED_LBFGS_MAIN_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.of(
    JAVA_INT,
    JAVA_INT,
    ADDRESS,
    ADDRESS
  )
  val REDUCED_LBFGS2_MAIN_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.of(
    JAVA_INT,
    JAVA_INT,
    ADDRESS,
    ADDRESS,
    ADDRESS
  )
}
