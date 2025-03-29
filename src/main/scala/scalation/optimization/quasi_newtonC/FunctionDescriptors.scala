
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Mar 14 14:51:56 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Function descriptors for the functions in the L-BFGS C library shared
 *  object. Used in the FFM implementation of the Limited memory
 *  Broyden–Fletcher–Goldfarb–Shanno (BFGS) for unconstrained optimization
 *  (L-BFGS) algorithm.
 */

package scalation
package optimization
package quasi_newtonC

import java.lang.foreign.FunctionDescriptor
import java.lang.foreign.ValueLayout.{ADDRESS, JAVA_DOUBLE, JAVA_INT}

// note: static final AddressLayout  ADDRESS
// @see https://docs.oracle.com/en/java/javase/21/docs/api/java.base/java/lang/foreign/ValueLayout.html#ADDRESS

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FunctionDescriptors` object provides a set of `FunctionDescriptors`
 *  values to model each function contained in the L-BFGS C library.
 *
 *  Used by the `LBFGS_FFM` object when constructing a `MethodHandle` to
 *  access a desired function from the L-BFGS C library shared object.
 */
object FunctionDescriptors:

    val LBFGS_EVALUATE_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.of (
        JAVA_DOUBLE,
        ADDRESS.withoutName (),                    // was .asUnbounded () - FIX - which method
        ADDRESS.withoutName (),
        ADDRESS.withoutName (),
        JAVA_INT,
        JAVA_DOUBLE)

    val LBFGS_FREE_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.ofVoid (
        ADDRESS)

    val LBFGS_MALLOC_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.of (
        ADDRESS.withoutName (),
        JAVA_INT) 

    val LBFGS_MAIN_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.of (
        JAVA_INT,
        JAVA_INT,
        ADDRESS,
        ADDRESS,
        ADDRESS,
        ADDRESS,
        ADDRESS,
        ADDRESS)

    val LBFGS_PARAMETER_INIT_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.ofVoid (
        ADDRESS)

    val LBFGS_PROGRESS_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.of (
        JAVA_INT,
        ADDRESS.withoutName (),
        ADDRESS.withoutName (),
        ADDRESS.withoutName (),
        JAVA_DOUBLE,
        JAVA_DOUBLE,
        JAVA_DOUBLE,
        JAVA_DOUBLE,
        JAVA_INT,
        JAVA_INT,
        JAVA_INT)

    val LBFGS_STRERROR_FUNCTION_DESCRIPTOR: FunctionDescriptor = FunctionDescriptor.of (
        ADDRESS.withoutName (),
        JAVA_INT)

end FunctionDescriptors

