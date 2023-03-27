
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Mar 14 14:51:09 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained
 *  optimization (L-BFGS-B) algorithm memory layouts.
 */

// Package.
package scalation.optimization.L_BFGS_C

// General imports.
import java.lang.foreign.{GroupLayout, MemoryLayout, ValueLayout}
import java.lang.foreign.ValueLayout.{JAVA_DOUBLE, JAVA_INT}

// Object.
object MemoryLayouts {
  val LBFGS_FLOATVAL_LAYOUT: ValueLayout = JAVA_DOUBLE
  val LBFGS_PARAMETER_LAYOUT: GroupLayout = MemoryLayout.structLayout(
    JAVA_INT.withName("m"),
    MemoryLayout.paddingLayout(32),
    JAVA_DOUBLE.withName("epsilon"),
    JAVA_INT.withName("past"),
    MemoryLayout.paddingLayout(32),
    JAVA_DOUBLE.withName("delta"),
    JAVA_INT.withName("max_iterations"),
    JAVA_INT.withName("linesearch"),
    JAVA_INT.withName("max_linesearch"),
    MemoryLayout.paddingLayout(32),
    JAVA_DOUBLE.withName("min_step"),
    JAVA_DOUBLE.withName("max_step"),
    JAVA_DOUBLE.withName("ftol"),
    JAVA_DOUBLE.withName("wolfe"),
    JAVA_DOUBLE.withName("gtol"),
    JAVA_DOUBLE.withName("xtol"),
    JAVA_DOUBLE.withName("orthantwise_c"),
    JAVA_INT.withName("orthantwise_start"),
    JAVA_INT.withName("orthantwise_end")
  ).withName("lbfgs_parameter_t")
}
