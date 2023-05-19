
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @date    Wed Apr 19 14:38:29 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained
 *  optimization (L-BFGS-B) algorithm optimization logic trait.
 */

// Package.
package scalation.optimization.L_BFGS_C

// General imports.
import java.lang.foreign.MemorySegment

// Trait
trait OptimizationLogic:
  // Public methods.
  def evaluate(
    instance: MemorySegment,
    x: MemorySegment,
    g: MemorySegment,
    n: Int,
    step: Double
  ): Double

  def progress(
    instance: MemorySegment,
    x: MemorySegment,
    g: MemorySegment,
    fx: Double,
    xnorm: Double,
    gnorm: Double,
    step: Double,
    n: Int,
    k: Int,
    ls: Int
  ): Int
