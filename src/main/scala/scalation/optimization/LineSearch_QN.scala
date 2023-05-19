
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @date    Wed Apr 19 14:19:33 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Specialized Quasi-Newton Trait for Line Search Optimizers
 */

// Package.
package scalation.optimization

// Project imports.
import scalation.mathstat.VectorD
import scalation.optimization.L_BFGS_C.Types.{LBFGSCallbackData, LBFGSParameters}

// Trait.
trait LineSearch_QN:
  // Public method overrides.
  def search(
    n: Int,
    x: VectorD,
    f: Double,
    g: VectorD,
    s: VectorD,
    stp: VectorD,
    xp: VectorD,
    gp: VectorD,
    wp: VectorD,
    cd: LBFGSCallbackData,
    param: LBFGSParameters = LBFGSParameters()
  ): Int
