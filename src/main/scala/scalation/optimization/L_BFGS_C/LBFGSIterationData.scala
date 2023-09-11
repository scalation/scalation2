
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Fri Sep 1 11:40:55 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Iteration data used in the native implementation of the Limited memory
 *  Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained optimization
 *  (L-BFGS-B) algorithm.
 */

// Package.
package scalation.optimization.L_BFGS_C

// Project imports.
import scalation.mathstat.VectorD

// Case class.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSIterationData` class stores relevant data regarding the changes
 *  made to the variable and gradient values in a single iteration of the L-BFGS
 *  algorithm. This data is used in future iterations of the algorithm to
 *  improve the search direction used to minimize the function value.
 *
 *  @param s        [[VectorD]] containing the difference between the estimates
 *                  of the variable values (`x`), each stored in a [[VectorD]],
 *                  of the last 2 iterations of the algorithm. For the ''k''-th
 *                  iteration of the algorithm, the resulting s,,k,, will be:
 *                  s,,k,, = x,,k+1,, - x,,k,,.
 *  @param y        [[VectorD]] containing the difference between the gradient
 *                  vectors (`g`), each stored in a [[VectorD]], of the last 2
 *                  iterations of the algorithm. For the ''k''-th iteration of
 *                  the algorithm, the resulting y,,k,, will be:
 *                  y,,k,, = g,,k+1,, - g,,k,,.
 *  @param rho      Inverse of the dot product between `y` and `s`. This value
 *                  is used in multiple steps when determining the search
 *                  direction to take when minimizing the function value, hence
 *                  it is calculated for each iteration. For the ''k''-th
 *                  iteration of the algorithm, the resulting ρ,,k,, will be:
 *                  ρ,,k,, = 1 / (`y`,,k,,^t^ &bull; `s`,,k,,)
 *  @param alpha    Product between `rho` and the dot product between `s` and
 *                  `q` of this iteration and next iteration, respectively. This
 *                  value is used to recalculate the `q` values for past
 *                  iterations that are kept by the algorithm. For the ''k''-th
 *                  iteration of the algorithm, the resulting α,,k,, will be:
 *                  α,,k,, = ρ,,k,, * (`s`,,k,,^t^ &bull; `q`,,k+1,,)
 */
case class LBFGSIterationData(
    s: VectorD,
    y: VectorD,
    rho: Double,
    alpha: Double
)
