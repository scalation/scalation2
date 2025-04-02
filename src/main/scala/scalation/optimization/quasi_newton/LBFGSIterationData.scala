
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Fri Sep 1 11:40:55 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Holds Data about Previous Iterations of the L-BFGS Algorithm.
 */

package scalation
package optimization
package quasi_newton

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSIterationData` case class stores relevant data regarding the changes
 *  made to the variable and gradient values in a single iteration of the native
 *  implementation of the L-BFGS algorithm. This data is used in future
 *  iterations of the algorithm to improve the search direction used to minimize
 *  the function value.
 *
 *  @param s        `VectorD` containing the difference between the estimates
 *                  of the variable values (`x`), each stored in a `VectorD`
 *                  of the last 2 iterations of the algorithm. For the ''k''-th
 *                  iteration of the algorithm, the resulting s,,k,, will be:
 *                  s,,k,, = x,,k+1,, - x,,k,,.
 *  @param y        `VectorD` containing the difference between the gradient
 *                  vectors (`g`), each stored in a `VectorD` of the last 2
 *                  iterations of the algorithm. For the ''k''-th iteration of
 *                  the algorithm, the resulting y,,k,, will be:
 *                  y,,k,, = g,,k+1,, - g,,k,,.
 *  @param ys       Dot product between `y` and `s`. This value is used in
 *                  multiple steps when determining the search direction to take
 *                  when minimizing the function value, hence it is calculated
 *                  once for each iteration. For the ''k''-th iteration of the
 *                  algorithm, the resulting ys,,k,, will be:
 *                  `y`,,k,,^t^ &bull; `s`,,k,,.
 *  @param alpha    Product between `rho` and the dot product between `s` and
 *                  `q` of this iteration and next iteration, respectively. This
 *                  value is used to recalculate the `q` values for past
 *                  iterations that are kept by the algorithm. For the ''k''-th
 *                  iteration of the algorithm, the resulting α,,k,, will be:
 *                  α,,k,, = ρ,,k,, * (`s`,,k,,^t^ &bull; `q`,,k+1,,).
 */
case class LBFGSIterationData (s: VectorD, y: VectorD, ys: Double, var alpha: Double)

