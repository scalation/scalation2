
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @date    Tue Jan 23 10:48:17 EST 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Freudenstein-Roth Objective Function, its Minima and Gradient Function
 */

package scalation
package optimization
package functions

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FreudensteinRothFunction` object represents the Freudenstein-Roth function
 *  for tests and benchmarks performed on function optimizers.
 */
object FreudensteinRothFunction
        extends BenchmarkFunction:

    val functionMinimum: VectorD = VectorD (5, 4)

    def objFunction (x: VectorD): Double =
        (x(0) - 13 + x(1) * ((5 - x(1)) * x(1) - 2)) ~^ 2 + (x(0) - 29 + x(1) * ((x(1) + 1) * x(1) - 14)) ~^ 2

    override def gradFunction (x: VectorD): VectorD =
        VectorD (2 * (x(0) - 13 + x(1) * ((5 - x(1)) * x(1) - 2)) + 2 * (x(0) - 29 + x(1) * ((x(1) + 1) * x(1) - 14)),

                 2 * x(1) * ((5 - x(1)) * x(1) - 2) + 2 * (x(1) * ((x(1) + 1) * x(1) - 14) +
                 (x(0) - 13 + x(1) * ((5 - x(1)) * x(1) - 2)) * ((5 - x(1)) * x(1) - 2)))

end FreudensteinRothFunction

