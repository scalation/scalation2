
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @date    Mon Jan 29 15:25:28 EST 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Rosenbrock Objective Function, its Minima and Gradient Function
 */

package scalation
package optimization
package functions

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RosenbrockFunction` object represents the Rosenbrock function for tests and
 *  benchmarks performed on function optimizers.
 */
object RosenbrockFunction
       extends BenchmarkFunction:

    val functionMinimum: VectorD = VectorD (1, 1)

    def objFunction (x: VectorD): Double =
        (1.0 - x(0)) ~^ 2 + 100.0 * (x(1) - x(0) ~^ 2) ~^ 2

    override def gradFunction (x: VectorD): VectorD =
        VectorD ( -2.0 * (1 - x(0)) - 400.0 * x(0) * (x(1) - x(0) ~^ 2),

                 200.0 * (x(1) - x(0) ~^ 2))

end RosenbrockFunction

