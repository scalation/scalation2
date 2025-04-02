
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @date    Mon Jan 22 15:40:12 EST 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Booth Objective Function, its Minima and Gradient Function
 */

package scalation
package optimization
package functions

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BoothFunction` object represents the Booth function for tests and benchmarks
 *  performed on unction optimizers.
 */
object BoothFunction
       extends BenchmarkFunction:

    val functionMinimum: VectorD = VectorD (1, 3)

    def objFunction (x: VectorD): Double =
        (x(0) + 2 * x(1) - 7) ~^ 2 + (2 * x(0) + x(1) - 5) ~^ 2

    override def gradFunction (x: VectorD): VectorD =
        VectorD (10 * x(0) + 8 * x(1) - 34,

                  8 * x(0) + 10 * x(1) - 38)

end BoothFunction

