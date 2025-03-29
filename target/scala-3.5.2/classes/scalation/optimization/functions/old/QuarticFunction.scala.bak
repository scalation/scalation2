
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @date    Mon Jan 29 15:17:54 EST 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Quartic Objective Function, its Minima and Gradient Function
 */

package scalation
package optimization
package functions

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuarticFunction` object represents an example of a Quartic function for
 *  tests and benchmarks performed on function optimizers.
 */
object QuarticFunction
       extends BenchmarkFunction:

    val functionMinimum: VectorD = VectorD (1, 4)

    def objFunction (x: VectorD): Double =
        x(0) ~^ 4 + (x(0) - 3.0) ~^ 2 + (x(1) - 4.0) ~^ 2 + 1.0

    override def gradFunction (x: VectorD): VectorD =
        VectorD (4.0 * x(0) ~^ 3 + 2 * x(0) - 6,

                   2 * x(1) - 8)

end QuarticFunction

