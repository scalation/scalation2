
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @date    Tue Jan 23 10:45:34 EST 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Camel3 Objective Function, its Minima and Gradient Function
 */

package scalation
package optimization
package functions

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Camel3Function` object represents the Camel3 function for tests and benchmarks
 *  performed on function optimizers.
 */
object Camel3Function
       extends BenchmarkFunction:

    val functionMinimum: VectorD = VectorD (0, 0)

    def objFunction (x: VectorD): Double =
        2 * x(0) ~^ 2 - 1.05 * x(0) ~^ 4 + (1 / 6.0) * x(0) ~^ 6 + x(0) * x(1) + x(1) ~^ 2

    override def gradFunction (x: VectorD): VectorD =
        VectorD (4 * x(0) - 4.2 * x(0) ~^ 3 + x(0) ~^ 5 + x(1),

                 x(0) + 2 * x(1))

end Camel3Function

