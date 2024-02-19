
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @date    Mon Jan 22 15:21:01 EST 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Beale Objective Function, its Minima and Gradient Function
 */

package scalation
package optimization
package functions

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BealeFunction` object represents the Beale function for tests and benchmarks
 *  performed on function optimizers.
 */
object BealeFunction
       extends BenchmarkFunction:

    val functionMinimum: VectorD = VectorD (3, 0.5)

    def objFunction (x: VectorD): Double =
        (1.5 - x(0) + x(0) * x(1)) ~^ 2 + (2.25 - x(0) + x(0) * (x(1) ~^ 2)) ~^ 2 + (2.625 - x(0) + x(0) * (x(1) ~^ 3)) ~^ 2

    override def gradFunction (x: VectorD): VectorD =
        VectorD (2 * (1.5 - x(0) + x(0) * x(1)) * (-1 + x(1)) +
                 2 * (2.25 - x(0) + x(0) * (x(1) ~^ 2)) * (-1 + (x(1) ~^ 2)) +
                 2 * (2.625 - x(0) + x(0) * (x(1) ~^ 3)) * (-1 + (x(1) ~^ 3)),

                 2 * (1.5 - x(0) + x(0) * x(1)) * x(0) +
                 2 * (2.25 - x(0) + x(0) * (x(1) ~^ 2)) * (2 * x(0) * x(1)) +
                 2 * (2.625 - x(0) + x(0) * (x(1) ~^ 3)) * (3 * x(0) * (x(1) ~^ 2)))

end BealeFunction

