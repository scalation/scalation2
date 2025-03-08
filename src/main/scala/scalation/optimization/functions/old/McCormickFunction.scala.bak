
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @date    Mon Jan 22 15:33:02 EST 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    McCormick Objective Function, its Minima and Gradient Function
 */

package scalation
package optimization
package functions

import scala.math.{cos, sin}

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `McCormickFunction` object represents the McCormick function for tests and
 *  benchmarks performed on function optimizers.
 */
object McCormickFunction
       extends BenchmarkFunction:

    val functionMinimum: VectorD = VectorD (-0.54719, -1.54719)

    def objFunction (x: VectorD): Double =
        sin (x(0) + x(1)) + (x(0) - x(1)) ~^ 2 - 1.5 * x(0) + 2.5 * x(1) + 1

    override def gradFunction (x: VectorD): VectorD =
        VectorD (-1.5 + 2 * x(0) - 2 * x(1) + cos (x(0) + x(1)),

                  2.5 - 2 * x(0) + 2 * x(1) + cos (x(0) + x(1)))

end McCormickFunction

