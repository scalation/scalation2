
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @date    Tue Jan 23 10:46:44 EST 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Cube Objective Function, its Minima and Gradient Function
 */

package scalation
package optimization
package functions

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CubeFunction` object represents the Cube function for tests and benchmarks
 *  performed on function optimizers.
 */
object CubeFunction
       extends BenchmarkFunction:

    val functionMinimum: VectorD = VectorD (-1, 1)

    def objFunction (x: VectorD): Double =
        100 * (x(1) - x(0) ~^ 3) ~^ 2 + (1 - x(0)) ~^ 2

    override def gradFunction (x: VectorD): VectorD =
       VectorD (-200 * (x(1) - x(0) ~^ 3) * (3 * x(0) ~^ 2) - 2 * (1 - x(0)),

                 200 * (x(1) - x(0) ~^ 3))

end CubeFunction

