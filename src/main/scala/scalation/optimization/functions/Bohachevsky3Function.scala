
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @date    Tue Jan 23 10:41:25 EST 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Bohachevsky3 Objective Function, its Minima and Gradient Function
 */

package scalation
package optimization
package functions

import scala.math.{cos, Pi, sin}

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Bohachevsky3` object epresents the Bohachevsky3 function for tests and benchmarks
 *  performed on function optimizers.
 */
object Bohachevsky3Function
       extends BenchmarkFunction:

    val functionMinimum: VectorD = VectorD (0, 0)

    def objFunction (x: VectorD): Double =
        x(0) ~^ 2 + 2 * x(1) ~^ 2 - 0.3 * cos (3 * Pi * x(0) + 4 * Pi * x(1)) + 0.3

    override def gradFunction (x: VectorD): VectorD =
        VectorD (2 * x(0) + 0.9 * Pi * sin (3 * Pi * x(0) + 4 * Pi * x(1)),

                 4 * x(1) + 1.2 * Pi * sin (3 * Pi * x(0) + 4 * Pi * x(1)))

end Bohachevsky3Function

