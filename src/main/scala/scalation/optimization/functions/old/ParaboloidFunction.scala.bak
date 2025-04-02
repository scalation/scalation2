
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @date    Mon Jan 29 15:16:43 EST 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Paraboloid Objective Function, its Minima and Gradient Function
 */

package scalation
package optimization
package functions

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ParaboloidFunction` object represents an example of a Paraboloid function
 *  for tests and benchmarks performed on function optimizers.
 */
object ParaboloidFunction
       extends BenchmarkFunction:

    val functionMinimum: VectorD = VectorD (3, 4)

    def objFunction (x: VectorD): Double =
        (x(0) - 3.0) ~^ 2 + (x(1) - 4.0) ~^ 2 + 1.0

    override def gradFunction (x: VectorD): VectorD =
        VectorD (2 * x(0) - 6,

                 2 * x(1) - 8)

end ParaboloidFunction

