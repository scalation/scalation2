
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Jan 29 15:16:43 EST 2024
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Object to represent an example of a Paraboloid function for tests and
 *  benchmarks performed on function optimization and gradient descent classes.
 */

// Package definition.
package scalation
package optimization
package functions

// Project imports.
import scalation.mathstat.VectorD

// Object.
object ParaboloidExampleFunction extends BenchmarkFunction:
    // Public fields.
    val functionMinimum: VectorD = VectorD(3, 4)

    // Public methods.
    def objectiveFunction(x: VectorD): Double = (x(0) - 3.0) ~^ 2 + (x(1) - 4.0) ~^ 2 + 1.0

    // Overridden public methods.
    override def gradientFunction(x: VectorD): VectorD = VectorD(2 * x(0) - 6, 2 * x(1) - 8)
end ParaboloidExampleFunction
