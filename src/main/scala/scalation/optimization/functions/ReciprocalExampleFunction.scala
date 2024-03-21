
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Jan 29 15:28:08 EST 2024
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Object to represent an example of a Reciprocal function for tests and
 *  benchmarks performed on function optimization and gradient descent classes.
 */

// Package definition.
package scalation
package optimization
package functions

// Project imports.
import scalation.mathstat.VectorD

// Object.
object ReciprocalExampleFunction extends BenchmarkFunction:
    // Public fields.
    val functionMinimum: VectorD = VectorD(1.06035, 4)

    // Public methods.
    def objectiveFunction(x: VectorD): Double = 1 / x(0) + x(0) ~^ 4 + (x(0) - 3.0) ~^ 2 + (x(1) - 4.0) ~^ 2 + 1.0

    // Overridden public methods.
    override def gradientFunction(x: VectorD): VectorD = VectorD(-(x(0) ~^ (-2)) + 4.0 * x(0) ~^ 3 + 2 * x(0) - 6, 2 * x(1) - 8)
end ReciprocalExampleFunction
