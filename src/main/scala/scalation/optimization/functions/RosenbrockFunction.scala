
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Jan 29 15:25:28 EST 2024
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Object to represent the Rosenbrock function for tests and benchmarks
 *  performed on function optimization and gradient descent classes.
 */

// Package definition.
package scalation
package optimization
package functions

// Project imports.
import scalation.mathstat.VectorD

// Object.
object RosenbrockFunction extends BenchmarkFunction:
    // Public fields.
    val functionMinimum: VectorD = VectorD(1, 1)

    // Public methods.
    def objectiveFunction(x: VectorD): Double = (1.0 - x(0)) ~^ 2 + 100.0 * (x(1) - x(0) ~^ 2) ~^ 2

    // Overridden public methods.
    override def gradientFunction(x: VectorD): VectorD = VectorD(
        -2.0 * (1 - x(0)) - 400.0 * x(0) * (x(1) - x(0) ~^ 2),
        200.0 * (x(1) - x(0) ~^ 2)
    )
end RosenbrockFunction
