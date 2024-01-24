
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Jan 23 10:48:17 EST 2024
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Object to represent the Freudenstein-Roth function for tests and benchmarks
 *  performed on function optimization and gradient descent classes.
 */

// Package definition.
package scalation
package optimization
package functions

// Project imports.
import scalation.mathstat.VectorD

// Object.
object FreudensteinRothFunction extends BenchmarkFunction:
    // Public fields.
    val functionMinimum: VectorD = VectorD(5, 4)

    // Public methods.
    def objectiveFunction(x: VectorD): Double = (x(0) - 13 + x(1) * ((5 - x(1)) * x(1) - 2)) ~^ 2 + (x(0) - 29 + x(1) * ((x(1) + 1) * x(1) - 14)) ~^ 2

    // Overridden public methods.
    override def gradientFunction(x: VectorD): VectorD = VectorD(
        2 * (x(0) - 13 + x(1) * ((5 - x(1)) * x(1) - 2)) + 2 * (x(0) - 29 + x(1) * ((x(1) + 1) * x(1) - 14)),
        2 * x(1) * ((5 - x(1)) * x(1) - 2) + 2 * (x(1) * ((x(1) + 1) * x(1) - 14) + (x(0) - 13 + x(1) * ((5 - x(1)) * x(1) - 2)) * ((5 - x(1)) * x(1) - 2))
    )
end FreudensteinRothFunction
