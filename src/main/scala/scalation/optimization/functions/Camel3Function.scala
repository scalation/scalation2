
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Jan 23 10:45:34 EST 2024
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Object to represent the Camel3 function for tests and benchmarks performed
 *  on function optimization and gradient descent classes.
 */

// Package definition.
package scalation
package optimization
package functions

// Project imports.
import scalation.mathstat.VectorD

// Object.
object Camel3Function extends BenchmarkFunction:
    // Public fields.
    val functionMinimum: VectorD = VectorD(0, 0)

    // Public methods.
    def objectiveFunction(x: VectorD): Double = 2 * x(0) ~^ 2 - 1.05 * x(0) ~^ 4 + (1 / 6.0) * x(0) ~^ 6 + x(0) * x(1) + x(1) ~^ 2

    // Overridden public methods.
    override def gradientFunction(x: VectorD): VectorD = VectorD(4 * x(0) - 4.2 * x(0) ~^ 3 + x(0) ~^ 5 + x(1), x(0) + 2 * x(1))
end Camel3Function
