
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Jan 29 15:17:54 EST 2024
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Object to represent an example of a Quartic function for tests and
 *  benchmarks performed on function optimization and gradient descent classes.
 */

// Package definition.
package scalation
package optimization
package functions

// Project imports.
import scalation.mathstat.VectorD

// Object.
object QuarticExampleFunction extends BenchmarkFunction:
    // Public fields.
    val functionMinimum: VectorD = VectorD(1, 4)

    // Public methods.
    def objectiveFunction(x: VectorD): Double = x(0) ~^ 4 + (x(0) - 3.0) ~^ 2 + (x(1) - 4.0) ~^ 2 + 1.0

    // Overridden public methods.
    override def gradientFunction(x: VectorD): VectorD = VectorD(4.0 * x(0) ~^ 3 + 2 * x(0) - 6, 2 * x(1) - 8)
end QuarticExampleFunction
