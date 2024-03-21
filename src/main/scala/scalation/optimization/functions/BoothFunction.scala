
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Jan 22 15:40:12 EST 2024
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Object to represent the Booth function for tests and benchmarks performed on
 *  function optimization and gradient descent classes.
 */

// Package definition.
package scalation
package optimization
package functions

// Project imports.
import scalation.mathstat.VectorD

// Object.
object BoothFunction extends BenchmarkFunction:
    // Public fields.
    val functionMinimum: VectorD = VectorD(1, 3)

    // Public methods.
    def objectiveFunction(x: VectorD): Double = (x(0) + 2 * x(1) - 7) ~^ 2 + (2 * x(0) + x(1) - 5) ~^ 2

    // Overridden public methods.
    override def gradientFunction(x: VectorD): VectorD = VectorD(10 * x(0) + 8 * x(1) - 34, 8 * x(0) + 10 * x(1) - 38)
end BoothFunction
