
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Jan 23 10:46:44 EST 2024
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Object to represent the Cube function for tests and benchmarks performed on
 *  function optimization and gradient descent classes.
 */

// Package definition.
package scalation
package optimization
package functions

// Project imports.
import scalation.mathstat.VectorD

// Object.
object CubeFunction extends BenchmarkFunction:
    // Public fields.
    val functionMinimum: VectorD = VectorD(1, 1)

    // Public methods.
    def objectiveFunction(x: VectorD): Double = 100 * (x(1) - x(0) ~^ 3) ~^ 2 + (1 - x(0)) ~^ 2

    // Overridden public methods.
    override def gradientFunction(x: VectorD): VectorD = VectorD(-200 * (x(1) - x(0) ~^ 3) * (3 * x(0) ~^ 2) - 2 * (1 - x(0)), 200 * (x(1) - x(0) ~^ 3))
end CubeFunction
