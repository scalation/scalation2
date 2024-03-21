
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Jan 22 16:03:29 EST 2024
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Object to represent the Bohachevsky2 function for tests and benchmarks
 *  performed on function optimization and gradient descent classes.
 */

// Package definition.
package scalation
package optimization
package functions

// Project imports.
import scalation.mathstat.VectorD

// Object.
object Bohachevsky2Function extends BenchmarkFunction:
    // Public fields.
    val functionMinimum: VectorD = VectorD(0, 0)

    // Public methods.
    def objectiveFunction(x: VectorD): Double = x(0) ~^ 2 + 2 * x(1) ~^ 2 - 0.3 * math.cos(3 * math.Pi * x(0)) * math.cos(4 * math.Pi * x(1)) + 0.3

    // Overridden public methods.
    override def gradientFunction(x: VectorD): VectorD = VectorD(
        2 * x(0) + 0.3 * 3 * math.Pi * math.sin(3 * math.Pi * x(0)) * math.cos(4 * math.Pi * x(1)),
        4 * x(1) - 0.3 * 4 * math.Pi * math.cos(3 * math.Pi * x(0)) * math.sin(4 * math.Pi * x(1))
    )
end Bohachevsky2Function
