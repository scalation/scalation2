
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Jan 22 15:33:02 EST 2024
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Object to represent the McCormick function for tests and benchmarks
 *  performed on function optimization and gradient descent classes.
 */

// Package definition.
package scalation
package optimization
package functions

// Project imports.
import scalation.mathstat.VectorD

// Object.
object McCormickFunction extends BenchmarkFunction:
    // Public fields.
    val functionMinimum: VectorD = VectorD(-0.54719, -1.54719)

    // Public methods.
    def objectiveFunction(x: VectorD): Double = math.sin(x(0) + x(1)) + (x(0) - x(1)) ~^ 2 - 1.5 * x(0) + 2.5 * x(1) + 1

    // Overridden public methods.
    override def gradientFunction(x: VectorD): VectorD = VectorD(-1.5 + 2 * x(0) - 2 * x(1) + math.cos(x(0) + x(1)), 2.5 - 2 * x(0) + 2 * x(1) + math.cos(x(0) + x(1)))
end McCormickFunction
