
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Mar  6 14:01:47 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Gradient Descent with Momentum Optimizer
 *
 *  @see https://arxiv.org/pdf/1412.6980.pdf
 */

package scalation
package optimization

import scalation.mathstat._

import Minimize.hp

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GradientDescent_Mo` class provides functions to optimize the parameters (weights
 *  and biases) of Neural Networks with various numbers of layers.
 *  This optimizer implements a Gradient Descent with Momentum Optimizer.
 *  @see https://arxiv.org/pdf/1412.6980.pdf
 *  @param f       the vector-to-scalar (V2S) objective/loss function
 *  @param grad    the vector-to-vector (V2V) gradient function ∇f
 *  @param hparam  the hyper-parameters
 */
class GradientDescent_Mo (f: FunctionV2S, grad: FunctionV2V, hparam: HyperParameter = hp)
      extends Minimize
         with StoppingRule (hparam("upLimit").toInt):                   // limit on increasing loss

    private val debug = debugf ("GradientDescent_Mo", true)             // debug function
    private val flaw  = flawf ("GradientDescent_Mo")                    // flaw function

    private val β     = hparam("beta").toDouble                         // momentum hyper-parameter
    private val ν     = hparam("nu").toDouble                           // 0 => SGD, 1 => (normalized) SHB

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the Non-Linear Programming (NLP) problem by starting at x0 and
     *  iteratively moving down in the search space to a minimal point.
     *  Return the optimal point/vector x and its objective function value.
     *  @see https://arxiv.org/pdf/1412.6980.pdf
     *  @param x0     the starting point 
     *  @param α      the step-size/learning rate
     */
    def solve (x0: VectorD, α: Double = eta): FuncVec =
        var p    = new VectorD (x0.dim)                                 // momentum-based aggregated gradient
        var x    = x0                                                   // start parameters at initial guess
        var f_x  = -0.0                                                 // loss function, value indefined
        var best = (f_x, x)                                             // start with best = initial

        var (go, it) = (true, 1)                                        
        cfor (go && it <= MAX_IT, it += 1) {                            // iterate over each epoch/timestep
            val g = grad (x)                                            // get gradient of loss function
            debug ("solve", s"for it = $it, grad (x) = $g, x = $x")
            p   = g * (1 - β) + p * β                                   // update momentum-based aggregated gradient
            x  -= (g * (1 - ν) + p * ν) * α                             // update parameters
            f_x = f(x)                                                  // compute new loss function value
            debug ("solve", s"for it = $it, f(x) = $f_x, x = $x")

            best = stopWhen (f_x, x)
            if best._2 != null then go = false                          // early termination, return best
        } // cfor
        if go then getBest                                              // best solution found
        else best
    end solve

end GradientDescent_Mo


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `gradientDescent_MoTest` main function is used to test the `GradientDescent_Mo`
 *  class.
 *      f(x) = (x_0 - 3)^2 + (x_1 - 4)^2 + 1
 *  > runMain scalation.optimization.gradientDescent_MoTest
 */
@main def gradientDescent_MoTest (): Unit =

    var x0    = VectorD (0.0, 0.0)                                      // starting point
    hp("eta") = 1.0                                                     // learning rate (problem dependent)

    banner ("Minimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f (x: VectorD): Double = (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1
    def gr (x: VectorD): VectorD = VectorD (2 * x(0) - 6, 2 * x(1) - 8)

    val optimizer = new GradientDescent_Mo (f, gr)
    var opt = optimizer.solve (x0)
    println (s"][ optimal solution f(x), x) = $opt")

end gradientDescent_MoTest

