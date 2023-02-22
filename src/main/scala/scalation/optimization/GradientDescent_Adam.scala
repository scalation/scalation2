
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Mar  6 14:01:47 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   ADAptive Moment estimation (Adam) Optimizer
 *
 *  @see https://arxiv.org/pdf/1412.6980.pdf
 */

package scalation
package optimization

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GradientDescent_Adam` object defines the hyper-parameters for the optimizer.
 */
object GradientDescent_Adam:

    /** hyper-parameters for tuning the optimization algorithms - user tuning
     */
    val hp = new HyperParameter
    hp += ("eta", 0.01, 0.01)                                             // learning/convergence rate
    hp += ("maxEpochs", 400, 400)                                         // maximum number of epochs/iterations
    hp += ("beta", 0.9, 0.9)                                              // momentum decay hyper-parameter
    hp += ("beta2", 0.999, 0.999)                                         // momentum decay hyper-parameter
    hp += ("upLimit", 4, 4)                                               // up-limit hyper-parameter for stopping rule
    hp += ("eps", 1E-8, 1E-8)                                             // epsilon

end GradientDescent_Adam

import GradientDescent_Adam.hp

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GradientDescent_Adam` class provides functions to optimize the parameters
 *  (weights and biases) of Neural Networks with various numbers of layers.
 *  This optimizer implements an ADAptive Moment estimation (Adam) Optimizer.
 *  @see https://arxiv.org/pdf/1412.6980.pdf
 *  @param f       the vector-to-scalar (V2S) objective/loss function
 *  @param grad    the vector-to-vector (V2V) gradient function, grad f
 *  @param hparam  the hyper-parameters
 */
class GradientDescent_Adam (f: FunctionV2S, grad: FunctionV2V, hparam: HyperParameter = hp)
      extends Minimizer_NoLS
         with StoppingRule (hparam("upLimit").toInt):                   // limit on increasing loss

    private val debug = debugf ("GradientDescent_Adam", true)           // debug function
    private val flaw  = flawf ("GradientDescent_Adam")                  // flaw function

    private val η         = hp("eta").toDouble                          // set initial learning rate
    private val maxEpochs = hp("maxEpochs").toInt                       // maximum number of epochs
    private val β1        = hp("beta").toDouble                         // momentum hyper-parameter
    private val β2        = hp("beta2").toDouble                        // second momentum hyper-parameter
    private val upLimit   = hp("upLimit").toInt                         // limit on increasing loss
    private val eps       = hp("eps").toDouble                          // number close to zero

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the Non-Linear Programming (NLP) problem by starting at x0 and
     *  iteratively moving down in the search space to a minimal point.
     *  Return the optimal point/vector x and its objective function value.
     *  @see https://arxiv.org/pdf/1412.6980.pdf
     *  @param x0     the starting point 
     *  @param α      the step-size/learning rate
     *  @param toler  the tolerance
     */
    def solve (x0: VectorD, α: Double = η, toler: Double = eps): FuncVec =
        var p    = new VectorD (x0.dim)                                 // first moment of momentum
        var v    = new VectorD (x0.dim)                                 // second raw moment of momentum
        var ph   = VectorD.nullv                                        // bias-corrected first moment
        var vh   = VectorD.nullv                                        // bias-corrected second raw moment
        var x    = x0                                                   // start parameters at initial guess
        var f_x  = -0.0                                                 // loss function, value indefined
        var best = (f_x, x)                                             // start with best = initial

        var (go, t) = (true, 1)
        cfor (go && t <= maxEpochs, t += 1) {                           // iterate over each epoch/timestep
            val g = grad (x)                                            // get gradient of the loss function
            debug ("solve", s"for t = $t, grad (x) = $g, x = $x")
            p  = p * β1 + g * (1 - β1)                                  // update biased first moment
            v  = v * β2 + g~^2 * (1 - β2)                               // update biased second raw moment
            ph = p / (1 - β1~^t)                                        // compute bias-corrected first moment
            vh = v / (1 - β2~^t)                                        // compute bias-corrected second raw moment
//          x  -= ph * α                                                // update parameters (first moment only)
            x  -= (ph / (vh~^0.5 + eps)) * α                            // update parameters (both moments)
            f_x = f(x)                                                  // compute new loss function value
            debug ("solve", s"for t = $t, f(x) = $f_x, x = $x")

            best = stopWhen (f_x, x)
            if best._2 != null then go = false                          // early termination, return best
        } // cfor
        if go then getBest                                              // best solution found
        else best
    end solve

end GradientDescent_Adam


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `gradientDescent_AdamTest` main function is used to test the `GradientDescent_Adam`
 *  class.
 *      f(x) = (x_0 - 3)^2 + (x_1 - 4)^2 + 1
 *  > runMain scalation.optimization.gradientDescent_AdamTest
 */
@main def gradientDescent_AdamTest (): Unit =

    var x0    = VectorD (0.0, 0.0)                                      // starting point
    hp("eta") = 0.08                                                    // learning rate (problem dependent)

    banner ("Minimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
    def f (x: VectorD): Double = (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1
    def gr (x: VectorD): VectorD = VectorD (2 * x(0) - 6, 2 * x(1) - 8)

    val optimizer = new GradientDescent_Adam (f, gr)
    var opt = optimizer.solve (x0)
    println (s"][ optimal solution f(x), x) = $opt")

end gradientDescent_AdamTest

