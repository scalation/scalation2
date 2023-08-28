
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Nov 27 15:17:28 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: SimpleRNN for Time Series
 *
 *  @see https://machinelearningmastery.com/understanding-simple-recurrent-neural-networks-in-keras/
 */

package scalation
package modeling
package forecasting

import scalation.mathstat.{MatrixD, VectorD}

import ActivationFun._
import neuralnet.Optimizer

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleRNN4TS` class supports regression-like recurrent neural networks
 *  for Time Series data.  Given a response vector y, a predictor matrix x is built
 *  that consists of lagged y vectors.
 *      y_t = f2 (b dot f(a dot x))
 *  where x = [y_{t-1}, y_{t-2}, ... y_{t-lag}].
 *  @param y       the original un-expanded output/response vector
 *  @param lag     the maximum lag included (inclusive)
 *  @param h       the forecasting horizon (1, 2, ... h)
 *  @param nz      the number of nodes/units in hidden layer (-1 => use default formula)
 *  @param hparam  the hyper-parameters (use Optimizer.hp for default)
 *  @param f       the activation function family for layers 1->2 (input to output)
 */
class SimpleRNN4TS (y: VectorD, lag: Int, h: Int, nz: Int = -1,
                    hparam: HyperParameter = Optimizer.hp,
                    f: AFF = f_tanh):

    private val debug   = debugf ("SimpleRNN4TS", true)                   // debug function
    private val flaw    = flawf ("SimpleRNN4TS")                          // flaw function
    private val MISSING = -0.0                                            // missing value

    // FIX - to be implemented

end SimpleRNN4TS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleRNN4TSTest` main function test the basic calculations for RNNs.
 *  @see https://machinelearningmastery.com/understanding-simple-recurrent-neural-networks-in-keras/
 *  > runMain scalation.modeling.forecasting.simpleRNN4TSTest
 */
@main def simpleRNN4TSTest () =

    val x = VectorD (1, 2, 3)                                             // the given time series: x0, x1, x2
    val m = x.dim                                                         // length of the time series (# time-steps)
    val n = 1                                                             // number of variables in time series
    val k = 2                                                             // number of hidden units

    // weight matrices
    val wx = MatrixD ((n, k),  0.18662322, -1.2369459)                    // input to hidden
    val wh = MatrixD ((k, k),  0.86981213, -0.49338293,                   // hidden to next hidden
                               0.49338293,  0.8698122)
    val wy = MatrixD ((k, n), -0.4635998,   0.6538409)                    // hidden to output

    // bias vectors
    val bh = VectorD (0.0, 0.0)                                           // bias at hidden layer
    val by = VectorD (0.0)                                                // bias at output layer

    val h0 = new VectorD (k)                                              // initial hidden state = k-vector of zeros
    val h1 = wx * x(0) + h0 + bh                                          // rest of the hidden states are 1-by-k matrices
    val h2 = wx * x(1) + h1 * wh + bh
    val h3 = wx * x(2) + h2 * wh + bh
    val y3 = h3 * wy + by                                                 // output = one-step ahead forecast
 
    println (s" h1 = $h1 \n h2 = $h2 \n h3 = $h3")

    println (s"Forecast from computation = $y3")                          // to improve, adjust weights via training
//  println (s"Forecast from network     = $y3_model")                    // FIX - to be implemented

end simpleRNN4TSTest

