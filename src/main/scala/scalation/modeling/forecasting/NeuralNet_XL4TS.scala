
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Feb 13 16:22:21 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: NeuralNet_XL for Time Series
 */

package scalation
package modeling
package forecasting

import scalation.mathstat._

import ActivationFun._
import neuralnet.{NeuralNet_XL, Optimizer}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_XL4TS` object supports X-layer regression-like neural networks 
 *  for Time Series data.  Given a response vector y, a predictor matrix x is built
 *  that consists of lagged y vectors.
 *      y_t = f2 (b dot f(a dot x))
 *  where x = [y_{t-1}, y_{t-2}, ... y_{t-lag}].
 */
object NeuralNet_XL4TS:

    private val debug   = debugf ("NeuralNet_XL4TS", true)                // debug function
    private val flaw    = flawf ("NeuralNet_XL4TS")                       // flaw function
    private val MISSING = -0.0                                            // missing value

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `NeuralNet_XL` object from a response vector.  The input/data matrix
     *  x is formed from the lagged y vectors as columns in matrix x.
     *  @param y       the original un-expanded output/response vector
     *  @param lag     the maximum lag included (inclusive)
     *  @param h       the forecasting horizon (1, 2, ... h)
     *  @param nz      the number of nodes in hidden layer (-1 => use default formula)
     *  @param hparam  the hyper-parameters (use Optimizer.hp for default)
     *  @param f       the array of activation function family for layers k->k+1
     */
    def apply (y: VectorD, lag: Int, h: Int, nz: Int = -1,
               hparam: HyperParameter = Optimizer.hp,
               f: Array [AFF] = Array (f_eLU, f_eLU, f_tanh)): NeuralNet_XL =
        var (x, yy) = RegressionMV4TS.buildMatrix (y, lag, h)             // column for each lag

        val mod = NeuralNet_XL.rescale (x, yy, null, null, hparam, f)
        mod.modelName = s"NeuralNet_XL4TS_$lag"
        mod
    end apply

end NeuralNet_XL4TS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `neuralNet_XL4TSTest` main function tests the `NeuralNet_XL4TS` class.
 *  This test is used to CHECK that the buildMatrix function is working correctly.
 *  May get NaN for some maximum lags (p) due to multi-collinearity.
 *  > runMain scalation.modeling.forecasting.neuralNet_XL4TSTest
 */
@main def neuralNet_XL4TSTest (): Unit =

    val m = 30
    val y = VectorD.range (1, m)                                       // used to CHECK the buildMatrix function
    val h = 3                                                          // the forecasting horizon

    for p <- 5 to 5 do                                                 // autoregressive hyper-parameter p
        banner (s"Test: NeuralNet_XL4TS with $p lags")
        val mod = NeuralNet_XL4TS (y, p, h)                            // create model for time series data
        mod.trainNtest2 ()()                                           // train the model on full dataset
        println (mod.summary)

        val yy = mod.getY
        val yp = mod.predict (mod.getX)
        for j <- yp.indices2 do
            new Plot (null, yy(?, j), yp(?, j), s"yy_$j vs. yp_$j for ${mod.modelName} with $p lags", lines = true)
        end for
    end for

end neuralNet_XL4TSTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `neuralNet_XL4TSTest2` main function tests the `NeuralNet_XL4TS` class on real data:
 *  Forecasting lake levels.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.neuralNet_XL4TSTest2
 */
@main def neuralNet_XL4TSTest2 (): Unit =

    import Example_LakeLevels.y
    val m = y.dim
    val h = 3                                                          // the forecasting horizon

    for p <- 1 to 10 do                                                // autoregressive hyper-parameter p
        banner (s"Test: NeuralNet_XL4TS with $p lags")
        val mod = NeuralNet_XL4TS (y, p, h)                            // create model for time series data
        mod.trainNtest ()()                                            // train the model on full dataset
        println (mod.summary)

        banner ("Predictions/Forecasts")                               // direct forecasting technique
        val yy = mod.getY
        val yf = mod.predict (mod.getX)
        for k <- yf.indices2 do
            new Plot (null, yy(?, k), yf(?, k), s"yy_$k vs. yf_$k for ${mod.modelName} with $p lags", lines = true)
        end for
        println (s"yf = $yf")
        println (s"yf.dims = ${yf.dims}")
    end for

end neuralNet_XL4TSTest2

