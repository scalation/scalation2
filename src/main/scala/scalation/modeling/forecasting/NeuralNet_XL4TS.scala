
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_XL4TS` object supports X-layer regression-like neural networks 
 *  for Time Series data.  Given a response vector y, a predictor matrix x is built
 *  that consists of lagged y vectors.
 *      y_t = f2 (b dot f(a dot x))
 *  where x = [y_{t-1}, y_{t-2}, ... y_{t-lags}].
 */
object NeuralNet_XL4TS:

    private val debug   = debugf ("NeuralNet_XL4TS", true)                // debug function
    private val flaw    = flawf ("NeuralNet_XL4TS")                       // flaw function
    private val MISSING = -0.0                                            // missing value

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `NeuralNet_XL` object from a response vector.  The input/data matrix
     *  x is formed from the lagged y vectors as columns in matrix x.
     *  @param y       the original un-expanded output/response vector
     *  @param lags    the maximum lag included (inclusive)
     *  @param h       the forecasting horizon (1, 2, ... h)
     *  @param nz      the number of nodes in hidden layer (-1 => use default formula)
     *  @param hparam  the hyper-parameters (use Optimizer.hp for default)
     *  @param f       the array of activation function family for layers k->k+1
     */
    def apply (y: VectorD, lags: Int, h: Int, nz: Int = -1,
               hparam: HyperParameter = Optimizer.hp,
               f: Array [AFF] = Array (f_eLU, f_eLU, f_tanh)): NeuralNet_XL =
        var (x, yy) = buildMatrix4TS (y, lags, h)                         // column for each lag

        val mod = NeuralNet_XL.rescale (x, yy, null, null, hparam, f)
        mod.modelName = s"NeuralNet_XL4TS_$lags"
        mod
    end apply

end NeuralNet_XL4TS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `neuralNet_XL4TSTest` main function tests the `NeuralNet_XL4TS` class.
 *  This test is used to CHECK that the buildMatrix4TS function is working correctly.
 *  May get NaN for some maximum lags (p) due to multi-collinearity.
 *  > runMain scalation.modeling.forecasting.neuralNet_XL4TSTest
 */
@main def neuralNet_XL4TSTest (): Unit =

    val m = 30
    val y = VectorD.range (1, m)                                       // used to CHECK the buildMatrix4TS function
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


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `neuralNet_XL4TSTest4` main function tests the `NeuralNet_XL4TS` class on real data:
 *  Forecasting COVID-19 Weekly Data.
 *  > runMain scalation.modeling.forecasting.neuralNet_XL4TSTest4
 */
@main def neuralNet_XL4TSTest4 (): Unit =

    val LAGS = 10                                                           // number of lags
    val h    = 4                                                            // forecasting horizon

    val (x, y) = Example_Covid.loadData (Array ("new_cases", "hosp_patients", "icu_patients"), "new_deaths")

    println (s"x.dims = ${x.dims}, y.dim = ${y.dim}")

//  val f_ : Array [AFF] = Array (f_sigmoid, f_tanh, f_id)
//  Optimizer.hp ("eta") = 0.1
    
    banner ("Test NeuralNet_XL4TS on COVID-19 Weekly Data")
    val mod = NeuralNet_XL4TS (y, LAGS, h)  //, f = f_)                               // create model for time series data
//  val mod = NeuralNet_XL4TS.exo (y, LAGS, x(?, 0), x(?, 1), x(?, 2), h)(1, LAGS+1)  // create model for time series data

//  val (x_, y_, xx, yy) = NeuralNet_XL4TS.split_TnT (mod.getX, mod.getY)
//  val (yp, qof) = mod.trainNtest (x_, y_)(xx, yy)                             // train on (x_, y_) and test on (xx, yy)

    val (yp, qof) = mod.trainNtest ()()                                         // train on full and test on full
    val yy = y(LAGS until y.dim)
    new Plot (null, yy, yp(?, 0), s"${mod.modelName}, yy vs. yp", lines = true)

    banner (s"Feature Selection Technique: stepRegression")
    val (cols, rSq) = mod.stepRegressionAll (cross = false)                     // R^2, R^2 bar, sMAPE, NA
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "NA"),
               s"R^2 vs n for NeuralNet_XL4TS with tech", lines = true)
//  println (mod.summary ())

    banner ("Feature Importance")
    println (s"tech: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

end neuralNet_XL4TSTest4

