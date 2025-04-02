
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Jun 13 01:27:00 EST 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Random Walk (guess previous value)
 *           Also known as the Naive Model
 */

package scalation
package modeling
package forecasting_old

import scalation.mathstat._

import scalation.scala2d.writeImage

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomWalk` class provides basic time series analysis capabilities for 
 *  RandomWalk models.  RandomWalk models are often used for forecasting.
 *  Given time series data stored in vector y, its next value y_t+1 = y(t+1)
 *  may be predicted based on its past value of y:
 *
 *      y_t+1 = y_t + e_t+1
 *
 *  where y_t is the previous value of y and e_t+1 is the new residual/error term.
 *  @param y       the response vector (time series data)
 *  @param tt      the time vector, if relevant (time index may suffice)
 *  @param hparam  the hyper-parameters (none => use null)
 */
class RandomWalk (y: VectorD, tt: VectorD = null, hparam: HyperParameter = null)
      extends Forecaster (y, tt, hparam)
         with Correlogram (y)
         with Fit (dfm = 1, df = y.dim - 1):

    private val debug = debugf ("RandomWalk", true)                    // debug function
    private val flaw  = flawf ("RandomWalk")                           // flaw function

    modelName = s"RandomWalk"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit a `RandomWalk` model to the times-series data in vector y_.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the training/full response vector
     */
    def train (x_null: MatrixD, y_ : VectorD): Unit = 
        m = y_.dim                                                     // length of relevant time-series
        makeCorrelogram (y_)                                           // correlogram computes psi matrix
        debug ("train", s"parameters for $modelName = $parameter")     // []
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test PREDICTIONS of a RandomWalk forecasting model y_ = f(lags (y_)) + e
     *  and return its predictions and  QoF vector.  Testing may be in-sample
     *  (on the full set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train before test.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the testing/full response/output vector
     */
    override def test (x_null: MatrixD, y_ : VectorD): (VectorD, VectorD) =
        val (yp, no_qof) = super.test (null, y_)                        // call super.test for predictions
        resetDF (1, y_.dim - 1)                                     // reset the degrees of freedom
        (yp, diagnose (y_, yp))                                         // return predictions and QoF vector
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test FORECASTS of a RandomWalk forecasting model y_ = f(lags (y_)) + e
     *  and return its forecasts and QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train and forecastAll before testF.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the testing/full response/output vector
     */
    def testF (h: Int, y_ : VectorD): (VectorD, VectorD, VectorD) =
        val (yy, yfh) = testSetupF (y_, h)                             // get and align actual and forecasted values
        resetDF (1, yy.dim - 1)                                        // reset the degrees of freedom
        debug ("testF", s" for h = $h: yy.dim = ${yy.dim}, yfh.dim = ${yfh.dim}")
//      Forecaster.differ (yy, yfh)                                    // uncomment for debugging
        (yy, yfh, diagnose (yy, yfh))                                  // return actual, forecasted and QoF vectors
    end testF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t+1 using the 1-step ahead forecast.
     *
     *      y_t+1 = y_t
     *
     *  @param t   the time point from which to make prediction
     *  @param y_  the actual values to use in making predictions
     */
    def predict (t: Int, y_ : VectorD): Double = y_(t)                 // predict using the prior value

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a vector of size h, of 1 through h-steps ahead forecasts for the model.
     *      forecast the following time points:  t+1, ..., t-1+h.
     *  Note, must create the yf matrix before calling the forecast method.
     *  Intended to work with rolling validation (analog of predict method)
     *  @param t   the time point from which to make forecasts
     *  @param yf  the forecast matrix (time x horizons)
     *  @param y_  the actual values to use in making predictions
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecast (t: Int, yf: MatrixD, y_ : VectorD, h: Int): VectorD =
        if h < 1 then flaw ("forecast", s"horizon h = $h must be at least 1")

        val yd = new VectorD (h)                                       // hold forecasts for each horizon
        for k <- 1 to h do
            yf(t+k, k) = y_(t)                                         // forecast down the diagonal
            yd(k-1)    = y_(t)                                         // record diagonal values
        end for
        yd                                                             // return forecasts for each horizon
    end forecast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points at horizon h (h-steps ahead).
     *  Assign to FORECAST MATRIX and return h-step ahead forecast.
     *  Note, `predictAll` provides predictions for h = 1.
     *  @see `forecastAll` method in `Forecaster` trait.
     *  @param yf  the forecast matrix (time x horizons)
     *  @param y_  the actual values to use in making forecasts
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecastAt (yf: MatrixD, y_ : VectorD, h: Int): VectorD =
        if h < 2 then flaw ("forecastAt", s"horizon h = $h must be at least 2")
        val h1 = h - 1 

        yf(h1, h) = yf(h1-1, h1)                                       // first forecast is special case

        for t <- y_.indices do                                         // make forecasts over all time points for horizon k
            yf(t+h, h) = yf(t+h-1, h-1)                                // forecast down the diagonal - previous value
        end for
        yf(?, h)                                                       // return the h-step ahead forecast vector
    end forecastAt

end RandomWalk


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomWalk` companion object provides factory methods for the `RandomWalk` class.
 */
object RandomWalk:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `RandomWalk` object.
     *  @param y       the response vector (time series data)
     *  @param tt      the time vector, if relevant (time index may suffice)
     *  @param hparam  the hyper-parameters
     */
    def apply (y: VectorD, tt: VectorD = null, hparam: HyperParameter = null): RandomWalk = 
        new RandomWalk (y, tt, hparam)
    end apply

end RandomWalk


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `randomWalkTest` main function tests the `RandomWalk` class on simulated data.
 *  Test predictions (one step ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRTest
 */
@main def randomWalkTest (): Unit =

    val y = makeTSeries ()                                             // create simulated time-series (see `Stationary`)

    banner (s"Test Predictions: Random Walk on simulated time-series")
    val mod = new RandomWalk (y)                                       // create model for time series data Random Walk Model
    mod.trainNtest ()()                                                // train and test on full dataset

    banner ("Select model based on ACF and PACF")
    mod.plotFunc (mod.acF, "ACF")                                      // Auto-Correlation Function (ACF)
    mod.plotFunc (mod.pacF, "PACF")                                    // Partial Auto-Correlation Function (PACF)

end randomWalkTest

import forecasting.Example_LakeLevels.y

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `randomWalkTest2` main function tests the `RandomWalk2` class on real data:
 *  Forecasting lake levels.  It compares with the other baselines: `NullModel` and `TrendModel`.
 *  Test predictions (one step ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.randomWalkTest2
 */
@main def randomWalkTest2 (): Unit =

    banner (s"Test Predictions: RandomWalk on LakeLevels Dataset")
    val mod = new RandomWalk (y)                                       // create a Random Walk Model
    val (yp, qof) = mod.trainNtest ()()                                // train and test on full dataset

    banner (s"Test Predictions: NullModel on LakeLevels Dataset")
    val nm = new NullModel (y)                                         // create a Null Model
    val (yp2, qof2) = nm.trainNtest ()()                               // train and test on full dataset

    banner (s"Test Predictions: TrendModel on LakeLevels Dataset")
    val tm = new TrendModel (y)                                        // create a Trend Model
    val (yp3, qof3) = tm.trainNtest ()()                               // train and test on full dataset

    val yy = y(1 until y.dim)
    val plot = new PlotM (null, MatrixD (yy, yp, yp2, yp3), Array ("y", "y-RW", "y-NM", "y-TM"),
                          "Compare Baseline Models", lines = true)

    writeImage (DATA_DIR + "plot_ts_baseline.png", plot)

    banner ("Select model based on ACF and PACF")
    mod.plotFunc (mod.acF, "ACF")                                      // Auto-Correlation Function (ACF)
    mod.plotFunc (mod.pacF, "PACF")                                    // Partial Auto-Correlation Function (PACF)

end randomWalkTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `randomWalkTest3` main function tests the `RandomWalk` class on real data:
 *  Forecasting lake levels.
 *  Test forecasts (1 to h steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.randomWalkTest3
 */
@main def randomWalkTest3 (): Unit =

    val hh = 3                                                         // maximum forecasting horizon

    banner (s"Test Forecasts: RandomWalk on LakeLevels Dataset")
    val mod = new RandomWalk (y)                                       // create model for time series data
    mod.trainNtest ()()                                                // train and test on full dataset

    mod.forecastAll (y, hh)                                            // forecast h-steps ahead (h = 1 to hh) for all y
    Forecaster.evalForecasts (mod, y, hh, true)

end randomWalkTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `randomWalkTest4` main function tests the `RandomWalk` class on real data:
 *  Forecasting COVID-19.  Test forecasts (1 to h steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.randomWalkTest4
 */
@main def randomWalkTest4 (): Unit =

    val exo_vars = Array.ofDim [String] (0)                            // no exogenous variables in this case
    val (xx, yy) = forecasting.Example_Covid.loadData (exo_vars, "new_deaths")
    val iskip    = yy.indexWhere (_ >= 6.0)                            // find day with at least 6 deaths
    println (s"iskip = $iskip is first week with at least 6 deaths")

    val rat = 0.5                                                      // set train-test ratio
    val cut = (rat * yy.dim).toInt
    val y   = yy(cut until yy.dim)                                     // throw away the training part (no training for RW)

    val hh = 4                                                         // forecasting horizon (weeks 1 to 4)
 
    banner (s"Test Forecasts: RandomWalk on COVID-19 Dataset")
    val mod = new RandomWalk (y)                                       // create model for time series data
    mod.trainNtest ()()                                                // train and test on full dataset

    mod.forecastAll (y, hh)                                            // forecast h-steps ahead (h = 1 to hh) for all y
    Forecaster.evalForecasts (mod, y, hh, true)

end randomWalkTest4

