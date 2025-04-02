
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat May 28 23:47:06 EDT 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: (Linear) Trend Model
 */

package scalation
package modeling
package forecasting_old

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TrendModel` class provides basic time series analysis capabilities for 
 *  TrendModel models.  TrendModel models are often used for forecasting.
 *  Given time series data stored in vector y, its next value y_t+1 = y(t+1)
 *  may be predicted based on a linear function of time t:
 *
 *      y_t+1 = b0 + b1 (t+1) + e_t+1
 *
 *  where b0 is the intercept, b1 is the slope and e_t+1 is the new residual/error term.
 *  @param y       the response vector (time-series data)
 *  @param tt      the time vector (time index may suffice)
 *  @param hparam  the hyper-parameters (none => use null)
 */
class TrendModel (y: VectorD, tt: VectorD = null, hparam: HyperParameter = null)
      extends Forecaster (y, tt, hparam)
         with Correlogram (y)
         with Fit (dfm = 1, df = y.dim - 1):

    private val debug = debugf ("TrendModel", true)                    // debug function
    private val flaw  = flawf ("TrendModel")                           // flaw function
                m     = y.dim                                          // number of time points (@see `FitM`)
    private var b     = VectorD.nullv                                  // parameter values from SimpleRegression

    modelName = s"TrendModel"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit a `TrendModel` model to the times-series data in vector y_.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the training/full response vector
     */
    def train (x_null: MatrixD, y_ : VectorD): Unit = 
        makeCorrelogram (y_)                                           // correlogram computes psi matrix

        val yy = y_(1 until y_.dim)
        val t_ = VectorD.range (1, y_.dim)
        b = SimpleRegression.coeff (t_, yy)                            // compute b = [b_0, b_1] 
        debug ("train", s"parameters for $modelName = $parameter")     // [b_0, b_1]
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test PREDICTIONS of a TrendModel forecasting model y_ = f(lags (y_)) + e
     *  and return its predictions and  QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train before test.
     *  @param x_null  the training/testing data/input matrix (ignored, pass null)
     *  @param y_      the training/testing/full response/output vector
     */
    override def test (x_null: MatrixD, y_ : VectorD): (VectorD, VectorD) =
        val (yp, no_qof) = super.test (null, y_)                        // call super.test for predictions
        resetDF (1, y_.dim - 1)                                         // reset the degrees of freedom
        (yp, diagnose (y_, yp))                                         // return predictions and QoF vector
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test FORECASTS of a TrendModel forecasting model y_ = f(lags (y_)) + e
     *  and return its forecasts and QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train and forecastAll before testF.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the training/testing/full response/output vector
     */
    def testF (h: Int, y_ : VectorD): (VectorD, VectorD, VectorD) =
        val (yy, yfh) = testSetupF (y_, h)                             // get and align actual and forecasted values
        resetDF (1, yy.dim - 1)                                        // reset the degrees of freedom
        println (s"testF: yy.dim = ${yy.dim}, yfh.dim = ${yfh.dim}")
//      Forecaster.differ (yy, yfh)                                    // uncomment for debugging
        (yy, yfh, diagnose (yy, yfh))                                  // return aligned actual, forecasted and QoF vectors
    end testF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameter vector for the Linear Trend model.
     */
    override def parameter: VectorD = b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t+1 using the 1-step ahead forecast.
     *      y_t+1 = b_0 + b_1 (t+1)
     *  Use `SimpleRegression` to determine the coefficients b_0 and b_1 and make predictions.
     *  @param t   the time point from which to make prediction
     *  @param y_  the actual values to use in making predictions
     */
    def predict (t: Int, y_ : VectorD): Double = b(0) + b(1) * (t+1)

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
            val pred = b(0) + b(1) * (t+k)
            yf(t+k, k) = pred                                          // forecast down the diagonal
            yd(k-1)    = pred                                          // record diagonal values
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

        yf(h1, h) = b(0) + b(1) * h1                                   // first forecast is special case

        for t <- y_.indices do                                         // make forecasts over all time points for horizon k
            yf(t+h, h) = b(0) + b(1) * (t+h)                           // forecast down the diagonal - SimpleRegression
        end for
        yf(?, h)                                                       // return the h-step ahead forecast vector
    end forecastAt

end TrendModel


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TrendModel` companion object provides factory methods for the `TrendModel` class.
 */
object TrendModel:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `TrendModel` object.
     *  @param y       the response vector (time series data)
     *  @param tt      the time vector, if relevant (time index may suffice)
     *  @param hparam  the hyper-parameters
     */
    def apply (y: VectorD, tt: VectorD = null, hparam: HyperParameter = null): TrendModel = 
        new TrendModel (y, tt, hparam)
    end apply

end TrendModel


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `trendModelTest` main function tests the `TrendModel` class on simulated data.
 *  Test predictions (one step ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.trendModelTest
 */
@main def trendModelTest (): Unit =

    val y = makeTSeries ()                                             // create simulated time-series (see `Stationary`)

    banner (s"Test Predictions: TrendModel on simulated time-series")
    val mod = new TrendModel (y)                                       // create model for time series data Trend Model
    mod.trainNtest ()()                                                // train and test on full dataset

    banner ("Select model based on ACF and PACF")
    mod.plotFunc (mod.acF, "ACF")                                      // Auto-Correlation Function (ACF)
    mod.plotFunc (mod.pacF, "PACF")                                    // Partial Auto-Correlation Function (PACF)

end trendModelTest

import forecasting.Example_LakeLevels.y

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `trendModelTest2` main function tests the `TrendModel` class on real data:
 *  Forecasting lake levels.
 *  Test predictions (one step ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.trendModelTest2
 */
@main def trendModelTest2 (): Unit =

    banner (s"Test Predictions: TrendModel on LakeLevels Dataset")
    val mod = new TrendModel (y)                                       // create model for time series data
    mod.trainNtest ()()                                                // train and test on full dataset

    banner ("Select model based on ACF and PACF")
    mod.plotFunc (mod.acF, "ACF")                                      // Auto-Correlation Function (ACF)
    mod.plotFunc (mod.pacF, "PACF")                                    // Partial Auto-Correlation Function (PACF)

end trendModelTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `trendModelTest3` main function tests the `TrendModel` class on real data:
 *  Forecasting lake levels.
 *  Test forecasts (1 to h steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.trendModelTest3
 */
@main def trendModelTest3 (): Unit =

    val hh = 3                                                         // maximum forecasting horizon

    banner (s"Test Forecasts: TrendModel on LakeLevels Dataset")
    val mod = new TrendModel (y)                                       // create model for time series data
    mod.trainNtest ()()                                                // train and test on full dataset

    mod.forecastAll (y, hh)                                            // forecast h-steps ahead (h = 1 to hh) for all y
    Forecaster.evalForecasts (mod, y, hh, true)

end trendModelTest3

