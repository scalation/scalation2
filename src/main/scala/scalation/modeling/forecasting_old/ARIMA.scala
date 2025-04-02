
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Jun 13 01:27:00 EST 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Auto-Regressive, Integrated, Moving Average (ARIMA)
 *
 *  @see http://en.wikipedia.org/wiki/Autoregressive%E2%80%93moving-average_model
 *  @see http://www.emu.edu.tr/mbalcilar/teaching2007/econ604/lecture_notes.htm
 *  @see http://www.stat.berkeley.edu/~bartlett/courses/153-fall2010
 *  @see http://www.princeton.edu/~apapanic/ORFE_405,_Financial_Time_Series_%28Fall_2011%29_files/slides12-13.pdf
 */
 
package scalation
package modeling
package forecasting_old

import scalation.mathstat._

import ARIMA_diff._
import Forecaster.differ

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARIMA` class provides basic time series analysis capabilities for Auto-
 *  Regressive 'AR' Integrated 'I' Moving-Average 'MA' models.  In an
 *  ARIMA(p, d, q) model, p and q refer to the order of the Auto-Regressive
 *  and Moving-Average components of the model; d refers to the order of
 *  differencing.  Given time series data stored in vector y, its next value y_t = y(t)
 *  may be predicted based on prior values of y and its noise:
 *      y_t = δ + Σ(φ_i y_t-i) + Σ(θ_i e_t-i) + e_t
 *  where δ is a constant, φ is the auto-regressive coefficient vector,
 *  θ is the moving-average coefficient vector, and e is the noise vector.
 *------------------------------------------------------------------------------
 *  If d > 0, then the time series must be differenced first before applying
 *  the above model.
 *------------------------------------------------------------------------------
 *  @param y       the original input vector (time series data)
 *  @param tt      the time vector, if relevant (time index may suffice)
 *  @param hparam  the hyper-parameters
 */
class ARIMA (y: VectorD, tt: VectorD = null, hparam: HyperParameter = SARIMA.hp)
      extends Forecaster (y, tt, hparam)
         with Fit (dfm = hparam("p").toDouble, df = y.dim - pq (hparam)):

    private val debug  = debugf ("ARIMA", true)                        // debug function
    private val flaw   = flawf ("ARIMA")                               // flaw function
    private val p      = hparam("p").toInt                             // p-th order Auto-Regressive model
    private val d      = hparam("d").toInt                             // the number of differences to take
    private val q      = hparam("q").toInt                             // q-th order Moving-Average model
    private val v      = diff (y, d)                                   // d-th difference of the full time-series
            val arma   = new ARMA (v, tt, hparam)                      // delegate to the `ARMA` class

    if d out (0, 3) then flaw ("init", s"difference d = $d must be in (0, 3)")

    arma.modelName = s"ARIMA($p, $d, $q)"                              // rename delegate ARMA to match
    modelName = arma.modelName                                         // use same name for ARIMA

    debug ("init", s"$modelName")

    new Plot (null, y, null, s"Plot $modelName: y vs. t", lines = true)
    if d > 0 then new Plot (null, v, null, s"Plot $modelName: v = diff (y, d) vs. t", lines = true)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Pick one of the following vectors: v full first difference, u differenced, or u itself.
     *  @param u  the input time-series vector
     */
    def pick (u: VectorD): VectorD =
        if u == y then v                                               // passed in original full time-series
        else if d > 0 then diff (u, d)                                 // sub-series differenced
        else u                                                         // sub-series as is
    end pick

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `ARIMA` model to the times-series data in vector y_.
     *  Estimate the coefficient vectors φ and θ for (p, q)-th order ARIMA(p, q) model.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the training/full response vector (e.g., full y)
     */
    override def train (x_null: MatrixD, y_ : VectorD): Unit =
        arma.train (x_null, pick (y_))
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test PREDICTIONS of an ARIMA forecasting model y_ = f(lags (y_)) + e
     *  and return its predictions and  QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train before test.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the training/testing/full response/output vector
     */
    override def test (x_null: MatrixD, y_ : VectorD): (VectorD, VectorD) =
        arma.test (x_null, pick (y_))
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train and test the forecasting model y_ = f(y-past) + e and report its QoF
     *  and plot its predictions.  Return the predictions and QoF.
     *  @param y_  the training/full response/output vector (defaults to full y)
     *  @param yy  the testing/full response/output vector (defaults to full y)
     */
    override def trainNtest (y_ : VectorD = y)(yy: VectorD = y): (VectorD, VectorD) =
        arma.trainNtest (pick (y_))(pick (yy))
    end trainNtest

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test FORECASTS of an ARIMA forecasting model y_ = f(lags (y_)) + e
     *  and return its forecasts and QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train and forecastAll before testF.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the training/testing/full response/output vector
     */
    def testF (h: Int, y_ : VectorD): (VectorD, VectorD, VectorD) =
        arma.testF (h, pick (y_))                                  // return aligned actual, forecasted and qof vectors
    end testF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameter vector for the ARIMA(p, q) model.
     */
    override def parameter: VectorD = arma.parameter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t+1 using the 1-step ahead forecast.
     *      y_t+1 = f (y_t, ...)
     *  @param t   the time point from which to make prediction
     *  @param y_  the actual values to use in making predictions
     */
    def predict (t: Int, y_ : VectorD): Double =
        arma.predict (t, pick (y_))
    end predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict all values corresponding to the given vector y_.
     *  @param y_  the actual values to use in making predictions
     */
//  override def predictAll (y_ : VectorD): VectorD = arma.predictAll (pick (y_))

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
        arma.forecast (t, yf, pick (y_), h)
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
        arma.forecastAt (yf, pick (y_), h)
    end forecastAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points and all horizons (1 through h-steps ahead).
     *  Record these in the FORECAST MATRIX yf, where
     *      yf(t, k) = k-steps ahead forecast for y_t
     *  Note, column 0, yf(?, 0), is set to y (the actual time-series values).
     *     last column, yf(?, h+1), is set to t (the time values, for reference).
     *  Forecast recursively down diagonals in the yf forecast matrix.
     *  The top right and bottom left triangles in yf matrix are not forecastable.
     *  @param y_  the actual values to use in making forecasts
     *  @param h   the maximum forecasting horizon, number of steps ahead to produce forecasts
     */
    override def forecastAll (y_ : VectorD, h: Int): MatrixD =
        debug ("forecastAll", s"y_.dim = ${y_.dim}, e.dim = ${e.dim}")
        arma.forecastAll (pick (y_), h)
    end forecastAll

    ////////////////////////////////////////////////////////////////////////////////
    // Make predictions/forecasts on the original scale time-series (not differenced).

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t+1 using the 1-step ahead forecast.
     *      y_t+1 = f (y_t, ...) + e_t+1
     *  @param t   the time point from which to make prediction
     *  @param y_  the actual values to use in making predictions
     */
    def predict2 (t: Int, y_ : VectorD): Double =
        arma.predict (t, pick (y_)) + (if d > 0 then y_(t) else 0.0)   // FIX - surely it's incorrect?
    end predict2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict all values corresponding to the given vector y_.
     *  @param y_  the actual values to use in making predictions
     */
    def predictAll2 (y_ : VectorD, show: Boolean = true): VectorD =
        val yp = new VectorD (y_.dim)
        yp(0) = y_(0)
        for t <- 0 until y_.dim-1 do yp(t+1) = predict2 (t, y_)
        if show then
//          println (FitM.fitMap (diagnose (y_, yp), qoF_names))
            println (s"nparams = $nparams")
            resetDF (nparams - 1, y_.dim - nparams)
            println (report (diagnose (y_, yp)))                       // report on Quality of Fit (QoF)
            println (s"mase = ${Fit.mase (y, yp)}")                    // Means Absolute Scaled Error
            new Plot (null, y_, yp, "Plot y, yp vs. t", lines = true)
        end if
        yp
    end predictAll2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points at horizon h (h-steps ahead).
     *  Assign to FORECAST MATRIX and return h-step ahead forecast.
     *  @param yf  the forecast matrix (time x horizons)
     *  @param y_  the actual values to use in making forecasts
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *
    def forecastAt2 (yf: MatrixD, y_ : VectorD, h: Int): VectorD =
        val yfh = arma.forecastAt (yf, pick (y_), h)
        if d > 0 then yfh + y_ else yfh
    end forecastAt2
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points and all horizons (1 through h-steps ahead).
     *  Record these in the yf matrix, where
     *      yf(t, k) = k-steps ahead forecast for y_t
     *  Note, column 0, yf(?, 0), is set to y (the actual time-series values).
     *  Forecast recursively down diagonals in the yf forecast matrix.
     *  The top right and bottom left triangles in yf matrix are not forecastable.
     *  @param y_  the actual values to use in making forecasts
     *  @param h   the maximum forecasting horizon, number of steps ahead to produce forecasts
     *
    def forecastAll2 (y_ : VectorD, h: Int): MatrixD =
        debug ("forecastAll2", s"y_.dim = ${y_.dim}, e.dim = ${e.dim}")
        yf = new MatrixD (y_.dim+h, h+2)                               // forecasts for all time points t & horizons to h
        for t <- y_.indices do yf(t, 0) = y_(t)                        // first column is the timestep (e.g., logical day)
        for k <- 1 to h do forecastAt2 (yf, y_, k)                     // forecast k-steps into the future
        for t <- yf.indices do yf(t, h+1) = t                          // last column is time (logical day)
        yf                                                             // return matrix of forecasted values
    end forecastAll2
     */

end ARIMA


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRIMATest` main function tests the `ARIMA` class on simulated data.
 *  Test predictions (one step ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRIMATest
 */
@main def aRIMATest (): Unit =

    import SARIMA.hp

    println (s"hp = $hp")

    val y = makeTSeries ()                                             // create simulated time-series (see `Stationary`)

    banner (s"Test Predictions: ARIMA(1, 1) on simulated time-series")
    var mod = new ARIMA (y)                                            // create model for time-series data ARIMA(1, 1)
    mod.trainNtest ()()                                                // train and test on full dataset

    banner (s"Test Predictions: ARIMA(1, 0) on simulated time-series")
    hp("q") = 0
    mod = new ARIMA (y)                                                // create model for time-series data ARIMA(1, 0)
    mod.trainNtest ()()      

    banner ("Select model based on ACF and PACF")
    mod.arma.plotFunc (mod.arma.acF, "ACF")                            // Auto-Correlation Function (ACF)
    mod.arma.plotFunc (mod.arma.pacF, "PACF")                          // Partial Auto-Correlation Function (PACF)

end aRIMATest

import forecasting.Example_LakeLevels.y

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRIMATest2` main function tests the `ARIMA` class on real data:  Forecasting lake levels.
 *  Test predictions (one step ahead forecasts) with no differencing
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRIMATest2
 */
@main def aRIMATest2 (): Unit =

    import SARIMA.hp

    hp("d") = 0            // (no differencing) => should give same results as ARMA (@see `aRMATest2`)
    println (s"hp = $hp")

    for p <- 1 to 5; q <- 0 to 2 do
        hp("p") = p; hp("q") = q                                       // set p (AR) and q (MA) hyper-parameters
        val mod = new ARIMA (y)                                        // create model for time-series data ARIMA(p, q)
        banner (s"Test Predictions: ${mod.modelName} on LakeLevels Dataset")
        mod.trainNtest ()()                                            // trainb and test the model on full dataset

        val yp = mod.predictAll2 (y)                                   // results on original scale
        new Plot (null, y, yp, s"Plot: ${mod.modelName} predictAll2: y, yp vs t", lines = true)
    end for

end aRIMATest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRIMATest3` main function tests the `ARIMA` class on real data:  Forecasting lake levels.
 *  Test predictions (one step ahead forecasts) taking one difference.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRIMATest3
 */
@main def aRIMATest3 (): Unit =

    import SARIMA.hp

    val d = 1
    hp("d") = d            // (first differencing) => should give same results as ARIMA (@see `aR1MATest3`)

    val v = diff (y, d)                                                // take the first difference of time-series y
    differ (y, backform (v, y))                                        // verify recovery of original times-series

    for p <- 1 to 5; q <- 0 to 2 do
        hp("p") = p; hp("q") = q                                       // set p (AR) and q (MA) hyper-parameters
        val mod = new ARIMA (y)                                        // create model for time-series data ARIMA(p, q)
        banner (s"Test Predictions: ${mod.modelName} on LakeLevels Dataset")
        val (vp, qof) = mod.trainNtest ()()                            // test and test the model on full dataset
        val yp = mod.predictAll2 (y)                                   // results on original scale

        new Plot (null, y, yp, s"Plot: ${mod.modelName} predictAll2: y, yp vs t", lines = true)
    end for

end aRIMATest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRIMATest4` main function tests the `ARIMA` class on real data:  Forecasting lake levels.
 *  Test forecasts (1 to h steps ahead forecasts) for several values of p and q.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  @see `aRMATest5`
 *  > runMain scalation.modeling.forecasting.aRIMATest4
 */
@main def aRIMATest4 (): Unit =

    import SARIMA.hp

    val hh = 2                                                        // maximum forecasting horizon

    val v = Δ (y)                                                     // velocity series (first differences)

    for p <- 2 to 2; q <- 0 to 0 do
        hp("p") = p; hp("q") = q                                      // set p (AR) and q (MA) hyper-parameters
        val mod = new ARIMA (y)                                       // create model for time series data
        banner (s"Test: ${mod.modelName} on LakeLevels Dataset")
        mod.trainNtest ()()                                           // train and test the model on full dataset

        val yf_ = mod.forecastAll (y, hh)                             // forecast using differenced values h-steps ahead for all y
        val yf  = transformBack (yf_, y, hh)                          // transform back to original scale
        println (s"yf_ = $yf_")                                       // forecast matrix on differenced values
        println (s"yf  = $yf")                                        // forecast matrix on original scale
        println (s"y   = $y")                                         // observed values on original scale

        val tf = new TestFit (y.dim)
        val vh1 = yf_(?, 1)                                           // test on differenced scale
        val vh2 = yf_(?, 2)
        println (tf.testDiagnose (v, vh1))
        println (tf.testDiagnose (v, vh2))
        new Plot (null, v, vh1, "v, vh1 vs. t", lines = true)
        new Plot (null, v, vh2, "v, vh2 vs. t", lines = true)

        val yh1 = yf(?, 1)                                            // test on original scale
        val yh2 = yf(?, 2)
        println (tf.testDiagnose (y, yh1))
        println (tf.testDiagnose (y, yh2))
        new Plot (null, y, yh1, "y, yh1 vs. t", lines = true)
        new Plot (null, y, yh2, "y, yh2 vs. t", lines = true)

//      Forecaster.checkForecastMatrix (yf, y, yp)                    // FIX - differences & un-differenced
//      Forecaster.evalForecasts (mod, y, hh)
    end for

end aRIMATest4

