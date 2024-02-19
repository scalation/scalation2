
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 2.0
 *  @date    Thu May 26 18:06:08 EDT 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Auto-Regressive, Integrated (0 or 1), Moving Average (AR1MA)
 *
 *  @see http://en.wikipedia.org/wiki/Autoregressive%E2%80%93moving-average_model
 *  @see http://www.emu.edu.tr/mbalcilar/teaching2007/econ604/lecture_notes.htm
 *  @see http://www.stat.berkeley.edu/~bartlett/courses/153-fall2010
 *  @see http://www.princeton.edu/~apapanic/ORFE_405,_Financial_Time_Series_%28Fall_2011%29_files/slides12-13.pdf
 */

package scalation
package modeling
package forecasting

import scalation.mathstat._

import Forecaster.differ

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Return the first difference of the time-series y, giving the velocity v_t = y_t+1 - y_t.
 *  @param y  the original time-series to be differenced
 */
def del (y: VectorD): VectorD = VectorD (for t <- 0 until y.dim - 1 yield y(t+1) - y(t))

inline def Δ (y: VectorD): VectorD = del (y)


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Return the undifferenced time-series from the velocity series.
 *  @param v   the differenced time-series (velocity)
 *  @param y0  the first value in the original time-series
 */
def undel (v: VectorD, y0: Double): VectorD =
    val y = new VectorD (v.dim + 1)
    y(0)  = y0
    for t <- 1 until y.dim do y(t) = v(t-1) + y(t-1)
    y
end undel


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AR1MA` class provides basic time-series analysis capabilities for Auto-
 *  Regressive 'AR' Integrated 'I' Moving-Average 'MA' models.  In an
 *  AR1MA(p, q) model, p and q refer to the order of the Auto-Regressive and
 *  Moving-Average components of the model; d=1 refers to the order of differencing.
 *  Works by taking the first difference and delegating to the `ARMA` class.
 *  Also works for d=0 (no differencing).
 *  @param y       the response vector (time-series data)
 *  @param tt      the time vector, if relevant (time index may suffice)
 *  @param hparam  the hyper-parameters
 *  @param diffr   whether to take a first difference (defaults to true)
 */
class AR1MA (y: VectorD, tt: VectorD = null, hparam: HyperParameter = SARIMAX.hp,
             diffr: Boolean = true)
      extends Forecaster (y, tt, hparam)
         with Correlogram (y)
         with Fit (dfm = hparam("p").toInt, df = y.dim - hparam("p").toInt):

    private val debug  = debugf ("AR1MA", true)                        // debug function
    private val p      = hparam("p").toInt                             // p-th order Auto-Regressive model
    private val q      = hparam("q").toInt                             // q-th order Moving-Average model
    private val v      = if diffr then Δ(y) else y                     // first difference of the full time-series
    private val arma   = new ARMA (v, tt, hparam)                      // delegate to the `ARMA` class

    modelName = s"AR1MA($p, $q)"

    new Plot (null, y, null, s"Plot $modelName: y vs. t", lines = true)
    if diffr then new Plot (null, v, null, s"Plot $modelName: v = Δ(y) vs. t", lines = true)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Pick one of the following vectors: v full first difference, u differenced, or u itself.
     *  @param u  the input time-series vector
     */
    def pick (u: VectorD): VectorD =
        if u == y then v                                               // passed in original full time-series
        else if diffr then Δ(u)                                        // sub-series differenced
        else u                                                         // sub-series as is
    end pick

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `AR1MA` model to the times-series data in vector y_.
     *  Estimate the coefficient vectors φ and θ for (p, q)-th order AR1MA(p, q) model.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the training/full response vector (e.g., full y)
     */
    override def train (x_null: MatrixD, y_ : VectorD): Unit =
        arma.train (x_null, pick (y_))
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test PREDICTIONS of an AR1MA forecasting model y_ = f(lags (y_)) + e
     *  and return its predictions and  QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train before test.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the training/testing/full response/output vector
     */
    def test (x_null: MatrixD, y_ : VectorD): (VectorD, VectorD) =
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
    /** Test FORECASTS of an AR1MA forecasting model y_ = f(lags (y_)) + e
     *  and return its forecasts and QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train and forecastAll before testF.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the training/testing/full response/output vector
     */
    def testF (h: Int, y_ : VectorD): (VectorD, VectorD) =
        arma.testF (h, pick (y_))
    end testF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameter vector for the AR1MA(p, q) model.
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
    override def predictAll (y_ : VectorD): VectorD =
        arma.predictAll (pick (y_))
    end predictAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a vector of size h, of 1 through h-steps ahead forecasts for the model.
     *      forecast the following time points:  t+1, ..., t-1+h.
     *  Note, must create the yf matrix before calling the forecast method.
     *  Intended to work with rolling validation (analog of predict method)
     *  @param t   the time point from which to make forecasts
     *  @param yf  the forecasting matrix (time x horizons)
     *  @param y_  the actual values to use in making predictions
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecast (t: Int, yf: MatrixD, y_ : VectorD, h: Int): VectorD =
        arma.forecast (t, yf, pick (y_), h)
    end forecast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points at horizon h (h-steps ahead).
     *  Assign to forecasting matrix and return h-step ahead forecast.
     *  @param yf  the forecasting matrix (time x horizons)
     *  @param y_  the actual values to use in making forecasts
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecastAt (yf: MatrixD, y_ : VectorD, h: Int): VectorD =
        arma.forecastAt (yf, pick (y_), h)
    end forecastAt

    ////////////////////////////////////////////////////////////////////////////////
    // Make predictions/forecasts on the original scale time-series (not differenced).

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t+1 using the 1-step ahead forecast.
     *      y_t+1 = f (y_t, ...) + e_t+1
     *  @param t   the time point from which to make prediction
     *  @param y_  the actual values to use in making predictions
     */
    def predict2 (t: Int, y_ : VectorD): Double =
        arma.predict (t, pick (y_)) + (if diffr then y_(t) else 0.0)
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
//          println (FitM.fitMap (diagnose (y_, yp), QoF.values.map (_.toString)))
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
     *  Assign to forecasting matrix and return h-step ahead forecast.
     *  @param yf  the forecasting matrix (time x horizons)
     *  @param y_  the actual values to use in making forecasts
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecastAt2 (yf: MatrixD, y_ : VectorD, h: Int): VectorD =
        val yfh = arma.forecastAt (yf, pick (y_), h)
        if diffr then yfh + y_ else yfh
    end forecastAt2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points and all horizons (1 through h-steps ahead).
     *  Record these in the yf matrix, where
     *      yf(t, k) = k-steps ahead forecast for y_t
     *  Note, column 0, yf(?, 0), is set to y (the actual time-series values).
     *  Forecast recursively down diagonals in the yf forecasting matrix.
     *  The top right and bottom left triangles in yf matrix are not forecastable.
     *  @param y_  the actual values to use in making forecasts
     *  @param h   the maximum forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecastAll2 (y_ : VectorD, h: Int): MatrixD =
        debug ("forecastAll2", s"y_.dim = ${y_.dim}, e.dim = ${e.dim}")
        yf = new MatrixD (y_.dim+h, h+2)                               // forecasts for all time points t & horizons to h
        for t <- y_.indices do yf(t, 0) = y_(t)                        // first column is the timestep (e.g., logical day)
        for k <- 1 to h do forecastAt2 (yf, y_, k)                     // forecast k-steps into the future
        for t <- yf.indices do yf(t, h+1) = t                          // last column is time (logical day)
        yf                                                             // return matrix of forecasted values
    end forecastAll2

end AR1MA


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aR1MATest` main function tests the `AR1MA` class on simulated data.
 *  Test predictions (one step ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aR1MATest
 */
@main def aR1MATest (): Unit =

    val y = makeTSeries ()                                             // create simulated time-series (see `Stationary`)

    banner (s"Test Predictions: AR1MA(1, 0) on simulated time-series")
    val mod = new AR1MA (y)                                            // create model for time-series data AR(1)
    mod.trainNtest ()()                                                // train and test on full dataset

    banner ("Select model based on ACF and PACF")
    mod.plotFunc (mod.acF, "ACF")                                      // Auto-Correlation Function (ACF)
    mod.plotFunc (mod.pacF, "PACF")                                    // Partial Auto-Correlation Function (PACF)

end aR1MATest

import Example_LakeLevels.y

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aR1MATest2` main function tests the `AR1MA` class on real data:  Forecasting lake levels.
 *  Test predictions (one step ahead forecasts) with no differencing
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aR1MATest2
 */
@main def aR1MATest2 (): Unit =

    import SARIMAX.hp

    // d = 0 (no differencing) => should give same results as ARMA (@see `aRMATest4`)

    for p <- 1 to 3; q <- 0 to 2 do
        hp("p") = p; hp("q") = q                                       // set p (AR) and q (MA) hyper-parameters
        val mod = new AR1MA (y, diffr = false)                         // create model for time-series data AR1MA(1, 0)
        banner (s"Test Predictions: ${mod.modelName} (d=0) on LakeLevels Dataset")
        val (vp, qof) = mod.trainNtest ()()                            // test and test the model on full dataset
        val yp = mod.predictAll2 (y)                                   // results on original scale

        new Plot (null, y, yp, s"Plot: ${mod.modelName} predictAll2: y, yp vs t", lines = true)
    end for

end aR1MATest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aR1MATest3` main function tests the `AR1MA` class on real data:  Forecasting lake levels.
 *  Test predictions (one step ahead forecasts) taking one difference.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aR1MATest3
 */
@main def aR1MATest3 (): Unit =

    import SARIMAX.hp

    // d = 1 (first difference)

    val v = Δ (y)                                                      // take the first difference of time-series y
    differ (y, undel (v, y(0)))                                        // verify recovery of original times-series

    for p <- 1 to 3; q <- 0 to 2 do
        hp("p") = p; hp("q") = q                                       // set p (AR) and q (MA) hyper-parameters
        val mod = new AR1MA (y)                                        // create model for time-series data AR1MA(1, 0)
        banner (s"Test Predictions: ${mod.modelName} (d=1) on LakeLevels Dataset")
        val (vp, qof) = mod.trainNtest ()()                            // test and test the model on full dataset
        val yp = mod.predictAll2 (y)                                   // results on original scale

        new Plot (null, y, yp, s"Plot: ${mod.modelName} predictAll2: y, yp vs t", lines = true)
    end for

end aR1MATest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aR1MATest4` main function tests the `AR1MA` class on real data:
 *  Forecasting COVID-19.
 *  > runMain scalation.modeling.forecasting.aR1MATest4
 */
@main def aR1MATest4 (): Unit =

    import SARIMAX.hp

    val data = MatrixD.load ("covid_19.csv", 1, 1)                     // skip first row (header) and first column
    val yy   = data(?, 4)                                              // column 4 is daily deaths
//  val yy   = data(?, 5)                                              // column 5 is daily deaths smoothed
    val is   = yy.indexWhere (_ >= 2.0)                                // find day of first death with at least 2 deaths
    println (s"is = $is is first day with at least 2 deaths")
    val y    = yy(is until yy.dim)                                     // slice out days before is

//  val h   = 2                                                        // forecasting horizon
    for p <- 1 to 5; q <- 1 to 3 do                                    // AR1MA hyper-parameter settings
        hp("p") = p; hp("q") = q
        val mod = new AR1MA (y)                                        // create an AR1MA model
        val (vp, qof) = mod.trainNtest ()()                            // train and the model on full dataset
        val yp = mod.predictAll2 (y)                                   // results on original scale
        println (s"yp = $yp")

/*
        val yfa = mod.forecastAll (y, h)
        val yf  = yfa(?, h)                                            // forecasted values - h steps ahead
        new Plot (null, y, yf, s"Plot of y & yf, forecasted (h = $h) ${mod.modelName} vs. t", true)
*/

    end for

end aR1MATest4
