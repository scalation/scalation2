
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng, John Miller, Michael Cotterell
 *  @version 2.0
 *  @date    Sat Jun 13 01:27:00 EST 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: Auto-Regressive (AR)
 *
 *  @see     http://en.wikipedia.org/wiki/Autoregressive%E2%80%93moving-average_model
 *  @see     http://www.emu.edu.tr/mbalcilar/teaching2007/econ604/lecture_notes.htm
 *  @see     http://www.stat.berkeley.edu/~bartlett/courses/153-fall2010
 *  @see     http://www.princeton.edu/~apapanic/ORFE_405,_Financial_Time_Series_%28Fall_2011%29_files/slides12-13.pdf
 */

package scalation
package modeling
package forecasting

import scala.math.max

import scalation.mathstat._

import Forecaster.differ

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AR` class provides basic time series analysis capabilities for Auto-Regressive
 *  (AR) models.  In an AR(p) model, p refers to the order of the Auto-Regressive
 *  components of the model.  AR models are often used for forecasting.
 *  Given time-series data stored in vector y, its next value y_t+1 = y(t+1)
 *  may be predicted based on prior values of y and its noise:
 *      y_t+1 = δ + Σ(φ_j y_t-j) + e_t+1
 *  where δ is a constant, φ is the auto-regressive coefficient vector,
 *  and e_t+1 is the noise term.
 *  @param y       the response vector (time-series data)
 *  @param tt      the time vector, if relevant (time index may suffice)
 *  @param hparam  the hyper-parameters
 */
class AR (y: VectorD, tt: VectorD = null, hparam: HyperParameter = ARMA.hp)
      extends Forecaster (y, tt, hparam)
         with Correlogram (y)
         with Fit (dfm = hparam("p").toInt, df = y.dim - hparam("p").toInt):

    private   val debug = debugf ("AR", true)                          // debug function
    private   val flaw  = flawf ("AR")                                 // flaw function
                  m     = y.dim                                        // number of time points (@see `FitM`)
    private val p       = hparam("p").toInt                            // p-th order Auto-Regressive model
    private var φ       = VectorD.nullv                                // AR(p) parameters/coefficients
    private var δ       = NO_DOUBLE                                    // drift/intercept/constant term
    private val pnq     = p                                            // sum of # parameters

    if p > MAX_LAGS then flaw ("init", s"p = $p must not be greater than MAX_LAGS = $MAX_LAGS")

    modelName = s"AR($p)"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `AR` model to the times-series data in vector y_.
     *  Estimate the coefficient vector φ for a p-th order Auto-Regressive AR(p) model.
     *  Uses Durbin-Levinson Algorithm (in `Correlogram`) to determine the coefficients.
     *  The φ vector is p-th row of psi matrix (ignoring the first (0th) column).
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the training/full response vector (e.g., full y)
     */
    def train (x_null: MatrixD, y_ : VectorD): Unit = 
        m = y_.dim                                                     // length of relevant time-series
        resetDF (pnq, m - pnq)                                         // reset the degrees of freedom
        makeCorrelogram (y_)                                           // correlogram computes psi matrix
        φ = psiM(p)(1 until p+1)                                       // coefficients = p-th row, columns 1, 2, ... p
        δ = statsF.mu * (1 - φ.sum)                                    // compute drift/intercept
        debug ("train", s"parameters for AR($p) model: φ = $φ, δ = $δ")
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test PREDICTIONS of an AR forecasting model y_ = f(lags (y_)) + e
     *  and return its predictions and  QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train before test.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the testing/full response/output vector
     */
    def test (x_null: MatrixD, y_ : VectorD): (VectorD, VectorD) =
        val (yy, yp) = testSetup (y_)                                  // get and align actual and predicted values
        resetDF (pnq, yy.dim - pnq)                                    // reset the degrees of freedom
        println (s"test: yy.dim = ${yy.dim}, yp.dim = ${yp.dim}")
//      differ (yy, yp)                                                // uncomment for debugging
        (yp, diagnose (yy, yp))                                        // return predictions and QoF vector
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test FORECASTS of an AR forecasting model y_ = f(lags (y_)) + e
     *  and return its forecasts and QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train and forecastAll before testF.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the testing/full response/output vector
     */
    def testF (h: Int, y_ : VectorD): (VectorD, VectorD) =
        val (yy, yfh) = testSetupF (y_, h)                             // get and align actual and forecasted values
        resetDF (pnq, yy.dim - pnq)                                    // reset the degrees of freedom
        println (s"testF: yy.dim = ${yy.dim}, yfh.dim = ${yfh.dim}")
//      differ (yy, yfh)                                               // uncomment for debugging
        (yfh, diagnose (yy, yfh))                                      // return predictions and QoF vector
    end testF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameter vector for the AR(p) model.
     */
    override def parameter: VectorD = φ :+ δ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t+1 using the 1-step ahead forecast.
     *      y_t+1 = φ_0 y_t + φ_1 y_t-1 + ... + φ_p-1 y_t-(p-1)
     *  When t-j is negative, use y_0
     *  @param t   the time point from which to make prediction
     *  @param y_  the actual values to use in making predictions
     */
    def predict (t: Int, y_ : VectorD): Double =
        var sum = δ                                                    // intercept
        for j <- 0 until p do sum += φ(j) * y_(max (0, t-j))           // add φ_j y_t-j
        sum
    end predict

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
        if h < 1 then flaw ("forecast", s"horizon h = $h must be at least 1")
        val yd = new VectorD (h)                                       // hold forecasts for each horizon
        for k <- 1 to h do
            val t1  = t + k - 1                                        // time point prior to horizon
            var sum = δ
            for j <- 0 until p do sum += φ(j) * yf(max (0, t1-j), max (0, k-1-j))
            yf(t+k, k) = sum                                           // forecast down the diagonal
            yd (k-1)   = sum                                           // record diagonal values
        end for
        yd                                                             // return forecasts for each horizon
    end forecast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points at horizon h (h-steps ahead).
     *  Assign to forecasting matrix and return h-step ahead forecast.
     *  For 1-step ahead (h = 1),
     *      y_t = δ + φ_0 y_t-1 + φ_1 y_t-2 + ... + φ_p-1 y_t-p
     *  When k < 0 let y_k = y_0 (i.e., assume first value repeats back in time).
     *  @param yf  the forecasting matrix (time x horizons)
     *  @param y_  the actual values to use in making forecasts
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecastAt (yf: MatrixD, y_ : VectorD, h: Int): VectorD =
        if h < 1 then flaw ("forecastAt", s"horizon h = $h must be at least 1")
        for t <- y_.indices do                                         // make forecasts over all time points for horizon h
            val t1  = t + h - 1                                        // time point prior to horizon
            var sum = δ
            for j <- 0 until p do sum += φ(j) * yf(max (0, t1-j), max (0, h-1-j))
            yf(t+h, h) = sum                                           // forecast down the diagonal
        end for
        yf(?, h)                                                       // return the h-step ahead forecast vector
    end forecastAt

end AR


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AR` companion object provides factory methods for the `AR` class.
 *  Use `ARMA` for hyper-parameters.
 */
object AR:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `AR` object.
     *  @param y       the response vector (time series data)
     *  @param tt      the time vector, if relevant (time index may suffice)
     *  @param hparam  the hyper-parameters
     */
    def apply (y: VectorD, tt: VectorD = null, hparam: HyperParameter = ARMA.hp): AR = 
        new AR (y, tt, hparam)
    end apply

end AR


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRTest` main function tests the `AR` class on simulated data.
 *  Test predictions (one step ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRTest
 */
@main def aRTest (): Unit =

    val y = makeTSeries ()                                             // create simulated time-series (see `Stationary`)

    banner (s"Test Predictions: AR(1) on simulated time-series")
    val mod = new AR (y)                                               // create model for time series data AR(1)
    mod.trainNtest ()()                                                // train and test on full dataset

    banner ("Select model based on ACF and PACF")
    mod.plotFunc (mod.acF, "ACF")                                      // Auto-Correlation Function (ACF)
    mod.plotFunc (mod.pacF, "PACF")                                    // Partial Auto-Correlation Function (PACF)

end aRTest

import Example_LakeLevels.y

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRTest2` main function tests the `AR` class on real data:  Forecasting lake levels.
 *  Test predictions (one step ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRTest2
 */
@main def aRTest2 (): Unit =

    banner (s"Test Predictions: AR(1) on LakeLevels Dataset")
    val mod = new AR (y)                                               // create model for time series data AR(1)
    mod.trainNtest ()()                                                // train and test on full dataset

    banner ("Select model based on ACF and PACF")
    mod.plotFunc (mod.acF, "ACF")                                      // Auto-Correlation Function (ACF)
    mod.plotFunc (mod.pacF, "PACF")                                    // Partial Auto-Correlation Function (PACF)

end aRTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRTest3` main function tests the `AR` class on real data:  Forecasting lake levels.
 *  Test forecasts (1 to h steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRTest3
 */
@main def aRTest3 (): Unit =

    val m  = y.dim                                                     // number of data points
    val hh = 2                                                         // maximum forecasting horizon

    ARMA.hp("p") = 3
    banner (s"Test Forecasts: AR(1) on LakeLevels Dataset")
    val mod = new AR (y)                                               // create model for time series data AR(1)
    val (yp, qof) = mod.trainNtest ()()                                // train and test on full dataset

    val yf = mod.forecastAll (y, hh)                                   // forecast h-steps ahead (h = 1 to hh) for all y
    println (s"y.dim = ${y.dim}, yp.dim = ${yp.dim}, yf.dims = ${yf.dims}")
    println (s"yf = $yf")
    assert (yf(?, 0)(0 until m) == y)                                  // column 0 must agree with actual values
    differ (yf(?, 1)(1 until m), yp)
    assert (yf(?, 1)(1 until m) == yp)                                 // column 1 must agree with one step-ahead predictions

    for h <- 1 to hh do
        val (yfh, qof) = mod.testF (h, y)                              // h-steps ahead forecast and its QoF
        val yy = y(h until m)                                          // actual response aligned with yfh
        println (s"Evaluate QoF for horizon $h:")
        println (FitM.fitMap (qof, QoF.values.map (_.toString)))       // evaluate h-steps ahead forecasts
        println (s"Fit.mae (y, yfh, h)  = ${Fit.mae (y, yfh, h)}")     // evaluate h-steps ahead forecasts with MAE
        println (s"Fit.mae_n (y, 1)     = ${Fit.mae_n (y, 1)}")        // evaluate h-steps ahead forecasts with MAE_n
        println (s"Fit.mase (y, yfh, h) = ${Fit.mase (y, yfh, h)}")    // evaluate h-steps ahead forecasts with MASE
    end for

end aRTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRTest4` main function tests the `AR` class on real data:  Forecasting lake levels.
 *  Test forecasts (1 to h steps ahead forecasts).  Try multiple values for p.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRTest4
 */
@main def aRTest4 (): Unit =

    val m  = y.dim                                                     // number of data points
    val hh = 3                                                         // maximum forecasting horizon
 
    var mod: AR = null
    for p <- 1 to 5 do                                                 // autoregressive hyper-parameter p
        ARMA.hp("p") = p                                               // set p hyper-parameter
        banner (s"Test: AR($p) on LakeLevels Dataset")
        mod = new AR (y)                                               // create model for time series data
        val (yp, qof) = mod.trainNtest ()()                            // train and test on full dataset

        val yf = mod.forecastAll (y, hh)                               // forecast h-steps ahead for all y
//      println (s"yf = $yf")
        println (s"y.dim = ${y.dim}, yp.dim = ${yp.dim}, yf.dims = ${yf.dims}")
        assert (yf(?, 0)(0 until m) == y)                              // column 0 must agree with actual values
        differ (yf(?, 1)(1 until m), yp)
        assert (yf(?, 1)(1 until m) == yp)                             // column 1 must agree with one step-ahead predictions

        for h <- 1 to hh do
            val (yfh, qof) = mod.testF (h, y)                          // h-steps ahead forecast and its QoF
            println (s"Evaluate QoF for horizon $h:")
            println (FitM.fitMap (qof, QoF.values.map (_.toString)))   // evaluate h-steps ahead forecasts
        end for
    end for

end aRTest4

