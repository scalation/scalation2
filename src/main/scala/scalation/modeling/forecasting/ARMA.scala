
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng, John Miller, Michael Cotterell
 *  @version 2.0
 *  @date    Sat Jun 13 01:27:00 EST 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: Auto-Regressive, Moving-Average (ARMA)
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
import scalation.optimization._

import Forecaster.differ
import ARMA.hp

def pq (hpar: HyperParameter): Int = hpar("p").toInt + hpar("q").toInt

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARMA` class provides basic time series analysis capabilities for Auto-Regressive,
 *  Moving-Average (ARMA) models.  In an ARMA(p, q) model, p refers to the order of the
 *  Auto-Regressive  components and q refers to the Moving-Average compoenest of the model.
 *  ARMA models are often used for forecasting.
 *  Given time-series data stored in vector y, its next value y_t+1 = y(t+1)
 *  may be predicted based on prior values of y and its noise:
 *      y_t+1 = δ + Σ(φ_j y_t-j) + Σ(θ_j e_t-j) + e_t+1
 *  where δ is a constant, φ is the auto-regressive coefficient vector,
 *  θ is the moving-average vector, and e_t+1 is the noise term.
 *  @param y       the response vector (time-series data)
 *  @param tt      the time vector, if relevant (time index may suffice)
 *  @param hparam  the hyper-parameters
 */
class ARMA (y: VectorD, tt: VectorD = null, hparam: HyperParameter = ARMA.hp)
      extends Forecaster (y, tt, hparam)
         with Correlogram (y)
         with Fit (dfm = pq (hparam), df = y.dim - pq (hparam)):

    private   val debug = debugf ("ARMA", true)                        // debug function
    private   val flaw  = flawf ("ARMA")                               // flaw function
    protected val p     = hparam("p").toInt                            // p-th order Auto-Regressive part
    protected val q     = hparam("q").toInt                            // q-th order Moving-Average part
    protected var φ     = new VectorD (p)                              // AR(p) parameters/coefficients
    protected var θ     = new VectorD (q)                              // MA(q) parameters/coefficients
    protected var δ     = NO_DOUBLE                                    // drift/intercept/constant term
    protected val pnq   = pq (hparam)                                  // combined order

    modelName = s"ARMA($p, $q)"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum lag used by this model (its capacity to look into the past).
     */
    override def cap: Int = max (p, q)                                 // max order

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `ARMA` model to the times-series data in vector y_.
     *  Estimate the coefficient vectors φ and θ for (p, q)-th order ARMA(p, q) model.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the training/full response vector (e.g., full y)
     */
    def train (x_null: MatrixD, y_ : VectorD): Unit = 
        m = y_.dim                                                       // length of relevant time-series
        println (s"e.dim = ${e.dim}")
        resetDF (pnq, m - pnq)                                           // reset the degrees of freedom
        makeCorrelogram (y_)                                             // correlogram computes psi matrix, gives ACF and PACF

        val mu = y_.mean                                                 // sample mean of y_
        val z  = y_ - mu                                                 // optimization works better using zero-centered data
        δ = 0.0                                                          // drift/intercept for z (should end up close to zero)
        val b = φ ++ θ :+ δ                                              // combine all parameters -> vector to optimize

        def csse (b: VectorD): Double =                                  // objective function - conditional sum of squared errors
            φ = b(0 until p); θ = b(p until p+q); δ = b(b.dim-1)         // pull parameters out of b vector 
            ssef (z, predictAll (z))                                     // compute loss function
        end csse

        def nll (b: VectorD): Double =                                   // objective function - negative log-likelihood (MLE)
            0.0                                                          // FIX - implement
        end nll

        val optimizer = new BFGS (csse)                                  // apply Quasi-Newton BFGS optimizer
//      val optimizer = new ConjugateGradient (csse)                     // apply Conjugate Gradient optimizer - fails
//      val optimizer = new CoordinateDescent (csse)                     // apply Coordinate Descent optimizer
//      val optimizer = new NelderMeadSimplex (csse, 3)                  // apply Nelder-Mead Simplex optimizer
//      val optimizer = new GridSearch (csse, 3); optimizer.setAxes ()   // apply GridSearch BFGS optimizer - close
        val (fb, bb) = optimizer.solve (b)                               // optimal solution for the objective function and parameters

        φ = bb(0 until p); θ = bb(p until p+q); δ = bb(b.dim-1)          // recover parameters for z
        δ += mu * (1 - φ.sum)                                            // uncenter  
        debug ("train", s"parameters for ARMA($p, $q) model: φ = $φ, θ = $θ, δ = $δ")
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test PREDICTIONS of an ARMA forecasting model y_ = f(lags (y_)) + e
     *  and return its predictions and  QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train before test.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the testing/full response/output vector
     */
    def test (x_null: MatrixD, y_ : VectorD): (VectorD, VectorD) =
        val (yy, yp) = testSetup (y_)                                    // get and align actual and predicted values
        resetDF (pnq, yy.dim - pnq)                                      // reset the degrees of freedom
        println (s"test: yy.dim = ${yy.dim}, yp.dim = ${yp.dim}")
//      differ (yy, yp)                                                  // uncomment for debugging
        (yp, diagnose (yy, yp))                                          // return predictions and QoF vector
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test FORECASTS of an ARMA forecasting model y_ = f(lags (y_)) + e
     *  and return its forecasts and QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train and forecastAll before testF.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the testing/full response/output vector
     */
    def testF (h: Int, y_ : VectorD): (VectorD, VectorD) =
        val (yy, yfh) = testSetupF (y_, h)                               // get and align actual and forecasted values
        resetDF (pnq, yy.dim - pnq)                                      // reset the degrees of freedom
        println (s"testF: yy.dim = ${yy.dim}, yfh.dim = ${yfh.dim}")
//      differ (yy, yfh)                                                 // uncomment for debugging
        (yfh, diagnose (yy, yfh))                                        // return predictions and QoF vector
    end testF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameter vector for the ARMA(p, q) model.
     */
    override def parameter: VectorD = φ ++ θ :+ δ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t+1 using 1-step ahead forecasts (p' = p-1).
     *      y_t+1 = δ + φ_0 y_t + φ_1 y_t-1 + ... + φ_p' y_t-p' +
     *                  θ_0 e_t + θ_1 e_t-1 + ... + θ_q' e_t-q'
     *  When k < 0 let y_k = y_0 (i.e., assume first value repeats back in time),
     *  but do not assume errors repeat.
     *  @see predictAll method in `Forecaster` trait
     *  @param t   the time point/index to be predicted
     *  @param y_  the actual values to use in making predictions
     */
    override def predict (t: Int, y_ : VectorD): Double =
        var sum = δ                                                      // intercept
        for j <- 0 until p do             sum += φ(j) * y_(max (0, t-j))
        for j <- 0 until q if t-j >= 0 do sum += θ(j) * e(t-j)
        if t < y_.dim-1 then e(t+1) = y_(t+1) - sum                      // update the error vector
        sum                                                              // prediction for y_t, yp_t
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
    override def forecast (t: Int, yf: MatrixD, y_ : VectorD, h: Int): VectorD =
        if h < 1 then flaw ("forecast", s"horizon h = $h must be at least 1")
        val m1 = y_.dim - 1
        val yd = new VectorD (h)                                       // hold forecasts for each horizon
        for k <- 1 to h do
            val t1  = t + k - 1                                        // time point prior to horizon
            var sum = δ
            for j <- 0 until p do sum += φ(j) * yf(max (0, t1-j), max (0, k-1-j))
            for j <- 0 until q do
                if t1-j in (0, m1) then sum += θ(j) * e(t1-j)
            end for
            yf(t+k, k) = sum                                           // forecast down the diagonal
            yd (k-1)   = sum                                           // record diagonal values
            if h == 1 && t < m1 then e(t+1) = y_(t+1) - sum            // update the next element in the error vector
        end for
        yd                                                             // return forecasts for each horizon
    end forecast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points at horizon h (h-steps ahead).
     *  Assign to forecasting matrix and return h-step ahead forecast.
     *  @see forecastAll method in `Forecaster` trait
     *  @param yf  the forecasting matrix (time x horizons)
     *  @param y_  the actual values to use in making forecasts
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    override def forecastAt (yf: MatrixD, y_ : VectorD, h: Int): VectorD =
        if h < 1 then flaw ("forecastAt", s"horizon h = $h must be at least 1")
        e(0)                                                           // assume error at time 0 is 0
        val m1 = y_.dim - 1
        for t <- y_.indices do                                         // make forecasts over all time points for horizon k
            val t1  = t + h - 1                                        // time point prior to horizon
            var sum = δ
            for j <- 0 until p do sum += φ(j) * yf(max (0, t1-j), max (0, h-1-j))
            for j <- 0 until q do
                if t1-j in (0, m1) then sum += θ(j) * e(t1-j)
            end for
            yf(t+h, h) = sum                                           // forecast down the diagonal
            if h == 1 && t < m1 then e(t+1) = y_(t+1) - sum            // update the next element in the error vector
        end for
        yf(?, h)                                                       // return the h-step ahead forecast vector
    end forecastAt

end ARMA


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARMA` companion object provides factory methods for the `ARMA` class.
 */
object ARMA:

    /** Base hyper-parameter specification for the `AR` and `ARMA` classes
     */
    val hp = new HyperParameter
    hp += ("p", 1, 1)                                                  // AR order AR(p)
    hp += ("q", 0, 0)                                                  // MA order MA(q)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARMA` object.
     *  @param y       the response vector (time series data)
     *  @param tt      the time vector, if relevant (time index may suffice)
     *  @param hparam  the hyper-parameters
     */
    def apply (y: VectorD, tt: VectorD = null, hparam: HyperParameter = ARMA.hp): ARMA = 
        new ARMA (y, tt, hparam)
    end apply

end ARMA


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRMATest` main function tests the `ARMA` class on simulated data.
 *  Test predictions (one step ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRTest
 */
@main def aRMATest (): Unit =

    val y = makeTSeries ()                                             // create simulated time-series (see `Stationary`)

    banner (s"Test Predictions: ARMA(1, 0) on simulated time-series")
    val mod = new ARMA (y)                                             // create model for time series data ARMA(1, 0)
    mod.trainNtest ()()                                                // train and test on full dataset

    banner ("Select model based on ACF and PACF")
    mod.plotFunc (mod.acF, "ACF")                                      // Auto-Correlation Function (ACF)
    mod.plotFunc (mod.pacF, "PACF")                                    // Partial Auto-Correlation Function (PACF)

end aRMATest

import Example_LakeLevels.y

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRMATest2` main function tests the `ARMA` class on real data:  Forecasting lake levels.
 *  Test predictions (one step ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRMATest2
 */
@main def aRMATest2 (): Unit =

    banner (s"Test Predictions: ARMA(1, 0) on LakeLevels Dataset")
    var mod = new ARMA (y)                                             // create model for time series data ARMA(1, 0)
    mod.trainNtest ()()                                                // train and test the model on full dataset

    banner (s"Test Predictions: ARMA(1, 1) on LakeLevels Dataset")
    hp("q") = 1                                                        // set moving-average hyper-parameter q to 1
    mod = new ARMA (y)                                                 // create model for time series data ARMA(1, 1)
    mod.trainNtest ()()                                                // train and test the model on full dataset

    banner ("Select model based on ACF and PACF")
    mod.plotFunc (mod.acF, "ACF")                                      // Auto-Correlation Function (ACF)
    mod.plotFunc (mod.pacF, "PACF")                                    // Partial Auto-Correlation Function (PACF)

end aRMATest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRMATest3` main function tests the `ARMA` class on real data:  Forecasting lake levels.
 *  Test forecasts (1 to h steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRMATest3
 */
@main def aRMATest3 (): Unit =

    val m  = y.dim                                                     // number of data points
    val hh = 2                                                         // maximum forecasting horizon

    banner (s"Test Forecasts: ARMA(1, 0) on LakeLevels Dataset")
    val mod = new ARMA (y)                                             // create model for time series data ARMA(1, 0)
    val (yp, qof) = mod.trainNtest ()()                                // train and test the model on full dataset

    val yf = mod.forecastAll (y, hh)                                   // forecast h-steps ahead (h = 1 to hh) for all y
    println (s"y.dim = ${y.dim}, yp.dim = ${yp.dim}, yf.dims = ${yf.dims}")
    assert (yf(?, 0)(0 until m) == y)                                  // column 0 must agree with actual values
    differ (yf(?, 1)(1 until m), yp)
    assert (yf(?, 1)(1 until m) == yp)                                 // column 1 must agree with one step-ahead predictions

    for h <- 1 to hh do
        val (yfh, qof) = mod.testF (h, y)                              // h-steps ahead forecast and its QoF
        println (s"Evaluate QoF for horizon $h:")
        println (FitM.fitMap (qof, QoF.values.map (_.toString)))       // evaluate h-steps ahead forecasts
    end for

end aRMATest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRMATest4` main function tests the `ARMA` class on real data:  Forecasting lake levels.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRMATest4
 */
@main def aRMATest4 (): Unit =

    val m  = y.dim                                                     // number of data points
    val hh = 2                                                         // maximum forecasting horizon
 
    for p <- 1 to 7; q <- 0 to 2 do
        hp("p") = p; hp("q") = q                                       // set p (AR) and q (MA) hyper-parameters
        val mod = new ARMA (y)                                         // create model for time series datsk
        banner (s"Test: ${mod.modelName} on LakeLevels Dataset")
        val (yp, qof) = mod.trainNtest ()()                            // train and test the model on full dataset

        val yf = mod.forecastAll (y, hh)                               // forecast h-steps ahead for all y
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

end aRMATest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRMATest5` main function tests the `ARMA` class on real data:  Forecasting lake levels.
 *  This test looks at the velocity series (first differences).
 *  Test predictions (one step ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRMATest5
 */
@main def aRMATest5 (): Unit =

    val v = Δ (y)                                                      // velocity series (first differences)

    var mod: ARMA = null
    for p <- 0 to 8; q <- 0 to 8 if p + q > 0 do
        hp("p") = p; hp("q") = q
        banner (s"Test Predictions: ARMA($p, $q) on LakeLevels Dataset")
        mod = new ARMA (v)                                             // create model for time series data ARMA(1, 0)
        mod.trainNtest ()()                                            // train the model on full dataset
    end for

    banner ("Select model based on ACF and PACF")
    mod.plotFunc (mod.acF, "ACF")                                      // Auto-Correlation Function (ACF)
    mod.plotFunc (mod.pacF, "PACF")                                    // Partial Auto-Correlation Function (PACF)

end aRMATest5


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRMATest6` main function tests the `ARMA` class on real data:
 *  Forecasting COVID-19.
 *  > runMain scalation.modeling.forecasting.aRMATest6
 */
@main def aRMATest6 (): Unit =

    import ARMA.hp

    val data = MatrixD.load ("covid_19.csv", 1, 1)                     // skip first row (header) and first column
    val yy   = data(?, 4)                                              // column 4 is daily deaths
//  val yy   = data(?, 5)                                              // column 5 is daily deaths smoothed
    val is   = yy.indexWhere (_ >= 2.0)                                // find day of first death with at least 2 deaths
    println (s"is = $is is first day with at least 2 deaths")
    val y    = yy(is until yy.dim)                                     // slice out days before is

    val h = 1                                                          // forecasting horizon
    for p <- 1 to 4; q <- 0 to 2 do                                    // ARMA hyper-parameter settings
        hp("p") = p; hp("q") = q
        val mod = new ARMA (y)                                         // create an ARMA model
        val (yp, qof) = mod.trainNtest ()()                            // train and the model on full dataset

/*
        val yfa = mod.forecastAll (y, h)
        val yf  = yfa(?, h)                                            // forecasted values - h steps ahead
        new Plot (null, y, yf, s"Plot of y & yf, forecasted (h = $h) ${mod.modelName} vs. t", true)
*/

    end for

end aRMATest6


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRMATest6` main function tests the `AR` class on real data:  Forecasting Weekly Covid-19.
 *  Test forecasts (1 to h steps ahead forecasts).  Try multiple values for p.
 *  > runMain scalation.modeling.forecasting.aRMATest7
 */
@main def aRMATest7 (): Unit =

    val y  = Example_Covid.loadData_y ("new_deaths")
    val m  = y.dim                                                     // number of data points
    val hh = 2                                                         // maximum forecasting horizon

    println (s"y.dim = ${y.dim}")

    var mod: ARMA = null
    for p <- 1 to 12 do                                                // autoregressive hyper-parameter p
        ARMA.hp("p") = p                                               // set p hyper-parameter
        banner (s"Test: ARMA($p) on Covid-19 Weekly Dataset")
        mod = new ARMA (y)                                             // create model for time series data
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

end aRMATest7

