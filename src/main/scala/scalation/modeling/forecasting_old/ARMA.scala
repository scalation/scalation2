
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng, John Miller, Michael Cotterell
 *  @version 2.0
 *  @date    Sat Jun 13 01:27:00 EST 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Auto-Regressive, Moving-Average (ARMA)
 *
 *  @see     http://en.wikipedia.org/wiki/Autoregressive%E2%80%93moving-average_model
 *  @see     http://www.emu.edu.tr/mbalcilar/teaching2007/econ604/lecture_notes.htm
 *  @see     http://www.stat.berkeley.edu/~bartlett/courses/153-fall2010
 *  @see     http://www.princeton.edu/~apapanic/ORFE_405,_Financial_Time_Series_%28Fall_2011%29_files/slides12-13.pdf
 */

package scalation
package modeling
package forecasting_old

import scala.math.max

import scalation.mathstat._
import scalation.optimization.quasi_newton.{BFGS => Optimizer}       // change import to change optimizer
//import scalation.optimization.quasi_newton.{LBFGS => Optimizer}
import scalation.random.NormalVec_c

def pq (hpar: HyperParameter): Int = hpar("p").toInt + hpar("q").toInt

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARMA` class provides basic time series analysis capabilities for Auto-Regressive,
 *  Moving-Average (ARMA) models.  In an ARMA(p, q) model, p refers to the order of the
 *  Auto-Regressive  components and q refers to the Moving-Average compoenest of the model.
 *  Given time series data stored in vector y, its next value y_t+1 = y(t+1)
 *  may be predicted based on past values of y and past shocks (differences in actual and
 *  forecasted values):
 *
 *      y_t+1 = δ + Σ[φ_j y_t-j] + Σ[θ_j e_t-j] + e_t+1
 *
 *  where δ is a constant, φ is the auto-regressive coefficient vector,
 *  θ is the moving-average vector, and e_t+1 is the new residual/error/shock term.
 *  @param y       the response vector (time series data)
 *  @param tt      the time vector, if relevant (time index may suffice)
 *  @param hparam  the hyper-parameters
 */
class ARMA (y: VectorD, tt: VectorD = null, hparam: HyperParameter = SARIMA.hp)
      extends Forecaster (y, tt, hparam)
         with Correlogram (y)
         with Fit (dfm = pq (hparam), df = y.dim - pq (hparam)):

    private   val debug = debugf ("ARMA", true)                         // debug function
    private   val flaw  = flawf ("ARMA")                                // flaw function
    protected val p     = hparam("p").toInt                             // p-th order Auto-Regressive part
    protected val q     = hparam("q").toInt                             // q-th order Moving-Average part
    protected var φ     = NormalVec_c (p, 0.0, 0.1).gen                 // AR(p) parameters/coefficients
    protected var θ     = NormalVec_c (q, 0.0, 0.1).gen                 // MA(q) parameters/coefficients
    protected var δ     = NO_DOUBLE                                     // drift/intercept/constant term
    protected val pnq   = pq (hparam)                                   // combined order

    modelName = s"ARMA($p, $q)"

    debug ("init", s"$modelName")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum lag used by this model (its capacity to look into the past).
     */
    override def cap: Int = max (p, q)                                  // max order

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `ARMA` model to the times-series data in vector y_.
     *  Estimate the coefficient vectors φ and θ for (p, q)-th order ARMA(p, q) model.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the training/full response vector (e.g., full y)
     */
    def train (x_null: MatrixD, y_ : VectorD): Unit = 
        m = y_.dim                                                      // length of relevant time-series
        println (s"e.dim = ${e.dim}")
        resetDF (pnq, m - pnq)                                          // reset the degrees of freedom
        makeCorrelogram (y_)                                            // correlogram computes psi matrix, gives ACF and PACF

        val mu = y_.mean                                                // sample mean of y_
        val z  = y_ - mu                                                // optimization works better using zero-centered data
        δ = 0.0                                                         // drift/intercept for z (should end up close to zero)
        val b = φ ++ θ :+ δ                                             // combine all parameters -> vector to optimize

        def csse (b: VectorD): Double =                                 // objective function - conditional sum of squared errors
            φ = b(0 until p); θ = b(p until p+q); δ = b(b.dim-1)        // pull parameters out of b vector 
            ssef (z, predictAll (z))                                    // compute loss function
        end csse

/*
        def nll (b: VectorD): Double =                                  // objective function - negative log-likelihood (MLE)
            0.0                                                         // FIX - implement
        end nll
*/

        val optimizer = new Optimizer (csse)                            // apply Quasi-Newton optimizer
        val (fb, bb) = optimizer.solve (b, 0.5)                         // optimal solution for the objective function and parameters

        φ = bb(0 until p); θ = bb(p until p+q); δ = bb(b.dim-1)         // recover parameters for z
        δ += mu * (1 - φ.sum)                                           // uncenter  
        debug ("train", s"parameters for ARMA($p, $q) model: φ = $φ, θ = $θ, δ = $δ")
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test PREDICTIONS of an ARMA forecasting model y_ = f(lags (y_)) + e
     *  and return its predictions and  QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train before test.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the actual testing/full response/output vector
     */
    override def test (x_null: MatrixD, y_ : VectorD): (VectorD, VectorD) =
        val (yp, no_qof) = super.test (null, y_)                        // call super.test for predictions
        resetDF (pnq, y_.dim - pnq)                                     // reset the degrees of freedom
        (yp, diagnose (y_, yp))                                         // return predictions and QoF vector
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test FORECASTS of an ARMA forecasting model y_ = f(lags (y_)) + e
     *  and return its forecasts and QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train and forecastAll before testF.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the testing/full response/output vector
     */
    def testF (h: Int, y_ : VectorD): (VectorD, VectorD, VectorD) =
        val (yy, yfh) = testSetupF (y_, h)                              // get and align actual and forecasted values
        resetDF (pnq, yy.dim - pnq)                                     // reset the degrees of freedom
        println (s"testF: yy.dim = ${yy.dim}, yfh.dim = ${yfh.dim}")
//      Forecaster.differ (yy, yfh)                                     // uncomment for debugging
        (yy, yfh, diagnose (yy, yfh))                                   // return aligned actual, forecasted and QoF vectors
    end testF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameter vector for the ARMA(p, q) model.
     */
    override def parameter: VectorD = φ ++ θ :+ δ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t+1 using 1-step ahead forecasts (p' = p-1).
     *
     *      y_t+1 = δ + φ_0 y_t + φ_1 y_t-1 + ... + φ_p' y_t-p' +
     *                  θ_0 e_t + θ_1 e_t-1 + ... + θ_q' e_t-q'
     *
     *  When k < 0 let y_k = y_0 (i.e., assume first value repeats back in time),
     *  but do not assume errors repeat.  Note, column 1 of yf (yf(?, 1) holds yp.
     *  Must be executed in time order, so errors are properly recorded in vector e
     *  @see `predictAll` method in `Forecaster` trait.
     *  @see `rdot` in Forecaster.scala for reverse dot product implementation.
     *  @param i   the time series index from which to make prediction
     *  @param y_  the actual time series values to use in making predictions (has one backcast)
     */
    override def predict (i: Int, y_ : VectorD): Double =
        if i == 0 then e(0) = 0                                         // t = -1 (from backcast), assume no error
        if i == 1 then e(1) = y_(1) - yf(0, 1)                          // t = 0 (first real point)

        var sum = δ + rdot (φ, y_, i)                                   // intercept + AR terms
        for j <- 0 until q if i-j >= 0 do sum += θ(j) * e(i-j)          // add MA terms (shocks)
//      yf(i, 1) = sum                                                  // yp(i) = yf(i, 1)

        if i < y_.dim-1 then e(i+1) = y_(i+1) - sum                     // update the error vector
        sum                                                             // prediction yp
    end predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a vector of size h, of 1 through h-steps ahead forecasts for the model.
     *      forecast the following time points:  t+1, ..., t-1+h.
     *  Note, must create the yf matrix before calling the forecast method.
     *  Intended to work with rolling validation (analog of predict method)
     *  FIX - not updated
     *  @param t   the time point from which to make forecasts
     *  @param yf  the forecast matrix (time x horizons)
     *  @param y_  the actual values to use in making predictions
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecast (t: Int, yf: MatrixD, y_ : VectorD, h: Int): VectorD =
        if h < 1 then flaw ("forecast", s"horizon h = $h must be at least 1")
        val m1 = y_.dim - 1
        val yd = new VectorD (h)                                        // hold forecasts for each horizon
        for k <- 1 to h do
            val t1  = t + k - 1                                         // time point prior to horizon
            var sum = δ
            for j <- 0 until p do sum += φ(j) * yf(max (0, t1-j), max (0, k-1-j))
            for j <- 0 until q do
                if t1-j in (0, m1) then sum += θ(j) * e(t1-j)
            end for
            yf(t+k, k) = sum                                            // forecast down the diagonal
            yd(k-1)    = sum                                            // record diagonal values
            if h == 1 && t < m1 then e(t+1) = y_(t+1) - sum             // update the next element in the error vector
        end for
        yd                                                              // return forecasts for each horizon
    end forecast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points at horizon h (h-steps ahead).
     *  Assign to FORECAST MATRIX yf and return h-step ahead forecast.
     *  Use y_0 for y_t when t < 0 (i.e., assume first value repeats back in time).
     *  Note, `predictAll` provides predictions for h = 1.
     *  @see `forecastAll` method in `Forecaster` trait.
     *  FIX -- replace m1 cutoff with e values from pseudo-shocks (differences per horizon forecasts)
     *  @param yf  the forecast matrix (time x horizons)
     *  @param y_  the actual values to use in making forecasts
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    override def forecastAt (yf: MatrixD, y_ : VectorD, h: Int): VectorD =
        if h < 2 then flaw ("forecastAt", s"horizon h = $h must be at least 2")
        val h1 = h - 1                                                  // start pulling values for column h1
        val m1 = e.dim - 1

        var sum = δ
        for j <- 0 until p do              sum += φ(j) * yf(max0 (h1-1-j), max0 (h1-j))
        for j <- 0 until q if h1-j >= 0 do sum += θ(j) * e(h1-j)
        yf(h1, h) = sum                                                     // first forecast is special case

        for i <- y_.indices do                                              // make forecasts over all time points for horizon k
            val t1 = i + h1                                                 // time point prior to horizon
            var sum = δ + rdot (φ, yf, t1, h1)                              // intercept + AR terms
            for j <- 0 until q if t1-j in (0, m1) do sum += θ(j) * e(t1-j)  // add MA terms (shocks)
            yf(i+h, h) = sum                                                // forecast down the diagonal
        end for
        yf(?, h)                                                            // return the h-step ahead forecast vector
    end forecastAt

end ARMA


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARMA` companion object provides factory methods for the `ARMA` class.
 */
object ARMA:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARMA` object.
     *  @param y       the response vector (time series data)
     *  @param tt      the time vector, if relevant (time index may suffice)
     *  @param hparam  the hyper-parameters
     */
    def apply (y: VectorD, tt: VectorD = null, hparam: HyperParameter = SARIMA.hp): ARMA = 
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

    import SARIMA.hp

    println (s"hp = $hp")

    val y = makeTSeries ()                                            // create simulated time-series (see `Stationary`)

    banner (s"Test Predictions: ARMA(1, 1) on simulated time-series")
    var mod = new ARMA (y)                                            // create model for time series data ARMA(1, 1)
    mod.trainNtest ()()                                               // train and test on full dataset

    banner (s"Test Predictions: AR1MA(1, 0) on simulated time-series")
    hp("q") = 0
    mod = new ARMA (y)                                                // create model for time-series data AR1MA(1, 0)
    mod.trainNtest ()()                                               // train and test on full dataset

    banner ("Select model based on ACF and PACF")
    mod.plotFunc (mod.acF, "ACF")                                     // Auto-Correlation Function (ACF)
    mod.plotFunc (mod.pacF, "PACF")                                   // Partial Auto-Correlation Function (PACF)

end aRMATest

import forecasting.Example_LakeLevels.y

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRMATest2` main function tests the `ARMA` class on real data:  Forecasting lake levels.
 *  Test predictions (one step ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  @see `aR1MATest2`
 *  > runMain scalation.modeling.forecasting.aRMATest2
 */
@main def aRMATest2 (): Unit =

    import SARIMA.hp

    for p <- 1 to 5; q <- 0 to 2 do
        hp("p") = p; hp("q") = q                                       // set p (AR) and q (MA) hyper-parameters
        val mod = new ARMA (y)                                         // create model for time-series data AR1MA(p, q)
        banner (s"Test Predictions: ${mod.modelName} on LakeLevels Dataset")
        mod.trainNtest ()()                                            // train and test the model on full dataset
    end for

end aRMATest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRMATest3` main function tests the `ARMA` class on real data:  Forecasting lake levels.
 *  Test forecasts (1 to h steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRMATest3
 */
@main def aRMATest3 (): Unit =

    val hh = 2                                                        // maximum forecasting horizon

    banner (s"Test Forecasts: ARMA(1, 1) on LakeLevels Dataset")
    val mod = new ARMA (y)                                            // create model for time series data ARMA(1, 1)
    val (yp, qof) = mod.trainNtest ()()                               // train and test the model on full dataset

    mod.forecastAll (y, hh)                                           // forecast h-steps ahead (h = 1 to hh) for all y
    Forecaster.evalForecasts (mod, y, hh, true)                       // prediction interval forecasts

end aRMATest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRMATest4` main function tests the `ARMA` class on real data:  Forecasting lake levels.
 *  Test forecasts (1 to h steps ahead forecasts) for several values of p and q.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRMATest4
 */
@main def aRMATest4 (): Unit =

    import SARIMA.hp

//    val hh = 2                                                        // maximum forecasting horizon
 
    for p <- 1 to 5; q <- 0 to 2 do
        hp("p") = p; hp("q") = q                                      // set p (AR) and q (MA) hyper-parameters
        val mod = new ARMA (y)                                        // create model for time series data
        banner (s"Test: ${mod.modelName} on LakeLevels Dataset")
        val (yp, qof) = mod.trainNtest ()()                           // train and test the model on full dataset

/*
        val yf = mod.forecastAll (y, hh)                              // forecast h-steps ahead for all y
        Forecaster.evalForecasts (mod, y, hh)
*/
    end for

end aRMATest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRMATest5` main function tests the `ARMA` class on real data:  Forecasting lake levels.
 *  This test looks at the velocity series (first differences).
 *  Test predictions (one step ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  @see `aR1MATest4`
 *  > runMain scalation.modeling.forecasting.aRMATest5
 */
@main def aRMATest5 (): Unit =

    import SARIMA.hp
    import ARIMA_diff.{Δ, backform}

    val hh = 2                                                        // maximum forecasting horizon

    val v  = Δ (y)                                                    // velocity series (first differences)
    val t  = VectorD.range (0 until v.dim)
    val t2 = VectorD.range (1 until v.dim)
    val tf = new TestFit (v.dim)

    var mod: ARMA = null
    for p <- 2 to 2; q <- 0 to 0 do
        hp("p") = p; hp("q") = q
        mod = new ARMA (v)                                            // create model for time series data ARMA(1, 0)
        banner (s"Test: ${mod.modelName} on Differenced LakeLevels Dataset")
        val (vp, qof) = mod.trainNtest ()()                           // train the model on full dataset

        banner ("Diagnose prediction vp")
        println (tf.testDiagnose (v, vp))                             // diagnose v-predicted
        new Plot (t, v, vp, "v, vp vs. t", lines = true)

        val vf = mod.forecastAll (v, hh)                              // forecast h-steps ahead for all v
        println (s"vf.dims = ${vf.dims}, v.dim = ${v.dim}, vp.dim = ${vp.dim}")
        println (s"vf = $vf")                                         // forecast matrix on v-values
        println (s"v  = $v")                                          // actual v-values
        println (s"vp = $vp")                                         // one-step predicted v-values

        banner ("Diagnose forecasts vh1, vh2")
        val vh1 = vf(?, 1)(0 until v.dim)
        val vh2 = vf(?, 2)(1 until v.dim)
        println (tf.testDiagnose (v, vh1))                            // diagnose v-forecast @ h = 1 -- vh1
        println (tf.testDiagnose (v.drop (1), vh2))                   // diagnose v-forecast @ h = 2 -- vh2
        new Plot (t, v, vh1, "v, vh1 vs. t", lines = true)
        new Plot (t2, v.drop (1), vh2, "v, vh2 vs. t", lines = true)

        banner ("Diagnose prediction on original scale yp")
        val yp = backform (vp, y)                                     // predictions on original scale
        println (MatrixD (y, yp).transpose)
        println (tf.testDiagnose (y, yp))                             // diagnose y-predicted
        new Plot (null, y, yp, "y, yp vs. t", lines = true)

//      Forecaster.evalForecasts (mod, v, hh)
    end for

end aRMATest5


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRMATest6` main function tests the `ARMA` class on real data:
 *  Forecasting COVID-19.
 *  > runMain scalation.modeling.forecasting.aRMATest6
 */
@main def aRMATest6 (): Unit =

    import SARIMA.hp

    val data = MatrixD.load ("covid_19.csv", 1, 1)                    // skip first row (header) and first column
    val yy   = data(?, 4)                                             // column 4 is daily deaths
//  val yy   = data(?, 5)                                             // column 5 is daily deaths smoothed
    val is   = yy.indexWhere (_ >= 2.0)                               // find day of first death with at least 2 deaths
    println (s"is = $is is first day with at least 2 deaths")
    val y    = yy(is until yy.dim)                                    // slice out days before is

//  val h = 1                                                         // forecasting horizon
    for p <- 1 to 4; q <- 0 to 2 do                                   // ARMA hyper-parameter settings
        hp("p") = p; hp("q") = q
        val mod = new ARMA (y)                                        // create an ARMA model
        val (yp, qof) = mod.trainNtest ()()                           // train and the model on full dataset

/*
        val yfa = mod.forecastAll (y, h)
        val yf  = yfa(?, h)                                           // forecasted values - h steps ahead
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

    val y  = forecasting.Example_Covid.loadData_y ("new_deaths")
    val hh = 3                                                        // maximum forecasting horizon

    println (s"y.dim = ${y.dim}")

    var mod: ARMA = null
    for p <- 1 to 12 do                                               // autoregressive hyper-parameter p
        SARIMA.hp("p") = p                                            // set p hyper-parameter
        banner (s"Test: ARMA($p) on Covid-19 Weekly Dataset")
        mod = new ARMA (y)                                            // create model for time series data
        mod.trainNtest ()()                                           // train and test on full dataset

        mod.forecastAll (y, hh)                                       // forecast h-steps ahead for all y
        Forecaster.evalForecasts (mod, y, hh)
    end for

end aRMATest7

