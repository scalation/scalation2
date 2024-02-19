
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng, John Miller
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
package forecasting

import scala.math.sqrt

import scalation.mathstat._
import scalation.optimization._
import scalation.random.Normal

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Companion object for class `ARIMA`.  Includes features related to differencing
 *  and automated order selection.
 *  @see www.jstatsoft.org/article/view/v027i03/v27i03.pdf
 */
object ARIMA:

    private val flaw  = flawf ("ARIMA")                          // flaw function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the 'd'th difference of the time-series for 'd' in {0, 1, 2, 3}.
     *  A new vector is returned even when there is no difference taken ('d = 0'),
     *  to ensure the original is preserved.
     *  @param y  the original time-series to be differenced
     *  @param d  the order of simple differencing
     */
    def difference (y: VectorD, d: Int): VectorD =
        d match
        case 0 =>
            y.copy
        case 1 =>
            VectorD (for i <- 0 until y.dim-1 yield y(i+1) - y(i))
        case 2 =>
            VectorD (for i <- 0 until y.dim-2 yield y(i+2) - 2*y(i+1) + y(i))
        case 3 =>
            VectorD (for i <- 0 until y.dim-3 yield y(i+3) - 3*y(i+2) + 3*y(i+1) - y(i))
        case _ =>
            flaw ("difference", "ARIMA does not support differencing higher than order 3"); null
        end match
    end difference

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transform the fitted values on the training data of a differenced time series back
     *  to the original scale. Undo trend differencing only.
     *  @see stats.stackexchange.com/questions/32634/difference-time-series-before-arima-or-within-arima
     *  @param yp  the vector of predicted/fitted values
     *  @param y   the original time-series vector
     *  @param d   the order of simple differencing
     */
    def transformBack (yp: VectorD, y: VectorD, d: Int): VectorD =
        d match
        case 0 =>
            yp
        case 1 =>
            val tb = new VectorD (y.dim)
            tb(0) = y(0)
            for i <- 0 until y.dim-1 do tb(i+1) = yp(i) + y(i)
            tb
        case 2 =>
            val tb = new VectorD (y.dim)
            tb(0) = y(0); tb(1) = y(1)
            for i <- 0 until y.dim-2 do tb(i+2) = yp(i) + 2*y(i+1) - y(i)
            tb
        case 3 =>
            val tb = new VectorD (y.dim)
            tb(0) = y(0); tb(1) = y(1); tb(2) = y(2)
            for i <- 0 until y.dim-3 do tb(i+3) = yp(i) + 3*y(i+2) - 3*y(i+1) + y(i)
            tb
        case _ =>
            flaw ("transformBack", "ARIMA does not support differencing higher than order 3"); null
        end match
    end transformBack

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transform the forecasted values of a differenced time series back to the original
     *  for all horizons scale.
     *  @see stats.stackexchange.com/questions/32634/difference-time-series-before-arima-or-within-arima
     *  @param ypa  the matrix of all multi-horizon forecasted values
     *  @param y    the original time-series vector
     *  @param d    the order of simple differencing
     */
    def transformBack_allH (ypa: MatrixD, y: VectorD, d: Int): MatrixD =
        val tb = new MatrixD (ypa.dim, ypa.dim2)
        tb(?, 0) = y
        for k <- 1 until ypa.dim2 do tb(?, k) = transformBack (ypa(?, k), y, d)
        tb
    end transformBack_allH

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Transform the forecast values of a differenced time series back to the
     *  original scale.
     *  @see stats.stackexchange.com/questions/32634/difference-time-series-before-arima-or-within-arima
     *  @param yf  the vector of forecasted values
     *  @param y   the original time series
     *  @param d   the order of simple differencing
     *  @param t   the time point being forecasted (@see the 'forecast' method)
     */
    def transformBackF (yf: VectorD, y: VectorD, d: Int, t: Int): VectorD =
        d match
        case 0 =>
            yf
        case 1 =>
            val tb = y(t - 1 to t) ++ yf
            for i <- 1 until tb.dim do tb(i) += tb(i-1)
            tb(1 to tb.dim)
        case 2 =>
            val tb = y(t-2 to t) ++ yf
            for i <- 2 until tb.dim do tb(i) += (2*tb(i-1) - tb(i-2))
            tb(2 to tb.dim)
        case 3 =>
            val tb = y(t-3 to t) ++ yf
            for i <- 3 until tb.dim do tb(i) += (3*tb(i-1) - 3*tb(i-2) + tb(i-3))
            tb(3 to tb.dim)
        case _ =>
            flaw ("transformBackF", "ARIMA does not support differencing higher than order 3"); null
        end match
    end transformBackF

end ARIMA

import ARIMA._

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
class ARIMA (y: VectorD, tt: VectorD = null, hparam: HyperParameter = SARIMAX.hp)
      extends ARMA (y, tt, hparam):

    private val flaw   = flawf ("ARIMA")                         // flaw function

    protected val d      = hparam("d").toInt                     // the number of differences to take
//  protected var cap    = 0                                     // max of p and q
    protected var params = 0                                     // number of parameters estimated
    protected var differenced = d > 0                            // flag indicating whether differencing will be applied

    protected var mu   = -0.0                                    // sample mean (-0.0 means unassigned)
    protected var μ    = -0.0                                    // population mean estimated using MLE
    protected var sig2 = -0.0                                    // sample variance
    protected var σ2   = -0.0                                    // population variance estimated using MLE

    private var z      = VectorD.nullv                           // vector of centered predicted/fitted values
    private var zp     = VectorD.nullv                           // vector of centered predicted/fitted values

    init (y)                                                     // initialize vectors and parameters

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the model name including current hyper-parameters, e.g., ARIMA(2, 1, 1).
     */
    modelName = s"ARIMA($p, $d, $q)"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Initialize variables based on the working time-series v.
     *  Set/change the working time series.  May be used to set the time series
     *  to a different time window in order to produce newer forecast.
     *  @param v  the working vector/time-series
     */
    protected def init (v: VectorD): Unit =
        mu   = v.mean                                            // sample mean
        z    = difference (v, d)                                 // take the d-th difference of the time series
        zp   = new VectorD (z.dim)                               // predicted values prior to undifferencing/uncentering
//      e    = new VectorD (z.dim)                               // vector of errors/residuals
        sig2 = z.variance                                        // sample variance

        φ = new VectorD (p)                                   // AR coefficients
        θ = new VectorD (q)                                   // MA coefficients
//      cap    = max (p, q)                                   // greatest lag
        params = p + q + (if differenced then 0 else 1)       // number of parameters
    end init

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show estimates for parameters.
     */
    def showParameterEstimates (): Unit =
        println (s"differenced = $differenced")
        println (s"φ    = $φ")                                   // AR parameters
        println (s"θ    = $θ")                                   // MA parameters
        println (s"δ    = $δ")                                   // drift
        println (s"mu   = $mu")                                  // sample mean
        println (s"μ    = $μ")                                   // MLE mean
        println (s"sig2 = $sig2")                                // sample variance
        println (s"σ2   = $σ2")                                  // MLE variance
    end showParameterEstimates

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `ARIMA` model to the times-series data in vector y_.  Must call setPQ first.
     *  Estimate the coefficient vectors φ and θ for (p, q)-th order ARIMA(p, d, q) model.
     *  It uses BFGS, a Quasi-Newton optimizer, to minimize the negative log-likelihood.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the training/full response vector
     */
    override def train (x_null: MatrixD, y_ : VectorD): Unit =
        val optimizer = new BFGS (nll)                           // nonlinear optimizer
        val b = new VectorD (params + 1)                         // parameter values

        if ! differenced then b(b.size-2) = mu                   // sample mean, initial est. for μ parameter
        b(b.size-1) = sqrt (sig2)                                // sample standard deviation, initial est. for σ parameter
        optimizer.solve (b)                                      // find b that maximizes likelihood

        δ = μ * (1 - φ.sum)                                      // update drift value
//      δ = stats.mu * (1 - φ.sum)

        showParameterEstimates ()
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The negative log-likelihood function to be minimized.
     *  @see math.unice.fr/~frapetti/CorsoP/Chapitre_4_IMEA_1.pdf, page 36
     *  @see spia.uga.edu/faculty_pages/monogan/teaching/ts/Barima.pdf
     *  @see stats.stackexchange.com/questions/77663/arima-estimation-by-hand
     *  @param b  the input parameter vector
     */
    protected def nll (b: VectorD): Double =
        if b.size != params + 1 then flaw ("nll", "input parameter vector size incorrect")
        for i <- 0 until p do   φ(i)   = b(i)
        for i <- p until p+q do θ(i-p) = b(i)
        if ! differenced then μ = b(b.size-2)
        σ2 = b.last~^2

        updateFittedValues ()
    end nll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the vector of fitted values 'zp', the vector of errors 'e', and
     *  return the negative log-likelihood '-ll'.
     *  @see `Fit` for definition of 'll'.
     */
    protected def updateFittedValues (): Double =
        if ! differenced then for i <- z.indices do z(i) = y(i) - μ    // for undifferenced time series, center using est. μ

        zp(0) = z(0)                                             // no past values or errors => copy actual
        for t <- 1 until zp.dim do
            e(t-1)  = z(t-1) - zp(t-1)                           // error in previous forecast
            var sum = 0.0
            for j <- 0 until p if t-j > 0 do sum += φ(j) * z(t-1-j)
            for j <- 0 until q if t-j > 0 do sum += θ(j) * e(t-1-j)
            zp(t) = sum
        end for

        -ll (e.normSq / m, σ2, m)                                // return negative log likelihood
    end updateFittedValues

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the error (difference between actual and predicted) and useful
     *  diagnostics for the dataset.
     *  @param y    vector of observed values
     *  @param yp   vector of predicted values
     */
    override def test (x_null: MatrixD, y_ : VectorD): (VectorD, VectorD) =
        // FIX - add testSetup
        val yp = predictAll (y_)
        resetDF (params, y.dim - params)
        (yp, diagnose (y, yp))
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of predicted/fitted values on the training/full dataset.
     *  Based on 'zp' calculated in the 'updateFittedValues' method.
     *  @param y_  the given time-series
     */
    override def predictAll (y_ : VectorD): VectorD =
        if differenced then transformBack (zp, y, d) else zp + μ
    end predictAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce h-steps-ahead forecast for ARIMA models.
     *  @see ams.sunysb.edu/~zhu/ams586/Forecasting.pdf
     *  @param t  the time point from which to make forecasts (in the original scale)
     *  @param h  the number of steps to forecast, must be at least one
     */
    def forecast (t: Int = y.dim, h: Int = 1): VectorD =
        if t > y.dim then flaw ("forecast", s"t ($t) cannot be greater than y.dim (${y.dim})")
        val tz = t - d                                           // scale t to match vector z and e
        if tz < cap then flaw ("forecast", s"tz ($tz) must be at least cap ($cap)")

        val zf = new VectorD (cap + h)                           // forecasted centered values
        val e_ = new VectorD (cap + h)                           // available observed errors

        for i <- 0 until cap if tz-cap+i >= 0 do                 // seed with first cap = max(p, q) values
            zf(i) = z(tz-cap+i)                                  // copy first cap values
            e_(i) = e(tz-cap+i)                                  // unveil first cap errors (observed in training)
        end for
        for i <- cap until zf.dim do                             // start at t = cap (enough for first value to forecast)
            var sum = 0.0
            for j <- 0 until p do sum += φ(j) * zf(i-1-j)
            for j <- 0 until q do sum += θ(j) * e_(i-1-j)
            zf(i) = sum
        end for
        val f = zf(cap to zf.dim)                                // dump first cap values
        if differenced then transformBackF (f, y, d, t)
        else f + μ                                               // return the vector of forecasts
    end forecast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all time points using 1 through h-steps ahead forecasts.
     *  The h-th row of matrix is the horizon h forecast (where h = 0 is actual data).
     *  @param h  the forecasting horizon, number of steps ahead to produce forecasts
     */
    override def forecastAll (y_ : VectorD, h: Int): MatrixD =
        val yf  = new MatrixD (y.dim, h+1)                       // forecasts for all horizons h & time points t
        yf(?, 0) = y                                             // first row is actual values
        val cut = cap + d                                        // cut over from actual to forecasted values

        for t <- y.indices do
            if t < cut then
                for k <- 1 to h do yf(t, k) = y(t)               // copy first cut observed values from y
            else
                val ft = forecast (t, h)                         // forecasts at time point t, horizons 1 to h
                for k <- 1 to h if t+k-1 < y.dim do
                    yf(t+k-1, k) = ft(k-1)                       // place forecasts diagonally
                end for
            end if
        end for

        // fill in blank values in first few rows where no forecasts can be produced by copying values from previous columns
        for k <- 2 to h; t <- cut until cut+k-1 do yf(t, k) = yf(t, k-1)   // copy forecasted values

        yf                                                       // return matrix of forecasted values
    end forecastAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all m time points and all horizons (1 through h-steps ahead).
     *  Record these in the yf matrix, where
     *      yf(t, k) = k-steps ahead forecast for y_t
     *  Note, yf(?, 0) is set to y (the actual time-series values).
     *  Do not forecast errors, rather use observed errors from training and make sure not
     *  to use errors that would correspond to knowing future errors (all future errors should
     *  be assumed to be 0).
     *  @see https://otexts.com/fpp3/arima-forecasting.html, section 9.8
     *  @param h  the maximum forecasting horizon, number of steps ahead to produce forecasts
     */
/***
    override def forecastAll2 (h: Int): MatrixD = forecastAll (h)
    FIX - values must be computed diagonially - does not work for d = 1, etc. (missing value at 'cut')
    override def forecastAll2 (h: Int): MatrixD =
    {
        val zf = new MatrixD (y.dim, h+1)                        // forecast matrix: rows - time, cols - horizon
        for t <- z.indices do zf(t, 0) = z(t)                    // first column is actual values, horizon 0
        val cut = cap + d                                        // cut over from actual to forecasted values
 
        for k <- 1 to h do                                       // loop through k-steps ahead forecasts
            val e_ = new VectorD (z.dim)                         // redetermine errors from a clean slate

            for t <- 0 until cut do                              // seed the first cap = max(p, q) values
                zf(t, k) = z(t)                                  // copy first cap actual values
                e_(t)    = e(t)                                  // copy first cap errors (observed in training)
            end for

            for t <- cut until y.dim do                          // forecast from cap to the end of time-series
                if t-k >= 0 then e_(t-k) = e(t-k)                // unveil previous error at time t-k
                var sum = 0.0
                for j <- 0 until p if t-j > 0 then sum += φ(j) * zf(t-1-j, max (0, k-1-j))
                for j <- 0 until q if t-j > 0 then sum += θ(j) * e_(t-1-j)
                zf(t, k) = sum                                   // centered forecast for time t
            end for
        end for
        println (s"forecastAll2: zf (${zf.dim}) = $zf")
        if differenced then transformBack_allH (zf, y, d)
        else zf + μ                                              // return uncentered forecasts
    end forecastAll2
***/

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Obtain residuals/errors in the original scale.
     */
    def residuals: VectorD = if differenced then y - predictAll (y) else e

end ARIMA


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRIMATest` main function tests the `ARIMA` class on real data:
 *  Forecasting lake levels.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 * > runMain scalation.modeling.forecasting.aRIMATest
 */
@main def aRIMATest (): Unit =

    import Example_LakeLevels.y
    import SARIMAX.hp

    val d = 0                                                    // apply d-th order differencing - no differencing
//  val d = 1                                                    // apply d-th order differencing - first differences

    for h <- 1 to 1 do                                           // forecasting horizon
        for p <- 1 to 7 do                                       // auto-regressive hyper-parameter settings
            for q <- 0 to 2 do                                   // moving-average hyper-parameter settings
                banner (s"Test: ARIMA ($p, $d, $q) with h = $h")
                hp("p") = p; hp("d") = d; hp("q") = q            // set p, d and q for the ARIMA model
                val mod = new ARIMA (y)                          // create an ARIMA model
                mod.trainNtest ()()                              // train the model on full dataset

/*
                val yfa = mod.forecastAll (y, h)
                val yf  = yfa(?, h)                              // forecasted values - h steps ahead
                new Plot (null, y, yf, s"Plot of y & yf, forecasted (h = $h) ${mod.modelName} vs. t", true)

                if h == 1 then Forecaster.differ (yp, yf, allow = true)
                val skip = max (p, q)                            // skip the cap start-up
                banner (s"aRIMATest: QoF (@h = $h) for yf = mod.forecastAll")
                println (s"rSq (yf)   for h = $h is ${rSqF (y, yf)}")
                println (s"rSq (yf)   for h = $h is ${rSqF (y, yf, max (p, q))}, skip = $skip")
                println (s"sMAPE (yf) for h = $h is ${smapeF (y, yf)}")
                println (s"sMAPE (yf) for h = $h is ${smapeF (y, yf, max (p, q))}, skip = $skip")

                banner (s"aRIMATest: QoF (@h = $h) for yf2 = mod.forecastAll2")
                println (s"rSq (yf2)   for h = $h is ${rSqF (y, yf2)}")
                println (s"rSq (yf2)   for h = $h is ${rSqF (y, yf2, max (p, q))}, skip = $skip")
                println (s"sMAPE (yf2) for h = $h is ${smapeF (y, yf2)}")
                println (s"sMAPE (yf2) for h = $h is ${smapeF (y, yf2, max (p, q))}, skip = $skip")
*/
            end for
        end for
    end for

end aRIMATest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRIMATest2` main function tests the `ARIMA` class.
 *  Test simulated data.
 *  > runMain scalation.modeling.forecasting.aRIMATest2
 */
@main def aRIMATest2 (): Unit =

    import SARIMAX.hp

    banner ("ARIMA Test2")
    val m = 20
    val noise = Normal (0, 2)
//  val noise = Uniform (-5, 5)
    val y = VectorD (for i <- 0 until m yield i + noise.gen)

    println (s"y = $y")

    val (p, d, q) = (1, 1, 1)
    hp("p") = p; hp("d") = d; hp("q") = q                        // set p, d and q for the ARIMA model
    val mod = new ARIMA (y)                                      // time series data: y vs. t
    mod.train (null, y)                                          // train the model on full dataset
    val (yp, qof) = mod.test (null, y)                           // test the model on full dataset
    println (mod.report (qof))                                   // report on Quality of Fit (QoF)

end aRIMATest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRIMATest3` main function tests the `ARIMA` class.
 *  Traffic dataset.
 *  > runMain scalation.modeling.forecasting.aRIMATest3
 */
@main def aRIMATest3 (): Unit =

    import SARIMAX.hp

    val nfile = "travelTime.csv"
    val data  = MatrixD.load (nfile)

//  val t = data(?, 0)
    val y = data(?, 1)
    println (s"y = $y")

    val (p, d, q) = (1, 1, 1)
    val steps = 1                                                // number of steps for the forecasts

    hp("p") = p; hp("d") = d; hp("q") = q                        // set p, d and q for the ARIMA model
    val mod = new ARIMA (y)                                      // time series data: y vs. t
    mod.train (null, y)                                          // train the model on full dataset
    val (yp, qof) = mod.test (null, y)                           // test the model on full dataset
    println (mod.report (qof))                                   // report on Quality of Fit (QoF)

    val ar_f = mod.forecast (h = steps)
    println (s"$steps-step ahead forecasts using ${mod.modelName}  model = $ar_f")

end aRIMATest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRIMATest4` main function tests the `ARIMA` class.
 *  Simulated data with a quadratic pattern.
 *  > runMain scalation.modeling.forecasting.aRIMATest4
 */
@main def aRIMATest4 (): Unit =

    import SARIMAX.hp

    val m     = 50
    val (p, d, q) = (1, 1, 1)                                    // hyper-parameters for the ARIMA model
    val steps = 2                                                // number of steps for the forecasts
    val sig2  = 10000.0
    val noise = Normal (0.0, sig2)
    val y = VectorD (for i <- 0 until m yield 40 * (i-1) - (i-2) * (i-2) + noise.gen)

    banner (s"Build ARIMA($p, $d, $q) model")
    hp("p") = p; hp("d") = d; hp("q") = q                        // set p, d and q for the ARIMA model
    val mod = new ARIMA (y)                                      // time series model ARIMA
    mod.train (null, y)                                          // train the model on full dataset
    val (yp, qof) = mod.test (null, y)                           // test the model on full dataset
    println (mod.report (qof))                                   // report on Quality of Fit (QoF)

    banner ("Make Forecasts")
    val yf = mod.forecast (steps)
    println (s"$steps-step ahead forecasts using ${mod.modelName} model = $yf")

end aRIMATest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRIMATest5` main function tests the `ARIMA` class on real data:
 *  Forecasting lake levels.  Select the best number of lags.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRIMATest5
 *
@main def aRIMATest5 (): Unit =

    import Example_LakeLevels.y

    val d = 0                                                    // level of differencing
    val mod = new ARIMA (y)                                      // create model for time series data
    mod.setPQ (VectorI (1, 1))
    mod.train (null, y)                                          // train the model on full dataset
    val (yp, qof) = mod.test (null, y)                           // test the model on full dataset
    println (mod.report (qof))                                   // report on Quality of Fit (QoF)

    val res = mod.forwardSel ()
    println (s"forwardSel: $res")

    for (sp, sq) <- Array ((1, 0), (2, 1), (1, 1), (1, 2), (0, 1)) do
        val res = mod.forwardSel2 (VectorI (sp, sq))
        println (s"forwardSel2: $res")
    end for

end aRIMATest5
 */

