
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Feb  9 20:23:15 EST 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Seasonal Auto-Regressive, Integrated, Moving Average, with eXogenous variables (SARIMAX)
 *
 *  @see http://en.wikipedia.org/wiki/Autoregressive%E2%80%93moving-average_model
 *  @see http://www.emu.edu.tr/mbalcilar/teaching2007/econ604/lecture_notes.htm
 *  @see http://www.stat.berkeley.edu/~bartlett/courses/153-fall2010
 *  @see http://www.princeton.edu/~apapanic/ORFE_405,_Financial_Time_Series_%28Fall_2011%29_files/slides12-13.pdf
 */

//  U N D E R   D E V E L O P M E N T

package scalation
package modeling
package forecasting

import scala.math.sqrt

import scalation.mathstat._
import scalation.optimization.BFGS
import scalation.random.{Normal, Uniform}

import SARIMA._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SARIMAX` companion object provides hyper-parameters for SARIMAX models
 *  and their subtypes.
 */
object SARIMAX:

    /** Base hyper-parameter specification for the `SARIMAX` class its subtypes
     */
    val hp = new HyperParameter
    hp += ("p", 1, 1)             // number of Auto-Regressive (AR) parameters
    hp += ("d", 1, 1)             // number of Differences to take
    hp += ("q", 1, 1)             // number of Moving-Average (MA) parameters
    hp += ("P", 1, 1)             // number of Seasonal Auto-Regressive (AR) parameters
    hp += ("D", 1, 1)             // number of Seasonal Differences to take
    hp += ("Q", 1, 1)             // number of Seasonal Moving-Average (MA) parameters
    hp += ("s", 7, 7)             // length of the Seasonal Period
    hp += ("a", 1, 1)             // the first lag for the eXogenous variables
    hp += ("b", 2, 2)             // the last lag for the eXogenous variables

end SARIMAX


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SARIMAX` class provides basic time series analysis capabilities for Auto-
 *  Regressive AR Integrated I Moving-Average MA, with eXogenous variables X models.
 *  In a SARIMAX(p, d, q)x(P, D, Q)_s [a, b] model,
 *  p and q are the orders of the AR and MA components; d is the order of differencing.
 *  P and Q are the orders of the AR and MA seasonal components; d is the order of Seasonal differencing.
 *  s is the seasonal period, and [a, b] is the range of lags for the eXogenous variables.
 *------------------------------------------------------------------------------
 *  @param y       the original endogenous input vector (time series data)
 *  @param x       the exogenous time series data as an input matrix
 *  @param dd      the order of seasonal differencing
 *  @param period  the seasonal period (at least 2)
 *  @param tt      the time vector, if relevant (time index may suffice)
 *  @param hparam  the hyper-parameters
 */
class SARIMAX (y: VectorD, x: MatrixD, dd: Int = 0, period: Int = 2,
              tt: VectorD = null, hparam: HyperParameter = SARIMAX.hp)
      extends SARIMA (y, dd, period, tt, hparam):

    private val flaw  = flawf ("SARIMAX")                         // flaw function

    if period < 2 then flaw ("init", "the seasonal period must be at least 2")

    private var pp = 0                                           // seasonal AR order
    private var qq = 0                                           // seasonal MA order, @see ARIMA for (p, d, q)
    private var z  = VectorD.nullv                               // time series after differencing
    private var z_ = VectorD.nullv                               // intermediate results after simple differencing
    private var zp = VectorD.nullv                               // predicted values prior to undifferencing/uncentering
    private var φφ = VectorD.nullv                               // seasonal AR(pp) coefficients
    private var θθ = VectorD.nullv                               // seasonal MA(qq) coefficients

    differenced = d > 0 || dd > 0                                // flag indicating whether differencing will be applied
    init (y)                                                     // initialize vectors and parameters

    println (s"x.dims = ${x.dims}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the model name including its current hyper-parameter.
     */
    modelName = if dd > 0 then s"SARIMAX ($p, $d, $q) x ($pp, $dd, $qq)_${period}"
                else s"SARIMAX ($p, $d, $q)"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum lag used by this model (its capacity to look into the past).
     */
    override def cap: Int = Array (p, q, pp*period, qq*period).max

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Initialize variables based on the working time-series v.
     *  Set/change the working time series.  May be used to set the time series
     *  to a different time window in order to produce newer forecast.
     *  @param v  the working vector/time-series
     */
    protected override def init (v: VectorD): Unit =
        mu     = v.mean                                          // sample mean
        val zz = difference (v, d, dd, period)                   // difference (simple/seasonal) the time series
        z      = zz._1                                           // time series after differencing
        z_     = zz._2                                           // intermediate results after simple differencing
        zp     = new VectorD (z.dim)                             // predicted values prior to undifferencing/uncentering
//      e      = new VectorD (z.dim)                             // vector of errors/residuals
        sig2   = z.variance                                      // sample variance
    end init

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set values for the models orders p, q, pp and qq.
     *  @param pq  the vector of model orders
     */
    override def setPQ (pq: VectorI): Unit =
        val n = pq.dim
//      if n > 0 then p  = pq(0)
        φ  = new VectorD (p)                                     // AR coefficients
//      if n > 1 then q  = pq(1)
        θ  = new VectorD (q)                                     // MA coefficients
        if n > 2 then pp = pq(2)
        φφ = new VectorD (pp)                                    // seasonal AR coefficients
        if n > 3 then qq = pq(3)
        θθ = new VectorD (qq)                                    // seasonal MA coefficients
        params = p + q + pp + qq + (if differenced then 0 else 1)   // number of parameters
    end setPQ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `SARIMAX` model to the times-series data in vector y_.  Must call setPQ first.
     *  Estimate the coefficient vectors doer a SARIMAX(p, d, q, P, D, Q)_s model.
     *  It uses BFGS, a Quasi-Newton optimizer, to minimize the negative log-likelihood.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the training/full response vector
     */
    override def train (x_null: MatrixD, y_ : VectorD): Unit =
        val solver = new BFGS (nll)                              // nonlinear optimizer
        val b      = new VectorD (params + 1)                    // parameter values

        if ! differenced then b(b.size-2) = mu                   // sample mean, initial est. for μ parameter
        b(b.size-1) = sqrt (sig2)                                // sample standard deviation, initial est. for σ parameter
        solver.solve (b)                                         // find b that maximizes likelihood

        δ = μ * (1 - φ.sum)                                      // update drift value
//      δ = stats.mu * (1 - φ.sum)

        showParameterEstimates ()                                // show ARMA parameters
        println (s"θθ  = $θθ")                                   // seasonal moving average coefficients
        println (s"φφ  = $φφ")                                   // seasonal auto-regressive coefficients
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The negative log-likelihood function to be minimized.
     *  @see spia.uga.edu/faculty_pages/monogan/teaching/ts/Barima.pdf
     *  @see stats.stackexchange.com/questions/77663/arima-estimation-by-hand
     *  @param b  the input parameter vector
     */
    protected override def nll (b: VectorD): Double =
        if b.size != params + 1 then flaw ("nll", "input parameter vector size incorrect")
        for i <- 0 until p do               φ(i)        = b(i)
        for i <- p until p+pp do           φφ(i-p)      = b(i)
        for i <- p+pp until p+pp+q do       θ(i-p-pp)   = b(i)
        for i <- p+pp+q until p+pp+q+qq do θθ(i-p-pp-q) = b(i)
        if ! differenced then μ = b(b.size-2)
        σ2 = b.last~^2

        updateFittedValues ()
    end nll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the vector of fitted values zp, the vector of errors e, and
     *  return the negative log-likelihood -ll.
     *  @see `Fit` for definition of ll.
     */
    protected override def updateFittedValues (): Double =
        if ! differenced then for i <- z.indices do z(i) = y(i) - μ    // for undifferenced time series, center using est. μ
 
        zp(0) = z(0)                                             // no past values or errors => copy actual
        for t <- 1 until zp.dim do
            e(t-1)  = z(t-1) - zp(t-1)                           // error in previous forecast
            var sum = 0.0
            for j <- 0 until p  if t-j > 0 do             sum +=  φ(j) * z(t-1-j)
            for j <- 0 until pp if t-(1+j)*period >= 0 do sum += φφ(j) * z(t-(1+j)*period)
            for j <- 0 until q  if t-j > 0 do             sum +=  θ(j) * e(t-1-j)
            for j <- 0 until qq if t-(1+j)*period >= 0 do sum += θθ(j) * e(t-(1+j)*period)
            zp(t) = sum
        end for

        -ll (e.normSq / m, σ2, m)                                // return negative log likelihood
    end updateFittedValues

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameter vector (concatenation of φ, θ, φφ and θθ).
     */
    override def parameter: VectorD = φ ++ θ ++ φφ ++ θθ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of predicted/fitted values on the training/full data.
     *  Based on zp calculated in the updateFittedValues method.
     *  @param y_  the given time-series
     */
    override def predictAll (y_ : VectorD): VectorD =
        if differenced then transformBack (zp, z_, y, d, dd, period) else zp + μ
    end predictAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce h-steps-ahead forecast for SARIMAX models.
     *  @see ams.sunysb.edu/~zhu/ams586/Forecasting.pdf
     *  @param t  the time point from which to make forecasts (in the original scale)
     *  @param h  the number of steps to forecast, must be at least one
     */
//  override def forecast (t: Int = y.dim, h: Int = 1): VectorD =         // FIX - adjust to new framework
    override def forecast (t: Int, yf: MatrixD, y_ : VectorD, h: Int): VectorD =
        if t > y.dim then flaw ("forecast", s"t ($t) cannot be greater than y.dim (${y.dim})")
        val tz = t - d - dd * period                             // scale t to match vector z and e
        if tz < cap then flaw ("forecast", s"tz ($tz) must be at least cap ($cap)")

        val zf = new VectorD (cap + h)                           // forecasted centered values
        val e_ = new VectorD (cap + h)                           // available observed errors

        for i <- 0 until cap if tz-cap+i >= 0 do                 // seed with first cap values
            zf(i) = z(tz-cap+i)                                  // copy first cap values
            e_(i) = e(tz-cap+i)                                  // unveil first cap errors (observed in training)
        end for

        for i <- cap until zf.dim do                             // start at t = cap (enough for first value to forecast)
            var sum = 0.0
            for j <- 0 until  p do sum +=  φ(j) * zf(i-1-j)
            for j <- 0 until pp do sum += φφ(j) * zf(i-(1+j)*period)
            for j <- 0 until  q do sum +=  θ(j) * e_(i-1-j)
            for j <- 0 until qq do sum += θθ(j) * e_(i-(1+j)*period)
            zf(i) = sum
        end for

        val f = zf(cap until zf.dim)                             // dump first cap values
        if differenced then transformBackF (f, z_, y, d, dd, period, t)
        else f + μ                                               // return the vector of forecasts
    end forecast

end SARIMAX


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `sARIMAXTest` main function is used to test the `SARIMAX` class.
 *  > runMain scalation.modeling.forecasting.sARIMAXTest
 */
@main def sARIMAXTest (): Unit =

    println ("SARIMAX")
    val m = 100
    val noise = Uniform (-5, 5)
    val y = VectorD (for i <- 0 until m yield i + noise.gen)
    val x = MatrixD ((1, 1), 1)                                // FIX - make a useful matrix

    val mod = new SARIMAX (y, x, 1)                            // time series data: y, x vs. t, apply 1st order differencing

//  mod.plotFunc2 (mod.acf, "ACF")                             // must turn on DEBUG so that ACF is actually computed

    banner (s"Build a SARIMAX(1, 0, 0) model")
    mod.setPQ (VectorI (1, 0))                                 // set p and q, train and evaluate the ARIMA model
    mod.train (null, y)                                        // train the model on full dataset
    val (yp, qof) = mod.test (null, y)                         // test the model on full dataset
    println (mod.report (qof))                                 // report on Quality of Fit (QoF)

    banner (s"Build a SARIMAX(2, 0, 0) model")
    mod.setPQ (VectorI (2, 0))                                 // set p and q, train and evaluate the ARIMA model
    mod.train (null, y)                                        // train the model on full dataset
    val (yp2, qof2) = mod.test (null, y)                       // test the model on full dataset
    println (mod.report (qof2))                                // report on Quality of Fit (QoF)

    banner (s"Build a SARIMAX(1, 0, 1) model")
    mod.setPQ (VectorI (1, 1))                                 // set p and q, train and evaluate the ARIMA model
    mod.train (null, y)                                        // train the model on full dataset
    val (yp3, qof3) = mod.test (null, y)                       // test the model on full dataset
    println (mod.report (qof3))                                // report on Quality of Fit (QoF)

end sARIMAXTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `sARIMAXTest2` main function is used to test the `SARIMAX` class.
 *  > runMain scalation.modeling.forecasting.sARIMAXTest2
 */
@main def sARIMAXTest2 (): Unit =

    val m = 30
    val noise = Normal (0, 2)
    val y = VectorD (for i <- 0 until m yield i + noise.gen)
    val x = MatrixD ((1, 1), 1)                                // FIX - make a useful matrix

    println (s"y = $y")

    val (p, d, q) = (1, 1, 1)
//  val steps = 2                                              // number of steps for the forecasts

    val mod = new SARIMAX (y, x, d)                            // time series data: y, x vs. t

    banner (s"Build a SARIMAX(p, 0, 0) model")
    mod.setPQ (VectorI (p, 0))                                 // set p and q, train and evaluate the ARIMA model
    mod.train (null, y)                                        // train the model on full dataset
    val (yp, qof) = mod.test (null, y)                         // test the model on full dataset
    println (mod.report (qof))                                 // report on Quality of Fit (QoF)

//  val ar_f = ts.forecast (h = steps)
//  println (s"$steps-step ahead forecasts using SARIMAX($p, 0, 0) model = $ar_f")

    banner (s"Build a SARIMAX(0, 0, q) model")
    mod.setPQ (VectorI (0, q))                                 // set p and q, train and evaluate the ARIMA model
    mod.train (null, y)                                        // train the model on full dataset
    val (yp2, qof2) = mod.test (null, y)                       // test the model on full dataset
    println (mod.report (qof2))                                // report on Quality of Fit (QoF)

//  val ma_f = ts.forecast (h = steps)
//  println (s"$steps-step ahead forecasts using SARIMAX(0, 0, $q) model = $ma_f")

    banner (s"Build a SARIMAX(p, 0, q) model")
    mod.setPQ (VectorI (p, q))                                 // set p and q, train and evaluate the ARIMA model
    mod.train (null, y)                                        // train the model on full dataset
    val (yp3, qof3) = mod.test (null, y)                       // test the model on full dataset
    println (mod.report (qof3))                                // report on Quality of Fit (QoF)

//  val arma_f = ts.forecast (h = steps)
//  println (s"$steps-step ahead forecasts using SARIMAX($p, 0, $q) model = $arma_f")

end sARIMAXTest2

import Example_LakeLevels.y

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `sARIMAXTest3` main function is used to test the `SARIMAX` class.
 *  Forecasting lake levels.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.sARIMAXTest3
 */
@main def sARIMAXTest3 (): Unit =

    val x = MatrixD ((1, 1), 1)                                // FIX - make a useful matrix

//  val (pp, dd, qq) = (1, 1, 1)                               // seasonal hyper-parameters
//  val period = 4                                             // seasonal period

//  val d = 0                                                  // apply d-th order differencing - no differencing
    val d = 1                                                  // apply d-th order differencing - first differences

    for h <- 1 to 2 do                                         // forecasting horizon
        for p <- 1 to 3 do                                     // auto-regressive hyper-parameter settings
            for q <- 0 to 2 do                                 // moving-average hyper-parameter settings
                banner (s"Test3: SARIMAX ($p, $d, $q) with h = $h")
                val mod = new SARIMAX (y, x, d)                // create an SARIMAX model
                mod.setPQ (VectorI (p, q))                     // set p and q, train and evaluate the ARIMA model
                mod.train (null, y)                            // train the model on full dataset
                val (yp, qof) = mod.test (null, y)             // test the model on full dataset
                println (mod.report (qof))                     // report on Quality of Fit (QoF)
/*
                val yfa = mod.forecastAll (h)
                val yfb = mod.forecastAll2 (h)
                assert (yfa == yfb)
                val yf  = yfa.col(h)                           // forecasted values - h steps ahead
                val yf2 = yfb.col(h)                           // forecasted 2 values - h steps ahead

                new Plot (null, y, yp, s"Plot of y & yp, predicted SARIMAX ($p, $d, $q) vs. t", true)
                new Plot (null, y, yf, s"Plot of y & yf, forecasted (h = $h) SARIMAX ($p, $d, $q) vs. t", true)
                new Plot (null, y, yf2, s"Plot of y & yf2, forecasted2 (h = $h) SARIMAX ($p, $d, $q) vs. t", true)

                if h == 1 then differ (yp, yf, allow = true)
                differ (yf, yf2, allow = true)
                val skip = max (p, q)                          // skip the cap start-up

                banner (s"SARIMAXTest3: QoF (@h = $h) for yf = mod.forecastAll")
                println (s"rSq (yf)   for h = $h is ${rSqF (y, yf)}")
                println (s"rSq (yf)   for h = $h is ${rSqF (y, yf, max (p, q))}, skip = $skip")
                println (s"sMAPE (yf) for h = $h is ${smapeF (y, yf)}")
                println (s"sMAPE (yf) for h = $h is ${smapeF (y, yf, max (p, q))}, skip = $skip")

                banner (s"SARIMAXTest3: QoF (@h = $h) for yf2 = mod.forecastAll2")
                println (s"rSq (yf2)   for h = $h is ${rSqF (y, yf2)}")
                println (s"rSq (yf2)   for h = $h is ${rSqF (y, yf2, max (p, q))}, skip = $skip")
                println (s"sMAPE (yf2) for h = $h is ${smapeF (y, yf2)}")
                println (s"sMAPE (yf2) for h = $h is ${smapeF (y, yf2, max (p, q))}, skip = $skip")
*/
            end for
        end for
    end for

end sARIMAXTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `sARIMAXTest4` main function is used to test the `SARIMAX` class.
 *  > runMain scalation.modeling.forecasting.sARIMAXTest4
 */
@main def sARIMAXTest4 (): Unit =

    val nfile = "travelTime.csv"
    val data  = MatrixD.load (nfile)

//  val t = data(?, 0)
    val y = data(?, 1)
    val x = MatrixD ((1, 1), 1)                                // FIX - make a useful matrix

    val (p, d, q) = (1, 1, 1)
//  val steps = 1                                              // number of steps for the forecasts

    val mod = new SARIMAX (y, x, d)                            // time series data: y, x vs. t

    println (s"y = $y")

    // Build SARIMAX(p, d, q) models
 
    mod.setPQ (VectorI (p, 0))                                 // set p and q, train and evaluate the ARIMA model
    mod.train (null, y)                                        // train the model on full dataset
    val (yp, qof) = mod.test (null, y)                         // test the model on full dataset
    println (mod.report (qof))                                 // report on Quality of Fit (QoF)

//  val ar_f = ts.forecast (h = steps)
//  println (s"$steps-step ahead forecasts using SARIMAX($p, $d, $q) model = $ar_f")

    mod.setPQ (VectorI (0, q))                                 // set p and q, train and evaluate the ARIMA model
    mod.train (null, y)                                        // train the model on full dataset
    val (yp2, qof2) = mod.test (null, y)                       // test the model on full dataset
    println (mod.report (qof2))                                // report on Quality of Fit (QoF)

//  val ma_f = ts.forecast (h = steps)
//  println (s"$steps-step ahead forecasts using SARIMAX($p, $d, $q) model = $ma_f")

    mod.setPQ (VectorI (p, q))                                 // set p and q, train and evaluate the ARIMA model
    mod.train (null, y)                                        // train the model on full dataset
    val (yp3, qof3) = mod.test (null, y)                       // test the model on full dataset
    println (mod.report (qof3))                                // report on Quality of Fit (QoF)

//  val arma_f = ts.forecast (h = steps)
//  println (s"$steps-step ahead forecasts using SARIMAX($p, $d, $q) model = $arma_f")

end sARIMAXTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `sARIMAXTest5` main function tests the `SARIMAX` class on real data:
 *  Forecasting COVID-19.
 *  > runMain scalation.modeling.forecasting.sARIMAXTest5
 */
@main def sARIMAXTest5 (): Unit =

    import SARIMAX.hp

    val data = MatrixD.load ("covid_19.csv", 1, 1)             // skip first row (header) and first column
    val yy   = data(?, 4)                                      // column 5 is daily deaths
//  val yy   = data(?, 5)                                      // column 5 is daily deaths smoothed
    val is   = yy.indexWhere (_ >= 2.0)                        // find day of first death with at least 2 deaths
    println (s"is = $is is first day with at least 2 deaths")
    val y    = yy(is until yy.dim)                             // slice out days before is
    val x = MatrixD ((1, 1), 1)                                // FIX - make a useful matrix

    val h   = 2                                                // forecasting horizon
    hp("d") = 0                                                // level of differencing, try 0 and 1
    val dd  = 1                                                // level of seasonal differencing, try 0 and 1
    val s   = 7
    for p <- 1 to 15; q <- 1 to 3 do                           // SARIMAX hyper-parameter settings
        hp("p") = p; hp("q") = q
        val mod = new SARIMAX (y, x, dd, s)                    // create an SARIMAX model
        mod.setPQ (VectorI (p, q, 0, 0))
        val (yp, qof) = mod.trainNtest ()()                    // train and the model on full dataset

        val yfa = mod.forecastAll (y, h)
        val yf  = yfa(?, h)                                    // forecasted values - h steps ahead
        new Plot (null, y, yf, s"Plot of y & yf, forecasted (h = $h) ${mod.modelName} vs. t", true)

    end for

end sARIMAXTest5


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `sARIMAXTest6` main function is used to test the `SARIMAX` class.
 *  > runMain scalation.modeling.forecasting.sARIMAXTest6
 */
@main def sARIMAXTest6 (): Unit =

    val m     = 50
    val d     = 0                                              // levels of differencing
//  val steps = 2                                              // number of steps for the forecasts
    val sig2  = 10000.0
    val noise = Normal (0.0, sig2)
    val y = VectorD (for i <- 0 until m yield 40 * (i-1) - (i-2) * (i-2) + noise.gen)
    val x = MatrixD ((1, 1), 1)                                // FIX - make a useful matrix

    val mod = new SARIMAX (y, x, d)                            // time series model SARIMAX

    banner (s"Build SARIMAX(1, $d, 0) model")
    mod.setPQ (VectorI (1, 0))                                 // train for SARIMAX(1, d, 0) model
    mod.train (null, y)                                        // train the model on full dataset
    val (yp, qof) = mod.test (null, y)                         // test the model on full dataset
    println (mod.report (qof))                                 // report on Quality of Fit (QoF)

    for p <- 1 to 3 do
        banner (s"Build SARIMAX($p, $d, $p) model")
        mod.setPQ (VectorI (p, p))                             // retrain for SARIMAX(p, d, p) model
        mod.train (null, y)                                    // train the model on full dataset
        val (yp, qof) = mod.test (null, y)                     // test the model on full dataset
        println (mod.report (qof))                             // report on Quality of Fit (QoF)

//      banner ("Make Forecasts")
//      val yf = mod.forecast (steps)
//      println (s"$steps-step ahead forecasts using SARIMAX($p, $d, $p) model = $yf")
    end for

end sARIMAXTest6

