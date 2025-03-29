
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Hao Peng, John Miller, Michael Cotterell
 *  @version 2.0
 *  @date    Sat Jun 13 01:27:00 EST 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Auto-Regressive (AR)
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

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AR` class provides basic time series analysis capabilities for Auto-Regressive
 *  (AR) models.  In an AR(p) model, p refers to the order of the Auto-Regressive
 *  components of the model.  AR models are often used for forecasting.
 *  Given time series data stored in vector y, its next value y_t+1 = y(t+1)
 *  may be predicted based on past values of y:
 *
 *      y_t+1 = δ + Σ[φ_j y_t-j] + e_t+1
 *
 *  where δ is a constant, φ is the auto-regressive coefficient vector,
 *  and e_t+1 is the new residual/error term.
 *  @param y       the response vector (time series data)
 *  @param tt      the time vector, if relevant (time index may suffice)
 *  @param hparam  the hyper-parameters
 */
class AR (y: VectorD, tt: VectorD = null, hparam: HyperParameter = SARIMA.hp)
      extends Forecaster (y, tt, hparam)
         with Correlogram (y)
         with Fit (dfm = hparam("p").toDouble, df = y.dim - hparam("p").toDouble):

    private val debug  = debugf ("AR", true)                           // debug function
    private val flaw   = flawf ("AR")                                  // flaw function
    private val p      = hparam("p").toInt                             // p-th order Auto-Regressive model
    private var φ      = VectorD.nullv                                 // AR(p) parameters/coefficients
    private var δ      = NO_DOUBLE                                     // drift/intercept/constant term
    private val pnq    = p                                             // sum of # parameters
    private var calPhi = true                                          // calculate phi vector - not externally supplied

    if p > MAX_LAGS then flaw ("init", s"p = $p must not be greater than MAX_LAGS = $MAX_LAGS")

    modelName = s"AR($p)"

    debug ("init", s"$modelName")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Allow users to set the parameter/coefficient vector the values they want, e.g.,
     *  custom values or from another optimizer.
     *  @param phi  the custom values for the parameter vector
     */
    def setPhi (phi: VectorD): Unit =
        φ = phi
        calPhi = false
    end setPhi

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `AR` model to the times-series data in vector y_.
     *  Estimate the coefficient vector φ for a p-th order Auto-Regressive AR(p) model.
     *  Uses Durbin-Levinson Algorithm (in `Correlogram`) to determine the coefficients.
     *  The φ vector is p-th row of psi matrix (ignoring the first (0th) column).
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the training/full response vector (e.g., full y)
     */
    def train (x_null: MatrixD, y_ : VectorD): Unit = 
        m = y_.dim                                                     // length of relevant time series
        resetDF (pnq, m - pnq)                                         // reset the degrees of freedom
        makeCorrelogram (y_)                                           // correlogram computes psi matrix
        if calPhi then φ = psiM(p)(1 until p+1)                        // coefficients = p-th row, columns 1, 2, ... p
        δ = statsF.mu * (1 - φ.sum)                                    // compute drift/intercept
        debug ("train", s"parameters for AR($p) model: φ = $φ, δ = $δ")
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test PREDICTIONS of an AR forecasting model y_ = f(lags (y_)) + e
     *  and return its predictions and  QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train before test.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the testing/full response/output vector
     */
    override def test (x_null: MatrixD, y_ : VectorD): (VectorD, VectorD) =
        val (yp, no_qof) = super.test (null, y_)                        // call super.test for predictions
        resetDF (pnq, y_.dim - pnq)                                     // reset the degrees of freedom
        (yp, diagnose (y_, yp))                                         // return predictions and QoF vector
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test FORECASTS of an AR forecasting model y_ = f(lags (y_)) + e
     *  and return its forecasts and QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train and forecastAll before testF.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the testing/full response/output vector
     */
    def testF (h: Int, y_ : VectorD): (VectorD, VectorD, VectorD) =
        val (yy, yfh) = testSetupF (y_, h)                             // get and align actual and forecasted values
        resetDF (pnq, yy.dim - pnq)                                    // reset the degrees of freedom
        println (s"testF: yy.dim = ${yy.dim}, yfh.dim = ${yfh.dim}")
//      Forecaster.differ (yy, yfh)                                    // uncomment for debugging
        (yy, yfh, diagnose (yy, yfh))                                  // return aligned actual, forecasted and QoF vectors
    end testF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameter vector for the AR(p) model.
     */
    override def parameter: VectorD = φ :+ δ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t+1 using the 1-step ahead forecast.
     *
     *      y_t+1 = δ +  φ_0 y_t + φ_1 y_t-1 + ... + φ_p-1 y_t-(p-1)
     *
     *  When t-j is negative, use y_0.
     *  @see `predictAll` method in `Forecaster` trait.
     *  @see `rdot` in Forecaster.scala for reverse dot product implementation.
     *  @param i   the time series index from which to make prediction
     *  @param y_  the actual time series values to use in making predictions (has one backcast)
     */
    def predict (i: Int, y_ : VectorD): Double = δ + rdot (φ, y_, i)

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
            val t1  = t + k - 1                                        // time point prior to horizon
            val sum = δ + rdot (φ, yf, t1, k-1) 
            yf(t+k, k) = sum                                           // forecast down the diagonal
            yd(k-1)    = sum                                           // record diagonal values
        end for
        yd                                                             // return forecasts for each horizon
    end forecast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points at horizon h (h-steps ahead).
     *  Assign to FORECAST MATRIX yf and return h-step ahead forecast.
     *  Use y_0 for y_t when t < 0 (i.e., assume first value repeats back in time).
     *  Note, `predictAll` provides predictions for h = 1.
     *  @see `forecastAll` method in `Forecaster` trait.
     *  @param yf  the forecast matrix (time x horizons)
     *  @param y_  the actual values to use in making forecasts
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecastAt (yf: MatrixD, y_ : VectorD, h: Int): VectorD =
        if h < 2 then flaw ("forecastAt", s"horizon h = $h must be at least 2")
        val h1 = h - 1                                                 // start pulling values for column h1

        var sum = δ
        for j <- 0 until p do sum += φ(j) * yf(max0 (h1-1-j), max0 (h1-j))
        yf(h1, h) = sum                                                // first forecast is special case

        for i <- y_.indices do                                         // make forecasts over all time points for horizon h
            val t1 = i + h1                                            // time point prior to horizon
            yf(i+h, h) = δ + rdot (φ, yf, t1, h1)                      // forecast down the diagonal
        end for
        yf(?, h)                                                       // return the h-step ahead forecast vector
    end forecastAt

end AR


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AR` companion object provides factory methods for the `AR` class.
 *  Use `SARIMA` for hyper-parameters.
 */
object AR:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `AR` object.
     *  @param y       the response vector (time series data)
     *  @param tt      the time vector, if relevant (time index may suffice)
     *  @param hparam  the hyper-parameters
     */
    def apply (y: VectorD, tt: VectorD = null, hparam: HyperParameter = SARIMA.hp): AR = 
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

    val y = makeTSeries ()                                             // create simulated time series (see `Stationary`)

    banner (s"Test Predictions: AR(1) on simulated time series")
    val mod = new AR (y)                                               // create model for time series data AR(1)
    mod.trainNtest ()()                                                // train and test on full dataset

    banner ("Select model based on ACF and PACF")
    mod.plotFunc (mod.acF, "ACF")                                      // Auto-Correlation Function (ACF)
    mod.plotFunc (mod.pacF, "PACF")                                    // Partial Auto-Correlation Function (PACF)

end aRTest

import forecasting.Example_LakeLevels.y

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

    val hh = 2                                                         // maximum forecasting horizon

    banner (s"Test Forecasts: AR(1) on LakeLevels Dataset")
    val mod = new AR (y)                                               // create model for time series data AR(1)
    val (yp, qof) = mod.trainNtest ()()                                // train and test on full dataset

    mod.forecastAll (y, hh)                                            // forecast h-steps ahead (h = 1 to hh) for all y
    Forecaster.evalForecasts (mod, y, hh)                              // same as code below, except MASE

    for h <- 1 to hh do
        val (yy, yfh, qof) = mod.testF (h, y)                          // h-steps ahead forecast and its QoF
        println (s"Evaluate QoF for horizon $h:")
        println (FitM.fitMap (qof, qoF_names))                         // evaluate h-steps ahead forecasts
        println (s"Fit.mae (y, yfh, h)  = ${Fit.mae (y, yfh, h)}")     // evaluate h-steps ahead forecasts with MAE
        println (s"Fit.mae_n (y, 1)     = ${Fit.mae_n (y, 1)}")        // evaluate h-steps ahead forecasts with MAE_n
        println (s"Fit.mase (y, yfh, h) = ${Fit.mase (y, yfh, h)}")    // evaluate h-steps ahead forecasts with MASE

        val (low, up) = mod.forecastAtI (yy, yfh, h)                   // prediction interval forecasts
        val qof_all   = mod.diagnose_ (yy, yfh, low, up)               // fully evaluate h-steps ahead forecasts
        mod.show_interval_forecasts (yy, yfh, low, up, qof_all, h)
    end for

end aRTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRTest4` main function tests the `AR` class on real data:  Forecasting lake levels.
 *  Test forecasts (1 to h steps ahead forecasts).  Try multiple values for p.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRTest4
 */
@main def aRTest4 (): Unit =

    val hh = 2                                                         // maximum forecasting horizon
 
    var mod: AR = null
    for p <- 1 to 7 do                                                 // autoregressive hyper-parameter p
        SARIMA.hp("p") = p                                             // set p hyper-parameter
        banner (s"Test: AR($p) on LakeLevels Dataset")
        mod = new AR (y)                                               // create model for time series data
        mod.trainNtest ()()                                            // train and test on full dataset

        mod.forecastAll (y, hh)                                        // forecast h-steps ahead for all y
        Forecaster.evalForecasts (mod, y, hh)
    end for

end aRTest4
 

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRTest5` main function tests the `AR` class on small dataset.
 *  Test forecasts (h = 1 step ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRTest5
 */
@main def aRTest5 (): Unit =

    val y  = VectorD (1, 3, 4, 2, 5, 7, 9, 8, 6, 3)

    val mod = new AR (y)                                              // create model for time series data
    banner (s"In-ST Forecasts: ${mod.modelName} on a Small Dataset")
    mod.trainNtest ()()                                               // train and test on full dataset
    println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")

end aRTest5


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRTest6` main function tests the `AR` class on real data:  Forecasting Weekly Covid-19.
 *  Test forecasts (1 to h steps ahead forecasts).  Try multiple values for p.
 *  > runMain scalation.modeling.forecasting.aRTest6
 */
@main def aRTest6 (): Unit =

    val y  = forecasting.Example_Covid.loadData_y ("new_deaths")
    val hh = 4                                                         // maximum forecasting horizon

    println (s"y.dim = ${y.dim}")

    var mod: AR = null
    for p <- 1 to 12 do                                                // autoregressive hyper-parameter p
        SARIMA.hp("p") = p                                             // set p hyper-parameter
        banner (s"Test: AR($p) on Covid-19 Weekly Dataset")
        mod = new AR (y)                                               // create model for time series data
        mod.trainNtest ()()                                            // train and test on full dataset

        mod.forecastAll (y, hh)                                        // forecast h-steps ahead for all y
        Forecaster.evalForecasts (mod, y, hh)
    end for

end aRTest6


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRTest7` main function tests the `AR` class on simulated data.
 *  Test predictions (one step ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRTest7
 */
@main def aRTest7 (): Unit =

    import scala.math.sqrt

    import ActivationFun.f_sigmoid.{fM, dM}
    import neuralnet.{NeuralNet_3L, Optimizer}

    val y = VectorD (1, 2, 4, 7, 9, 8, 6, 5, 3)                        // create a time series by hand

    val m    = y.dim
    val mu_y = y.mean                                                  // mean for full series

    def rho (k: Int): Double =
        var s = 0.0
        var q = 0.0
        for t <- 0 until y.dim-k do
            s += (y(t) - mu_y) * (y(t+k) - mu_y)
        for t <- 0 until y.dim do
            q += (y(t) - mu_y)~^2
        s / q
    end rho

    val yB1  = y(1 until m)                                            // apply back-shift operator
    val yy   = y(0 until m-1)                                          // y clipped to match the size of yB1
    val zz   = yy - yy.mean
    val zB1  = yB1 - yB1.mean
    val r1   = (zz dot zB1) / sqrt ((zz dot zz) * (zB1 dot zB1))       // lag-1 auto-correlation
    println (s"mu_y = $mu_y")
    println (s"zz   = $zz")
    println (s"zB1  = $zB1")
    println (s"r1   = $r1")
    println (s"rho1 = ${rho(1)}")
    println (s"rho2 = ${rho(2)}")

    banner (s"Test Predictions: AR(1) on hand created time series")
    var mod = new AR (y)                                               // create model for time series data AR(1)
    mod.setPhi (VectorD (0.6))                                         // allows coefficients to be user specified
    mod.trainNtest ()()                                                // train and test on full dataset

    banner (s"Test Predictions: AR(2) on hand created time series")
    SARIMA.hp("p") = 2
    mod = new AR (y)                                                   // create model for time series data AR(2)
    mod.trainNtest ()()                                                // train and test on full dataset

    val x = MatrixD ((9, 3), 1, 1, 8,
                             1, 2, 7,
                             1, 3, 6,
                             1, 4, 5,
                             1, 5, 5,
                             1, 6, 4,
                             1, 7, 4,
                             1, 8, 3,
                             1, 9, 2)

    banner (s"Test Predictions: Regression on hand created time series")
    val reg = new Regression (x, y)
    val (yp, qof) =reg.trainNtest ()()                                 // train and test on full dataset
    println (reg.summary ())
    new Plot (null, y, yp, "Regression", lines = true)

    banner (s"Test Predictions: NeuralNet_3L on hand created time series")
    val x_  = x(?, 1 until 3)
    val y_  = MatrixD.fromVector (y)
    val a   = MatrixD.fill (2, 2, 0.1)                                  // weight matrix A
    val b   = MatrixD.fill (2, 1, 0.1)                                  // weight matrix B
    val ab  = VectorD.fill (2)(0.1)                                     // bias vector alpha
    val bb  = VectorD.fill (1)(0.1)                                     // bias vector beta
    val u   = x_ * a + ab                                               // hidden layer pre-activation
    val z   = fM (u)                                                    // hidden layer (use sigmoid)
    val v   = z * b + bb                                                // output layer pre-activation
    val yp_ = v                                                         // output layer (use id)
    val e   = yp_ - y_                                                  // negative error
    val d1  = e *~ dM (v)                                               // delta 1: output -> hidden
    val d0  = (d1 * b.transpose) *~ dM (z)                              // delta 0: hidden -> input

    println (s"u = $u, z = $z, v = $v, yp_ = $yp_, e = $e, d1 = $d1, d0 = $d0")

    Optimizer.hp ("eta") = 1.0
    val nn3 = new NeuralNet_3L (x(?, 1 until 3), MatrixD.fromVector (y), nz = 2)
    val (yq, q0f) = nn3.trainNtest ()()                                // train and test on full dataset
//  val (yq, q0f) = nn3.trainNtest2 ()()                               // train and test on full dataset - auto eta
    nn3.opti.plotLoss ("NeuralNet_3L")
    new Plot (null, y, yq(?, 0), "NeuralNet_3L", lines = true)

    banner ("Select model based on ACF and PACF")
    mod.plotFunc (mod.acF, "ACF")                                      // Auto-Correlation Function (ACF)
    mod.plotFunc (mod.pacF, "PACF")                                    // Partial Auto-Correlation Function (PACF)

end aRTest7

