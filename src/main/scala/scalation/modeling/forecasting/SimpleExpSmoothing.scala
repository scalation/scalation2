
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 2.0
 *  @date    Thu Jun 13 13:13:26 EDT 2019
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: Simple Exponential Smoothing (SES)
 *
 *  @see https://otexts.com/fpp2/ses.html
 */

package scalation
package modeling
package forecasting

import scalation.mathstat._
import scalation.optimization.L_BFGS_B
import scalation.random._

import Fit._
import Forecaster.differ
//import RollingValidation.trSize

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleExpSmoothing` class provide very basic time series analysis using
 *  Simple Exponential Smoothing models.  The forecasted value is the weighted average
 *  the latest value y_t and the latest smoothed value s_t.  The smoothing parameter
 *  α in [0, 1] causes the contributions of older values to decay exponentially.
 *  @see Smoothing Equation in section 7.1.
 *      s_t+1  = α y_t + (1 - α) s_t       smoothing equation
 *      yf_t+1 = s_t+1                     forecast equation
 *  where vector s is the smoothed version of vector y.
 *  @param y       the response vector (original time series data)
 *  @param tt      the time vector, if relevant (time index may suffice)
 *  @param hparam  the hyper-parameters
 */
class SimpleExpSmoothing (y: VectorD, tt: VectorD = null, hparam: HyperParameter = SimpleExpSmoothing.hp)
      extends Forecaster (y, tt, hparam)
         with Correlogram (y)
         with Fit (dfm = 1, df = y.dim - 1):

    private val debug = debugf ("SimpleExpSmoothing", true)            // debug function
    private val flaw  = flawf ("SimpleExpSmoothing")                   // flaw function
    private val TOL   = 1E-4                                           // tolerance
    private val lo    = VectorD.fill (1)(0.0)                          // lower bound on α for optimizer
    private val up    = VectorD.fill (1)(1.05)                         // upper bound on α for optimizer (1.0 with some slack)

    private var α     = hparam ("α").toDouble                          // default value for the smoothing parameter
    private var s     = VectorD.nullv                                  // vector of smoothed/leveled values (state)
    private var sf    = VectorD.nullv                                  // to hold smooth values for a forecast horizon
    private var opt   = true                                           // whehther to optimize the smoothing parameter

    modelName = "SimpleExpSmoothing"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the smoothing parameter α.
     *  @param a  the smoothing parameter
     */
    def reset (a: Double): Unit = α = a

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Toggle the opt flag that indicates whether optimization should be used to set α.
     */
    def toggleOpt (): Unit = opt = ! opt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Smooth the time-series data y, returning the leveled/smoothed data l.
     *  May be viewed as unoptimized training.
     *  @see Smoothing Equation in section 7.1.
     *      s_t+1 = α y_t + (1 - α) s_t                                // smoothing equation
     *  @param a   the smoothing parameter (decay rate for older values)
     *  @param y_  the response/output vector (training/full)
     */
    def smooth (a: Double = α, y_ : VectorD = y): VectorD =
        s = new VectorD (y_.dim)
        s(0) = y(0)
        for t <- 0 until y_.dim-1 do s(t+1) = a * y_(t) + (1 - a) * s(t)
        s
    end smooth

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the `SimpleExpSmoothing` model on the time-series data, by finding the value
     *  for the smoothing parameter α that minimizes the sum of squared errors sse.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the response/output vector (training/full)
     */
    def train (x_null: MatrixD, y_ : VectorD): Unit =
 
        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  The objective function to be minimized (sum of squared errors) by `L_BFGS_B`.
         *  @param x  the input vector VectorD (α) to be optimized
         */
        def f_obj (x: VectorD): Double = (y_ - smooth (x(0), y_)).normSq   // only one parameter

        if opt then
            val optimizer = new L_BFGS_B (f_obj, l = lo, u = up)           // Quasi-Newton optimizer
            val opt = optimizer.solve (VectorD (α), toler = TOL)           // optimize value for α
            α = (opt._2)(0)                                                // pull α from vector result
        end if
        s = smooth (α)                                                     // vector of smoothed/predicted values, with optimized α
        println (s" train: diagnose = ${diagnose (y_, s)}")
        debug ("train", s"α = $α")
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test PREDICTIONS of an SES forecasting model y_ = f(lags (y_)) + e
     *  and return its predictions and  QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train before test.
     *  @param x_null  the training/testing data/input matrix (ignored, pass null)
     *  @param y_      the training/testing/full response/output vector
     */
    def test (x_null: MatrixD, y_ : VectorD): (VectorD, VectorD) =
        val (yy, yp) = testSetup (y_)                                  // get and align actual and predicted values
        resetDF (1, y_.dim - 1)                                        // reset the degrees of freedom
        println (s"test: yy.dim = ${yy.dim}, yp.dim = ${yp.dim}")
//      differ (yy, yp)                                                // uncomment for debugging
        (yp, diagnose (yy, yp))                                        // return predictions and QoF vector
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test FORECASTS of an SES forecasting model y_ = f(lags (y_)) + e
     *  and return its forecasts and QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train and forecastAll before testF.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the training/testing/full response/output vector
     */
    def testF (h: Int, y_ : VectorD): (VectorD, VectorD) =
        val (yy, yfh) = testSetupF (y_, h)                             // get and align actual and forecasted values
        resetDF (1, yy.dim - 1)                                        // reset the degrees of freedom
        println (s"testF: yy.dim = ${yy.dim}, yfh.dim = ${yfh.dim}")
//      differ (yy, yfh)                                               // uncomment for debugging
        (yfh, diagnose (yy, yfh))                                      // return predictions and QoF vector
    end testF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameter vector.
     */
    override def parameter: VectorD = VectorD (α)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t+1 using the 1-step ahead forecasts.
     *      y_t+1 = s_t+1
     *  @see predictAll in `Forecaster`
     *  @param t   the time point/index to be predicted
     *  @param y_  the actual values to use in making predictions
     */
    def predict (t: Int, y_ : VectorD): Double = s(t+1)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of predicted values on the training data.
     *  Must call smooth or train first.
     *  @param y_  the actual values to use in making predictions
     */
    override def predictAll (y_ : VectorD): VectorD = s

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
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
            val pred = s(t+k)
            yf(t+k, k) = pred                                          // forecast down the diagonal
            yd (k-1)   = pred                                          // record diagonal values
        end for
        yd                                                             // return forecasts for each horizon
    end forecast

    // FIX - pick one forecast or forecast2

    def forecast2 (t: Int, yf: MatrixD, y_ : VectorD, h: Int): VectorD =
        if h < 1 then flaw ("forecast", s"horizon h = $h must be at least 1")
        val yf = new VectorD (h+1)
        
        yf(0) = if t == 0 then y(0) else s(t-1)
        sf(0) = s(t)
        for k <- 1 to h do
            yf(k) = sf(k-1)
            sf(k) = α * yf(k-1) + (1 - α) * sf(k-1)
        end for
        yf(1 until yf.dim)
    end forecast2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points at horizon h (h-steps ahead).
     *  Assign to forecasting matrix and return h-step ahead forecast.
     *  @param yf  the forecasting matrix (time x horizons)
     *  @param y_  the actual values to use in making forecasts
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecastAt (yf: MatrixD, y_ : VectorD, h: Int): VectorD =
        if h < 1 then flaw ("forecastAt", s"horizon h = $h must be at least 1")
        for t <- y_.indices do                                         // make forecasts over all time points for horizon k
            yf(t+h, h) = sf(h-1)
            sf(h)      = α * yf(t+h-1, h-1) + (1 - α) * sf(h-1)
        end for
        yf(?, h)                                                       // return the h-step ahead forecast vector
    end forecastAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all m time points and all horizons (1 through h-steps ahead).
     *  Record these in the yf matrix, where
     *      yf(t, k) = k-steps ahead forecast for y_t
     *  Note, yf(?, 0) is set to y (the actual time-series values).
     *  FIX - integrate with forecastAt
     *  @param h   the maximum forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the actual values to use in making predictions
     */
//  def forecastAll (h: Int, y_ : VectorD): MatrixD =
    override def forecastAll (y_ : VectorD, h: Int): MatrixD =
        val m = y_.dim
        yf     = new MatrixD (m, h+1)                                    // forecasts for all time points t & horizons to h
        val s_ = new MatrixD (m, h+1)                                    // state values, per time x horizon
        yf(?, 0) = y                                                     // first column is actual values, horizon 0
        s_(?, 0) = s
        for k <- 1 to h do
            yf(0, k) = y(0)                                              // copy first actual value
            s_(0, k) = s(0)
            for t <- 1 until m do                                        // forecast the rest
                 yf(t, k) = s_(t, k-1)
                 s_(t, k) = α * yf(t-1, k-1) + (1 - α) * s_(t-1, k-1)
            end for
            debug ("forecastAll", s"yf(?, $k) = ${yf(?, k)}")
        end for
        yf                                                               // return matrix of forecasted values
    end forecastAll

end SimpleExpSmoothing
 

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleExpSmoothing` companion object provides factory methods for the
 *  `SimpleExpSmoothing` class.
 */
object SimpleExpSmoothing:

    /** Base hyper-parameter specification for `SimpleExpSmoothing`
     */
    val hp = new HyperParameter;
    hp += ("α", 0.9, 0.9)                                        // default value for the smoothing parameter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SimpleExpSmoothing` object.
     *  @param y       the response vector (time series data)
     *  @param tt      the time vector, if relevant (time index may suffice)
     *  @param hparam  the hyper-parameters
     */
    def apply (y: VectorD, tt: VectorD = null, hparam: HyperParameter = hp): SimpleExpSmoothing =
        new SimpleExpSmoothing (y, tt, hparam)
    end apply

end SimpleExpSmoothing


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleExpSmoothingTest` main function tests the `AR` class on simulated data.
 *  Test predictions (one step ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.simpleExpSmoothingTest
 */
@main def simpleExpSmoothingTest (): Unit =

    val y = makeTSeries ()                                             // create simulated time-series (see `Stationary`)

    banner (s"Test Predictions: SimpleExpSmoothing on simulated time-series")
    val mod = new SimpleExpSmoothing (y)                               // create model for time series data AR(1)
    mod.trainNtest ()()                                                // train and test on full dataset

end simpleExpSmoothingTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleExpSmoothingTest2` main function is used to test the `SimpleExpSmoothing` class.
 *  Forecasting lake levels.  Compare AR(1) and SimpleExpSmoothing models for the time series data.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.simpleExpSmoothingTest2
 */
@main def simpleExpSmoothingTest2 (): Unit =

    import Example_LakeLevels.y

    val m  = y.dim
    val hh = 2
   
    banner ("Build AR(1) model")
    val ar1 = new AR (y)                                               // time series model AR(1)
    ar1.trainNtest ()()                                                // train and test on full dataset

    banner ("Build SimpleExpSmoothing model")
    val mod = new SimpleExpSmoothing (y)                               // time series model SimpleExpSmoothing
    val (yp, qof) = mod.trainNtest ()()                                // train and test on full dataset

    banner ("ForecastAll ...")
    val yf = mod.forecastAll (y, hh)                                   // forecast h-steps ahead (h = 1 to hh) for all y
    println (s"y.dim = ${y.dim}, yp.dim = ${yp.dim}, yf.dims = ${yf.dims}")
    println (s"yf = $yf")
    assert (yf(?, 0)(0 until m) == y)                                  // column 0 must agree with actual values
    differ (yf(?, 1)(1 until m), yp)
    assert (yf(?, 1)(1 until m) == yp)                                 // column 1 must agree with one step-ahead predictions

    for h <- 1 to hh do
        banner (s"Forecasts for horizon $h")
        val (yfh, qof) = mod.testF (h, y)                              // h-steps ahead forecast and its QoF
        val yy = y(h until m)                                          // actual response aligned with yfh
        println (s"Evaluate QoF for horizon $h:")
        println (FitM.fitMap (qof, QoF.values.map (_.toString)))       // evaluate h-steps ahead forecasts
        println (s"Fit.mae (y, yfh, h)  = ${Fit.mae (y, yfh, h)}")     // evaluate h-steps ahead forecasts with MAE
        println (s"Fit.mae_n (y, 1)     = ${Fit.mae_n (y, 1)}")        // evaluate h-steps ahead forecasts with MAE_n
        println (s"Fit.mase (y, yfh, h) = ${Fit.mase (y, yfh, h)}")    // evaluate h-steps ahead forecasts with MASE
    end for

/*
    for h <- 1 to 4 do                                                 // h-steps ahead  forecast
        banner (s"Rolling Validation h = $h")
        val stats = SimpleRollingValidation.crossValidate2 (mod, kt_ = 1, h = h)
        Fit.showQofStatTable (stats)
    end for
*/

end simpleExpSmoothingTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleExpSmoothingTest3` main function is used to test the `SimpleExpSmoothing` class.
 *  Test customized smoothing (call smooth) versus optimized smoothing (call train).
 *  > runMain scalation.modeling.forecasting.simpleExpSmoothingTest3
 */
@main def simpleExpSmoothingTest3 (): Unit =

    val m = 50
    val r = Random ()
    val y = VectorD (for i <- 0 until m yield i + 10.0 * r.gen)

    val mod = new SimpleExpSmoothing (y)                         // smooth time series data: y vs. t

    banner ("Customized Simple Exponential Smoothing")
    mod.smooth (0.5)                                             // use customized parameters, don't train
    val (yp, qof) = mod.test (null, y)                           // test the model on full dataset
    println (mod.report (qof))                                   // report on Quality of Fit (QoF)
    println (s"mase = ${Fit.mase (y, yp)}")

    banner ("Optimized Simple Exponential Smoothing")
    mod.train (null, y)                                          // train to use optimal α
    val (yp2, qof2) = mod.test (null, y)                         // test the model on full dataset
    println (mod.report (qof2))                                  // report on Quality of Fit (QoF)
    println (s"mase = ${Fit.mase (y, yp2)}")

end simpleExpSmoothingTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleExpSmoothingTest4` main function is used to test the `SimpleExpSmoothing` class.
 *  Test rolling validation.
 *  > runMain scalation.modeling.forecasting.simpleExpSmoothingTest4
 */
@main def simpleExpSmoothingTest4 (): Unit =

    val m = 50
    val r = Random ()
    val y = VectorD (for i <- 0 until m yield i + 10.0 * r.gen)
    val h = 3
    println (s"y = $y")

    banner ("Optimized Simple Exponential Smoothing")
    val mod = new SimpleExpSmoothing (y)                         // smooth time series data: y vs. t
    mod.trainNtest ()()                                          // train-test use optimal α

// FIX
    val yf = mod.forecastAll (y, h)
    for k <- 1 to h do                                           // h-steps ahead  forecast
        banner (s"forecastAll h = $h")
        new Plot (null, y, yf(k), s"SES: Plot y and yf(${k})", lines = true)

/*
        banner (s"Rolling Validation h = $h")
        val stats = SimpleRollingValidation.crossValidate2 (mod, kt_ = 1, h = h)
        Fit.showQofStatTable (stats)
*/
    end for

end simpleExpSmoothingTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleExpSmoothingTest5` main function is used to test the `SimpleExpSmoothing` class.
 *  Forecasting lake levels for several values of the smoothing parameter α.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.simpleExpSmoothingTest5
 */
@main def simpleExpSmoothingTest5 (): Unit =

    import Example_LakeLevels.y

    val mod = new SimpleExpSmoothing (y)                         // time series model SimpleExpSmoothing
    mod.toggleOpt ()                                             // switch auto optimization off

    for i <- 0 to 5 do
        val a = i.toDouble / 5.0
        banner (s"Build SimpleExpSmoothing model with α = $a")
        mod.reset (a)
        mod.train (null, y)                                      // train the model on full dataset
        val (yp, qof) = mod.test (null, y)                       // test the model on full dataset
        println (mod.report (qof))                               // report on Quality of Fit (QoF)
    end for

end simpleExpSmoothingTest5

