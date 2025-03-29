
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue May 11 16:25:40 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Quadratic Spline
 */

//  U N D E R   D E V E L O P M E N T

// FIX - forecast matrix looks like random work with a extra shift

package scalation
package modeling
package forecasting_old

import scala.math.max

import scalation.mathstat._

import QuadSpline.splineFit

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuadSpline` class fits quadratic splines to time-series data that are equally
 *  spaced in time.  A sliding window consisting of three data points is perfectly fit
 *  to a quadratic curve.
 *
 *      y_t = a + bt + ct^2
 *
 *  Note, slope matching and smoothness issues are ignored.
 *  @see wordsandbuttons.online/quadratic_splines_are_useful_too.html
 *  Any time point from t = 3 to the end of time series may be forecasted.
 *  FIX -- check the degrees of freedom (df)
 *  @param y       the response vector (time-series data)
 *  @param tt      the time vector, if relevant (time index may suffice)
 *  @param hparam  the hyper-parameters (none => use null)
 */
class QuadSpline (y: VectorD, tt: VectorD = null, hparam: HyperParameter = null)
      extends Forecaster (y, tt, hparam)
         with Correlogram (y)
         with Fit (dfm = 3, df = y.dim - 3):

    private val flaw = flawf ("QuadSpline")                            // flaw function

    modelName = "QuadSpline"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit a `QuadSpline` model to the times-series data in vector y_.
     *  Note: for `QuadSpline` there are no parameters to train.
     *  @param x_null  the data/input matrix (ignored)
     *  @param y_      the response/output vector (currently only works for y)
     */
    override def train (x_null: MatrixD, y_ : VectorD): Unit = { }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test PREDICTIONS of a QuadSpline forecasting model y_ = f(lags (y_)) + e
     *  and return its predictions and  QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train before test.
     *  @param x_null  the training/testing data/input matrix (ignored, pass null)
     *  @param y_      the training/testing/full response/output vector
     */
    override def test (x_null: MatrixD, y_ : VectorD): (VectorD, VectorD) =
        val (yp, no_qof) = super.test (null, y_)                        // call super.test for predictions
        resetDF (3, y_.dim - 3)                                         // reset the degrees of freedom
        (yp, diagnose (y_, yp))                                         // return predictions and QoF vector
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test FORECASTS of a QuadSpline forecasting model y_ = f(lags (y_)) + e
     *  and return its forecasts and QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train and forecastAll before testF.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the training/testing/full response/output vector
     */
    def testF (h: Int, y_ : VectorD): (VectorD, VectorD, VectorD) =
        val (yy, yfh) = testSetupF (y_, h)                             // get and align actual and forecasted values
        resetDF (3, yy.dim - 3)                                        // reset the degrees of freedom
        println (s"testF: yy.dim = ${yy.dim}, yfh.dim = ${yfh.dim}")
//      Forecaster.differ (yy, yfh)                                    // uncomment for debugging
        (yy, yfh, diagnose (yy, yfh))                                  // return aligned actual, forecasted and QoF vectors
    end testF
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameter vector (none, so return an empty vector).
     */ 
    override def parameter: VectorD = new VectorD (0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t+1 using the 1-step ahead forecast based on the quadratic
     *  curve fit to the previous three values:  y_t-2, y_t-1, y_t.
     *
     *      y_t+1 = a + bt + ct^2
     *
     *  @param t   the time point from which to make prediction
     *  @param y_  the actual values to use in making predictions
     */
    def predict (t: Int, y_ : VectorD): Double =
        if t >= 2 then
            val (a, b, c) = splineFit (t, y_(t-2), y_(t-1), y_(t))  // Quadratic Fit
            a + b*t + c*t~^2
        else
            y_(t)
        end if
//      if t < 2 || t >= y_.dim then y_.last
    end predict

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
            val pred = predict (t+k-1, y_)
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

        yf(h1, h) = yf(h1-1, h1)                                       // first forecast is special case

        for t <- y_.indices do                                         // make forecasts over all time points for horizon k
            if t > 2 then
                val (a, b, c) = splineFit (t, yf(t+h-3, max0 (h-3)),   // Quadratic Fit
                                              yf(t+h-2, max0 (h-2)),
                                              yf(t+h-1, max0 (h-1)))
                yf(t+h, h) = a + b*t + c*t~^2
            else if t == 2 then
                val (a, b) = splineFit (t, yf(t+h-2, max0 (h-2)),      // Linear Fit
                                           yf(t+h-1, max0 (h-1)))
                yf(t+h, h) = a + b*t
            else 
                yf(t+h, h) = yf(0, 0)
        end for
        yf(?, h)                                                       // return the h-step ahead forecast vector
    end forecastAt

end QuadSpline


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuadSpline` companion object provides factory methods for the `QuadSpline` class.
 */
object QuadSpline:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `QuadSpline` object that predicts using a quadratic curve that
     *  fits the last three point.
     *  @param y       the response vector (time-series data)
     *  @param tt      the time vector, if relevant (time index may suffice)
     *  @param hparam  the hyper-parameters (none => use null)
     */
    def apply (y: VectorD, tt: VectorD, hparam: HyperParameter = null): QuadSpline =
        new QuadSpline (y, tt, hparam)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Based on time t and three points y_t-2, y_t-1, y_t, determine values for the
     *  coefficients a, b and c.
     *  @param t    the time point
     *  @param y_2  the response value 2 time units in the past
     *  @param y_1  the response value 1 time units in the past
     *  @param y_0  the current response value
     */
    def splineFit (t: Double, y_2: Double, y_1: Double, y_0: Double): (Double, Double, Double) =
        val c = 0.5 * (y_0 - 2*y_1 + y_2)
        val b = 0.5 * (y_0 - y_2 - 4*c*t)
        val a = y_1 - b*t - c*t*t
        (a, b, c)
    end splineFit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Based on time t and two points y_t-1, y_t, determine values for the
     *  coefficients a and b.
     *  @param t    the time point
     *  @param y_1  the response value 1 time units in the past
     *  @param y_0  the current response value
     */
    def splineFit (t: Double, y_1: Double, y_0: Double): (Double, Double) =
        val b = y_1 - y_0
        val a = y_1 - b*t
        (a, b)
    end splineFit

end QuadSpline


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `quadSplineTest` main function tests the `QuadSpline` class on simulated data.
 *  Test predictions (one step ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.quadSplineTest
 */
@main def quadSplineTest (): Unit =

    val y = makeTSeries ()                                             // create simulated time-series (see `Stationary`)

    banner (s"Test Predictions: QuadSpline on simulated time-series")
    val mod = new QuadSpline (y)                                       // create model for time series data AR(1)
    mod.trainNtest ()()                                                // train and test on full dataset

end quadSplineTest

import forecasting.Example_LakeLevels.y

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `quadSplineTest2` main function is used to test the `QuadSpline` class.
 *  Forecasting lake levels.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.quadSplineTest2
 */
@main def quadSplineTest2 (): Unit =

    banner ("RandomWalk Model for Lake Levels Dataset")
    val rw = new RandomWalk (y)                                        // create a random walk model
    val (yp, qof) = rw.trainNtest ()()                                 // train and test on full dataset

    banner ("QuadSpline Model for Lake Levels Dataset")
    val mod = new QuadSpline (y)                                       // create a quadratic spline model
    val (yp2, qof2) = mod.trainNtest ()()                              // train and test on full dataset

    val mix = (yp + yp2) * 0.5
    new Plot (null, y(1 until y.dim), mix, "Mix: y vs. mix", lines = true)    

end quadSplineTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `quadSplineTest3` main function tests the `QuadSpline` class on real data:
 *  Forecasting lake levels.
 *  Test forecasts (1 to h steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.quadSplineTest3
 */
@main def quadSplineTest3 (): Unit =

    val hh = 3                                                         // maximum forecasting horizon

    banner (s"Test Forecasts: QuadSpline on LakeLevels Dataset")
    val mod = new QuadSpline (y)                                       // create model for time series data
    mod.trainNtest ()()                                                // train and test on full dataset

    mod.forecastAll (y, hh)                                            // forecast h-steps ahead (h = 1 to hh) for all y
    Forecaster.evalForecasts (mod, y, hh, true)

end quadSplineTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `quadSplineTest4` main function is used to test the `QuadSpline` class.
 *  Forecasting Fibonacci numbers.
 *  > runMain scalation.modeling.forecasting.quadSplineTest4
 */
@main def quadSplineTest4 (): Unit =

    val y = VectorD (1, 2, 3, 5, 8, 13, 21, 34, 55, 89)

    banner ("RandomWalk Model")
    val rw = new RandomWalk (y)                                        // create a random walk model
    rw.trainNtest ()()                                                 // train and test on full dataset

    banner ("QuadSpline Model")
    val mod = new QuadSpline (y)                                       // create a quadratic spline model
    mod.trainNtest ()()                                                // train and test on full dataset
    
end quadSplineTest4

