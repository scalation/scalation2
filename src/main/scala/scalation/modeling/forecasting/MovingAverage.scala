
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Jun 13 01:27:00 EST 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Simple Moving Average (not the same as MA in ARMA)
 */

package scalation
package modeling
package forecasting

import scala.math.max

import scalation.mathstat._
import scalation.random.Normal

//import RollingValidation.trSize

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleMovingAverage` class provides basic time series analysis capabilities.
 *  For a `SimpleMovingAverage` model with the time series data stored in vector y, the
 *  next value y_t = y(t) may be predicted based on the mean of prior values
 *  of y and its noise:
 *      y_t+1 = mean (y_t, ..., y_t-q') + e_t+1
 *  where e_t+1 is the noise vector and q' = q-1 the number of prior values used to
 *  compute the mean.
 *  @param y       the response vector (time series data)
 *  @param tt      the time points, if needed
 *  @param hparam  the hyper-parameters
 */
class SimpleMovingAverage (y: VectorD, tt: VectorD = null, hparam: HyperParameter = SimpleMovingAverage.hp)
      extends Forecaster (y, tt, hparam)
         with Correlogram (y)
         with Fit (dfm = 1, df = y.dim - 1):

    private val flaw = flawf ("SimpleMovingAverage")                   // flaw function
    private val q    = hparam("q").toInt                               // take mean of last q values

    modelName = s"SimpleMovingAverage($q)"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `SimpleMovingAverage` model to the times series data in vector y_.
     *  Note: for `SimpleMovingAverage` there are no parameters to train.
     *  @param x_null  the data/input matrix (ignored)
     *  @param y_      the response/output vector (currently only works for y)
     */
    def train (x_null: MatrixD, y_ : VectorD): Unit = {}

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test PREDICTIONS of a Simple Moving Average forecasting model and
     *  return its predictions and  QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train before test.
     *  @param x_null  the training/testing data/input matrix (ignored, pass null)
     *  @param y_      the training/testing/full response/output vector
     */
    def test (x_null: MatrixD, y_ : VectorD): (VectorD, VectorD) =
        val (yy, yp) = testSetup (y_)                                  // get and align actual and predicted values
        resetDF (1, yy.dim - 1)                                        // reset the degrees of freedom
        println (s"test: yy.dim = ${yy.dim}, yp.dim = ${yp.dim}")
//      differ (yy, yp)                                                // uncomment for debugging
        (yp, diagnose (yy, yp))                                        // return predictions and QoF vector
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test FORECASTS of a Simple Moving Average forecasting model and
     *  return its forecasts and QoF vector.  Testing may be in-sample
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameter vector (its null).
     */
    override def parameter: VectorD = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t+1 using the 1-step ahead forecast.
     *      y_t+1 = φ_0 y_t + φ_1 y_t-1 + ... + φ_p-1 y_t-(p-1)
     *  When t-j is negative, use y_0
     *  @param t   the time point from which to make prediction
     *  @param y_  the actual values to use in making predictions
     */
    def predict (t: Int, y_ : VectorD): Double =
        val sumq = new SumQueue (q)
        for i <- max (0, t-q+1) to t do sumq += y(i)                   // y_t-q+1 + ... + y_t
        sumq.mean                                                      // prediction for y_t+1
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
        val sumq = new SumQueue (q)
        for i <- max (0, t-q+1) to t do sumq += y(i)                   // y_t-q+1 + ... + y_t
        for k <- 1 to h do
            val pred = sumq.mean
            yf(t+k, k) = pred                                          // forecast down the diagonal
            yd (k-1)   = pred                                          // record diagonal values
            sumq      += yf(t+k, k-1)                                  // replace oldest value with this value
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
        val sumq = new SumQueue (q)                                    // maintain sum of q most recent values
        sumq    += y(0)                                                // put in the first actual value
        for t <- y_.indices do                                         // make forecasts over all time points for horizon k
             yf(t+h, h) = sumq.mean                                    // mean of last q values
             sumq      += yf(t+h, h-1)                                 // replace oldest value with this value
        end for
        yf(?, h)                                                       // return the h-step ahead forecast vector
    end forecastAt

end SimpleMovingAverage


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleMovingAverage` companion object provides factory methods for the `SimpleMovingAverage` class.
 */
object SimpleMovingAverage:

    /** Base hyper-parameter specification for `SimpleMovingAverage`
     */
    val hp = new HyperParameter
    hp += ("q", 3, 3)                                                  // number of prior values for mean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SimpleMovingAverage` object.
     *  @param y       the response vector (time series data)
     *  @param tt      the time points, if needed
     *  @param hparam  the hyper-parameters
     */
    def apply (y: VectorD, tt: VectorD = null, hparam: HyperParameter = hp): SimpleMovingAverage =
        new SimpleMovingAverage (y, tt, hparam)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Decompose a univariate time series into a moving average and a remainder.
     *  @see https://arxiv.org/pdf/2106.13008.pdf (Autoformer)
     *  @param y       the response vector (time series data)
     *  @param tt      the time points, if needed
     *  @param hparam  the hyper-parameters
     */
    def decompose (y: VectorD, tt: VectorD = null, hparam: HyperParameter = hp): (VectorD, VectorD) =
        val sma = new SimpleMovingAverage (y, tt, hparam)
        val s = sma.predictAll (y)
        s(0)  = s(1)                                                   // pad by copy copying first avg back
        (s, y - s)
    end decompose

end SimpleMovingAverage


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleMovingAverageTest` main function tests the `SimpleMovingAverage` class on simulated data.
 *  > runMain scalation.modeling.forecasting.simpleMovingAverageTest
 */
@main def simpleMovingAverageTest (): Unit =

    val y = makeTSeries ()                                             // generate a time-series (see `Stationary`)

    banner (s"Test Predictions: AR(1) on simulated time-series")
    val ar = new AR (y)                                                // create model for time series data AR(1)
    ar.trainNtest ()()                                                 // train and test on full dataset

    banner (s"Test Predictions: SimpleMovingAverage on simulated time-series")
    val mod = new SimpleMovingAverage (y)                              // time series model
    mod.trainNtest ()()                                                // train and test on full dataset

    banner ("Select model based on ACF and PACF")
    ar.plotFunc (ar.acF, "ACF")                                        // Auto-Correlation Function (ACF)
    ar.plotFunc (ar.pacF, "PACF")                                      // Partial Auto-Correlation Function (PACF)

end simpleMovingAverageTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleMovingAverageTest2` main function is used to test the `SimpleMovingAverage` class.
 *  > runMain scalation.modeling.forecasting.simpleMovingAverageTest2
 */
@main def simpleMovingAverageTest2 (): Unit =

    val y = makeTSeries ((t: Double) => t, 30, Normal ())              // generate a time-series (see `Stationary`)

    banner ("Build AR(1) Model")
    val ar = new AR (y)                                                // time series model
    ar.trainNtest ()()                                                 // train and test on full dataset

    banner ("Build SimpleMovingAverage Model")
    val mod = new SimpleMovingAverage (y)                              // time series model
    mod.trainNtest ()()                                                // train and test on full dataset

/*
    banner ("Make Forecasts")
    val steps = 10                                                     // number of steps for the forecasts
    val rw_f = rw.forecast (steps)
    println (s"$steps-step ahead forecasts using SimpleMovingAverage model = $rw_f")
    val tf = VectorD.range (n, n + steps)
    new Plot (tf, rw_f, null, s"Plot SimpleMovingAverage forecasts vs. t", true)
*/

end simpleMovingAverageTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleMovingAverageTest3` main function is used to test the `SimpleMovingAverage` class.
 *  Forecasting lake levels.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.simpleMovingAverageTest3
 */
@main def simpleMovingAverageTest3 (): Unit =

    import Example_LakeLevels.y

    banner ("Build AR(1) Model")
    val ar = new AR (y)                                                // time series model
    ar.trainNtest ()()                                                 // train and test on full dataset

    banner ("Build SimpleMovingAverage Model")
    val mod = new SimpleMovingAverage (y)                              // time series model
    mod.trainNtest ()()                                                // train and test on full dataset

/*
    for (h <- 1 to 4) {                                                // h-steps ahead  forecast
        banner (s"Rolling Validation h = $h")
        val stats = SimpleRollingValidation.crossValidate2 (ma, kt_ = 1, h = h)
//      val stats = RollingValidation.crossValidate2 (ma, kt_ = 5, h = h)
        Fit.showQofStatTable (stats)
    } // for
*/

end simpleMovingAverageTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleMovingAverageTest4` main function is used to test the `SimpleMovingAverage` class.
 *  Decompose the lake levels dataset.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.simpleMovingAverageTest4
 */
@main def simpleMovingAverageTest4 (): Unit =

    import Example_LakeLevels.y

    SimpleMovingAverage.hp("q") = 5                                        // number of points to average
    banner ("Use SimpleMovingAverage to Decompose the Lake Level Dataset")
    val (s, z) = SimpleMovingAverage.decompose (y)                         // time series model
    new Plot (null, y, null, "original time series", lines = true)
    new Plot (null, s, null, "moving average", lines = true)
    new Plot (null, z, null, "remainder", lines = true)

end simpleMovingAverageTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleMovingAverageTest5` main function is used to test the `SimpleMovingAverage` class.
 *  > runMain scalation.modeling.forecasting.simpleMovingAverageTest5
 */
@main def simpleMovingAverageTest5 (): Unit =

    val data = MatrixD.load ("travelTime.csv")                         // automatically prepends DATA_DIR

    val (t, y) = (data(?, 0), data(?, 1))

    println (s"t.dim = ${t.dim}, y.dim = ${y.dim}")

    banner ("Build AR(1) Model")
    val ar = new AR (y)                                                // time series model
    ar.trainNtest ()()                                                 // train and test on full dataset

    banner (s"Build SimpleMovingAverage model")
    val mod = new SimpleMovingAverage (y)                              // time series model
    mod.trainNtest ()()                                                // train and test on full dataset

end simpleMovingAverageTest5

