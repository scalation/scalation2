
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Jun 13 01:2700 EST 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Weighted Moving Average (not the same as MA in ARMA)
 */

package scalation
package modeling
package forecasting_old

import scalation.mathstat._
import scalation.random.Normal

//import RollingValidation.trSize

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `WeightedMovingAverage` class provides basic time series analysis capabilities.
 *  For a `WeightedMovingAverage` model with the time series data stored in vector y, the
 *  next value y_t+1 = y(t+1) may be predicted based on the weighted-average of the past
 *  q values of y:
 *
 *      y_t+1 = weight-average (y_t, ..., y_t-q') + e_t+1
 *
 *  where e_t+1 is the new residual/error term and q' = q-1.  The hyper-parameter u selects
 *  between flat (u = 0), linear weights (u = 1) or comninations of both.
 *  @param y       the response vector (time series data)
 *  @param tt      the time points, if needed
 *  @param hparam  the hyper-parameters
 */
class WeightedMovingAverage (y: VectorD, tt: VectorD = null, hparam: HyperParameter = SimpleMovingAverage.hp)
      extends Forecaster (y, tt, hparam)
         with Correlogram (y)
         with Fit (dfm = 1, df = y.dim - 1):

    private val debug = debugf ("WeightedMovingAverage", false)        // debug function
    private val flaw  = flawf ("WeightedMovingAverage")                // flaw function
    private val q     = hparam("q").toInt                              // take weighted average of last q values
    private val u     = hparam("u").toDouble                           // u = 0 => flat, 1 => linear weights

    modelName = s"WeightedMovingAverage($q)"

    private val ww = VectorD.range (1, q+1)
    private val w1 = ww / ww.sum                                       // linear weights
    private val w2 = VectorD.one (q) / q                               // flat weights
    val w  = w1 * u + w2 * (1 - u)                                     // combination of weights

    debug ("init", s"size q = $q, weights w = $w")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `WeightedMovingAverage` model to the times series data in vector y_.
     *  Note: for `WeightedMovingAverage` there are no parameters to train.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the actual training/full response/output vector
     */
    def train (x_null: MatrixD, y_ : VectorD): Unit = {}

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test PREDICTIONS of a Weighted Moving Average forecasting model and
     *  return its predictions and  QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train before test.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the actual testing/full response/output vector
     */
    override def test (x_null: MatrixD, y_ : VectorD): (VectorD, VectorD) =
        val (yp, no_qof) = super.test (null, y_)                       // call super.test for predictions
        resetDF (1, y_.dim - 1)                                        // reset the degrees of freedom
        (yp, diagnose (y_, yp))                                        // return predictions and QoF vector
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test FORECASTS of a Weighted Moving Average forecasting model and
     *  return its forecasts and QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train and forecastAll before testF.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the training/testing/full response/output vector
     */
    def testF (h: Int, y_ : VectorD): (VectorD, VectorD, VectorD) =
        val (yy, yfh) = testSetupF (y_, h)                             // get and align actual and forecasted values
        resetDF (1, yy.dim - 1)                                        // reset the degrees of freedom
        println (s"testF: yy.dim = ${yy.dim}, yfh.dim = ${yfh.dim}")
//      differ (yy, yfh)                                               // uncomment for debugging
        (yy, yfh, diagnose (yy, yfh))                                  // return aligned actual, forecasted and QoF vectors
    end testF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameter vector (there are none, so return an empty vector).
     */
    override def parameter: VectorD = new VectorD (0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t+1 using the 1-step ahead forecast.
     *
     *      y_t+1 = [y_t-q+1, ... y_t] dot w
     *
     *  When t < q-1 is negative, use mean of partial sequence
     *  @param t   the time point from which to make prediction
     *  @param y_  the actual values to use in making predictions
     */
    def predict (t: Int, y_ : VectorD): Double =
        if t < q-1 then
            var sum = y_(0)
            for j <- 1 to t do sum += y_(t)
            sum / (t+1)
        else
            val yy = y_(t-q+1 until t+1)
            yy dot w
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
     *  @param hh  the max forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecast (t: Int, yf: MatrixD, y_ : VectorD, hh: Int): VectorD =
        if hh < 1 then flaw ("forecast", s"horizon hh = $hh must be at least 1")
        val yd = new VectorD (hh)                                      // hold forecasts for each horizon

        for h <- 1 to hh do
            val h1   = h - 1
            val pred =
            if t+h < q then
                val yy = yf(0 until t+h, h1)
                yy.sum / yy.dim                                        // use flat weights
            else
                val yy = yf(t+h-q until t+h, h1)                       // FIX: using the diagonal could work better
                yy dot w
            yf(t+h, h) = pred                                          // forecast down the diagonal
            yd(h-1)    = pred                                          // record diagonal values
        end for
        yd                                                             // return forecasts for each horizon
    end forecast

/*
        val yd = new VectorD (h)                                       // hold forecasts for each horizon
        val sumq = new SumQueue (q)
        for i <- max (0, t-q+1) to t do sumq += y(i)                   // y_t-q+1 + ... + y_t
        for k <- 1 to h do
            val pred = sumq.mean
            yf(t+k, k) = pred                                          // forecast down the diagonal
            yd(k-1)    = pred                                          // record diagonal values
            sumq      += yf(t+k, k-1)                                  // replace oldest value with this value
        end for
*/

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
            if t+h < q then
                val yy = yf(0 until t+h, h1)
                yf(t+h, h) = yy.sum / yy.dim                           // use flat weights
            else
                val yy = yf(t+h-q until t+h, h1)                       // FIX: using the diagonal could work better
                yf(t+h, h) = yy dot w
        end for
        yf(?, h)                                                       // return the h-step ahead forecast vector
    end forecastAt

end WeightedMovingAverage


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `WeightedMovingAverage` companion object provides factory methods for the `WeightedMovingAverage` class.
 */
object WeightedMovingAverage:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `WeightedMovingAverage` object.
     *  @param y       the response vector (time series data)
     *  @param tt      the time points, if needed
     *  @param hparam  the hyper-parameters
     */
    def apply (y: VectorD, tt: VectorD = null, hparam: HyperParameter = SimpleMovingAverage.hp): WeightedMovingAverage =
        new WeightedMovingAverage (y, tt, hparam)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backcast to predict the value prior to the start of the time series.
     *  @param y  the response vector (time series data), a prefix suffices
     *  @param q  the length of the moving average
     */
    def backcast (y_ : VectorD): Double =
        val q   = SimpleMovingAverage.hp ("q").toInt
        val yy  = y_(0 until q+1).reverse
        val mod = new WeightedMovingAverage (yy)
        mod.predict (q, yy)
    end backcast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Decompose a univariate time series into a moving average and a remainder.
     *  @see https://arxiv.org/pdf/2106.13008.pdf (Autoformer)
     *  @param y       the response vector (time series data)
     *  @param tt      the time points, if needed
     *  @param hparam  the hyper-parameters
     */
    def decompose (y: VectorD, tt: VectorD = null, hparam: HyperParameter = SimpleMovingAverage.hp): (VectorD, VectorD) =
        val sma = new WeightedMovingAverage (y, tt, hparam)
        val s = sma.predictAll (y)
        s(0)  = s(1)                                                   // pad by copying first avg back
        (s, y - s)
    end decompose

end WeightedMovingAverage


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `weightedMovingAverageTest` main function tests the `WeightedMovingAverage` class
 *  on simulated data.
 *  > runMain scalation.modeling.forecasting.weightedMovingAverageTest
 */
@main def weightedMovingAverageTest (): Unit =

    val y = makeTSeries ()                                             // generate a time-series (see `Stationary`)

    banner (s"Test Predictions: WeightedMovingAverage on simulated time-series")
    val mod = new WeightedMovingAverage (y)                            // time series model
    mod.trainNtest ()()                                                // train and test on full dataset

    banner ("Build SimpleMovingAverage Model")
    val mod2 = new SimpleMovingAverage (y)                             // time series model
    mod2.trainNtest ()()                                               // train and test on full dataset

end weightedMovingAverageTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `weightedMovingAverageTest2` main function is used to test the `WeightedMovingAverage` class.
 *  > runMain scalation.modeling.forecasting.weightedMovingAverageTest2
 */
@main def weightedMovingAverageTest2 (): Unit =

    val y = makeTSeries ((t: Double) => t, 30, Normal ())              // generate a time-series (see `Stationary`)

    banner ("Build WeightedMovingAverage Model")
    val mod = new WeightedMovingAverage (y)                            // time series model
    mod.trainNtest ()()                                                // train and test on full dataset

    banner ("Build SimpleMovingAverage Model")
    val mod2 = new SimpleMovingAverage (y)                             // time series model
    mod2.trainNtest ()()                                               // train and test on full dataset

/*
    banner ("Make Forecasts")
    val steps = 10                                                     // number of steps for the forecasts
    val rw_f = rw.forecast (steps)
    println (s"$steps-step ahead forecasts using WeightedMovingAverage model = $rw_f")
    val tf = VectorD.range (n, n + steps)
    new Plot (tf, rw_f, null, s"Plot WeightedMovingAverage forecasts vs. t", true)
*/

end weightedMovingAverageTest2

import forecasting.Example_LakeLevels.y

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `weightedMovingAverageTest3` main function is used to test the `WeightedMovingAverage` class.
 *  Forecasting lake levels.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.weightedMovingAverageTest3
 */
@main def weightedMovingAverageTest3 (): Unit =

    val hh = 3                                                         // maximum forecasting horizon

    val hp = SimpleMovingAverage.hp
    hp("q") = 2

    banner (s"Test Forecasts: WeightedMovingAverage on LakeLevels Dataset")
    val mod = new WeightedMovingAverage (y)                            // create model for time series data
    val (yp0, qof0) = mod.trainNtest ()()                              // train and test on full dataset
    val yp = mod.predictAll (y)
    Forecaster.differ (yp, yp0)

    mod.forecastAll (y, hh)                                             // forecast h-steps ahead (h = 1 to hh) for all y
    Forecaster.evalForecasts (mod, y, hh, true)

    banner (s"Test Forecasts: SimpleMovingAverage on LakeLevels Dataset")
    val mod2 = new SimpleMovingAverage (y)                             // create model for time series data
    val (yp2, qof2) = mod2.trainNtest ()()                             // train and test on full dataset

    banner (s"Test Forecasts: Random Walk on LakeLevels Dataset")
    val mod3 = new RandomWalk (y)                                      // create model for time series data
    val (yp3, qof3) = mod3.trainNtest ()()                             // train and test on full dataset

    val yp_  = yp
    val yp2_ = yp2 ++ VectorD (y.last)
    val yp3_ = yp3 ++ VectorD (y.last)
    println (MatrixD (y, yp_, yp2_, yp3_).transpose)

    val tf = new TestFit (y.dim)
    println (tf.testDiagnose (y, yp_))
    println (tf.testDiagnose (y, yp2_))
    println (tf.testDiagnose (y, yp3_))

    println (WeightedMovingAverage.backcast (y))

end weightedMovingAverageTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `weightedMovingAverageTest4` main function is used to test the `WeightedMovingAverage` class.
 *  Decompose the lake levels dataset.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.weightedMovingAverageTest4
 */
@main def weightedMovingAverageTest4 (): Unit =

    import forecasting.Example_LakeLevels.y

    SimpleMovingAverage.hp("q") = 5                                    // number of points to average
    banner ("Use WeightedMovingAverage to Decompose the Lake Level Dataset")
    val (s, z) = WeightedMovingAverage.decompose (y)                   // time series model
    new Plot (null, y, null, "original time series", lines = true)
    new Plot (null, s, null, "moving average", lines = true)
    new Plot (null, z, null, "remainder", lines = true)

end weightedMovingAverageTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `weightedMovingAverageTest5` main function is used to test the `WeightedMovingAverage` class.
 *  > runMain scalation.modeling.forecasting.weightedMovingAverageTest5
 */
@main def weightedMovingAverageTest5 (): Unit =

    val data = MatrixD.load ("travelTime.csv")                         // automatically prepends DATA_DIR

    val (t, y) = (data(?, 0), data(?, 1))

    println (s"t.dim = ${t.dim}, y.dim = ${y.dim}")

    banner ("Build AR(1) Model")
    val ar = new AR (y)                                                // time series model
    ar.trainNtest ()()                                                 // train and test on full dataset

    banner (s"Build WeightedMovingAverage model")
    val mod = new WeightedMovingAverage (y)                            // time series model
    mod.trainNtest ()()                                                // train and test on full dataset

end weightedMovingAverageTest5

