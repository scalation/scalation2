
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Jun 30 13:27:00 EDT 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Auto-Regressive on lagged y (ARY_D) using OLS - Direct Forecasting
 */

package scalation
package modeling
package forecasting

//import scala.math.{max, min}
import scala.math.max

import scalation.mathstat._
import scalation.modeling.neuralnet.{RegressionMV => REGRESSION}

import MakeMatrix4TS._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARY_D` class provides basic time series analysis capabilities for
 *  ARY_D models.  ARY_D models are often used for forecasting.
 *  `ARY_D` uses DIRECT (as opposed to RECURSIVE) multi-horizon forecasting.
 *  Given time series data stored in vector y, its next value y_t = combination of last p values.
 *
 *      y_t = b dot x_t + e_t
 *
 *  where y_t is the value of y at time t and e_t is the residual/error term.
 *  @param x        the data/input matrix (lagged columns of y) @see `ARY_D.apply`
 *  @param y        the response/output matrix (column per horizon) (time series data) 
 *  @param hh       the maximum forecasting horizon (h = 1 to hh)
 *  @param tRng     the time range, if relevant (time index may suffice)
 *  @param hparam   the hyper-parameters (defaults to `MakeMatrix4TS.hp`)
 *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
 */
class ARY_D (x: MatrixD, y: MatrixD, hh: Int, tRng: Range = null,
             hparam: HyperParameter = hp,
             bakcast: Boolean = false)
      extends Forecaster_D (x, y, hh, tRng, hparam, bakcast):           // no automatic backcasting, @see `ARY_D.apply`

    private val debug = debugf ("ARY_D", true)                          // debug function
//  private val flaw  = flawf ("ARY_D")                                 // flaw function
    private val p     = hparam("p").toInt                               // use the last p values (p lags)
    private val spec  = hparam("spec").toInt                            // trend terms: 0 - none, 1 - constant, 2 - linear, 3 - quadratic
                                                                        //              4 - sine, 5 cosine
    private val nneg  = hparam("nneg").toInt == 1                       // 0 => unrestricted, 1 => predictions must be non-negative
    private val reg   = new REGRESSION (x, y, null, hparam)             // delegate training to multi-variate regression

    modelName = s"ARY_D($p)"

    debug ("init", s"$modelName with additional term spec = $spec")
//  debug ("init", s"[ x | y ] = ${x ++^ y}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `ARY_D` model to the times-series data in vector y_.
     *  Estimate the coefficient vector b for a p-th order Auto-Regressive ARY_D(p) model.
     *  Uses OLS Matrix Fatorization to determine the coefficients, i.e., the b (φ) vector.
     *  @param x_  the data/input matrix (e.g., full x)
     *  @param y_  the training/full response vector (e.g., full y)
     */
    def train_x (x_ : MatrixD, y_ : MatrixD): Unit =
        debug ("train", s"$modelName, x_.dim = ${x_.dim}, y_.dim = ${y_.dim}")
        reg.train (x_, y_)                                              // train the multi-variate regression model
        bb = reg.parameter                                              // coefficients from regression
    end train_x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t using the 1-step ahead forecast.
     *
     *      y_t = b_0 + b_1 y_t-1 + b_2 y_t-2 + ... + b_p y_t-p = b dot x_t
     *
     *  @param t   the time point being predicted
     *  @param y_  the actual values to use in making predictions (ignored)
     */
    def predict (t: Int, y_ : MatrixD): VectorD =
        val yp = rectify (reg.predict (x(t)), nneg)
//      if t < y_.dim then
//          debug ("predict", s"@t = $t, x(t) = ${x(t)}, yp = $yp vs. y_ = ${y_(t)}")
        yp
    end predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a vector of size hh, h = 1 to hh-steps ahead forecasts for the model,
     *  i.e., forecast the following time points:  t+1, ..., t+h.
     *  Intended to work with rolling validation (analog of predict method).
     *  @param t   the time point from which to make forecasts
     *  @param y_  the actual values to use in making predictions
     */
    override def forecast (t: Int, y_ : VectorD): VectorD =
//      val pred = reg.predict (x(min (t+1, x.dim-1)))               // FIX - why t+1
        val pred = predict (t, MatrixD (y_).transpose)
        for h <- 1 to hh do yf(t-1, h) = pred(h-1)                   // FIX - why t-1
        pred                                                         // yh is pred
    end forecast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points all horizons h (h-steps ahead).
     *  Assign into FORECAST MATRIX and return the forecast matrix.
     *  @param y_  the matrix of actual response values
     */
    override def forecastAll (y_ : MatrixD): MatrixD =
        for t <- y_.indices do
            val pred = predict (t, y_)
            for h <- 1 to hh do yf(t, h) = pred(h-1)
//          for h <- 1 to hh do yf(max0 (t-1), h) = pred(h-1)        // FIX - why -1
        yf
    end forecastAll

end ARY_D


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARY_D` companion object provides factory methods for the `ARY_D` class.
 */
object ARY_D:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARY_D` object by building an input matrix x and then calling the constructor.
     *  @param y       the response vector (time series data)
     *  @param hh      the maximum forecasting horizon (h = 1 to hh)
     *  @param tRng    the time range, if relevant (time index may suffice)
     *  @param hparam  the hyper-parameters
     */
    def apply (y: VectorD, hh: Int, tRng: Range = null, hparam: HyperParameter = hp): ARY_D =
        val p       = hparam("p").toInt                                 // use the last p values
        val spec    = hparam("spec").toInt                              // 0 - none, 1 - constant, 2 - linear, 3 -quadratic, 4 - sin, 5 = cos
        val lwave   = hparam("lwave").toDouble                          // wavelength (distance between peaks)
        
        val (x, yy) = buildMatrix4TS (y, p, hh, spec, lwave)
        new ARY_D (x, yy, hh, tRng, hparam)
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a response vector (time series) y, build and return an input/predictor MATRIX x
     *  and a forecasting target MATRIX yy for today and all future horizons up to hh.
     *  @param y        the given output/response vector
     *  @param p        the maximum lag included (inclusive)
     *  @param hh       the maximum forecasting horizon (h = 1, 2, ... hh)
     *  @param spec     the specification for adding columns (0 => none, 1 => constant 2 => linear)
     *                  0 - none, 1 - constant 2 - linear, 3 - quadratic, 4 - sine, 5 - cosine
     *  @[aram lwave    wavelength (distance between peaks)
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     */
    def buildMatrix4TS (y: VectorD, p: Int, hh: Int, spec: Int = 1, lwave: Double = 7.0,
                        bakcast: Boolean = false): (MatrixD, MatrixD) =
        val yb = if bakcast then WeightedMovingAverage.backcast (y) +: y   // y prepended with one backcast
                 else y
        val m  = yb.dim
        val xt = makeMatrix4T (y, spec, lwave, bakcast)                 // trend terms

        val x  = new MatrixD (m, p)                                     // columns for spec + each lag
        val yy = new MatrixD (m, hh)                                    // yy = [ y_h ] for h = 1 to hh
        for t <- x.indices do
            for j <- 1 to p do x(t, p-j) = yb(max0 (t-j))               // x  -> lags
            for h <- yy.indices2 do
                yy(t, h) = if t+h >= m then -0.0 else yb(t+h)           // yy -> actual and horizons
        end for

        println (s"buildMatrix4TS: xt.dims = ${xt.dims}, x.dims = ${x.dims}, yy.dims = ${yy.dims}")
        (xt ++^ x, yy)
    end buildMatrix4TS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluate the quality of point and optionally interval forecast for horizon (h = 1 to hh).
     *  @param mod   the forecasting model to be evaluated
     *  @param yy    the complete shifted per horizon actual time series values
     *  @param hh    the maximum forecasting horizon (h = 1 to hh)
     *  @param ints  whether to evaluate prediction interval forecasts as well as point forecasts
     */
    def evalForecasts (mod: Forecaster, yy: MatrixD, hh: Int, ints: Boolean = false): Unit =
        val ftMat = new MatrixD (hh, Fit.N_QoF)
        banner (s"Evaluate ${mod.modelName}'s QoF for horizons 1 to $hh:")
        val m = yy.dim

        for h <- 1 to hh do
            val yh  = yy(0 until m-h, h-1)                                // h-steps ahead actual values
            val yfh = mod.getYf(0 until m-h, h)                           // h-steps ahead forecast
            val qof = mod.diagnose (yh, yfh)
            ftMat(h-1) = qof
//          println (FitM.fitMap (qof, qoF_names))                        // evaluate h-steps ahead forecasts
            new Plot (null, yh, yfh, s"evalForecast: Plot of yh, yfh for ${mod.modelName} vs. t @h = $h", true)

/*
            if ints then
                val (low, up) = mod.forecastAtI (yy, yfh, h)              // prediction interval forecasts
                val qof_all   = mod.diagnose_ (yy, yfh, low, up)          // fully evaluate h-steps ahead forecasts
                mod.show_interval_forecasts (yy, yfh, low, up, qof_all, h)
*/
        end for

        println ("fitMap     qof = ")
        println (FitM.showFitMap (ftMat.transpose, QoF.values.map (_.toString)))
    end evalForecasts

end ARY_D

import Example_Covid.loadData_y
import Example_LakeLevels.y

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRY_DTest` main function tests the `ARY_D` class on real data:
 *  Forecasting Lake Levels using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRY_DTest
 */
@main def aRY_DTest (): Unit =

    val hh = 3                                                          // maximum forecasting horizon

    val mod = ARY_D (y, hh)                                             // create model for time series data
    banner (s"In-ST Forecasts: ${mod.modelName} on LakeLevels Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset

    mod.forecastAll ()                                                  // forecast h-steps ahead (h = 1 to hh) for all y
    ARY_D.evalForecasts (mod, mod.getYy, hh)
    println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")

end aRY_DTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRY_DTest2` main function tests the `ARY_D` class on real data:
 *  Forecasting Lake Levels using Train-n-Test Split (TnT) with Rolling Validation.
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRY_DTest2
 */
@main def aRY_DTest2 (): Unit =

    val hh = 3                                                          // maximum forecasting horizon

    val mod = ARY_D (y, hh)                                             // create model for time series data
    banner (s"TnT Forecasts: ${mod.modelName} on LakeLevels Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset

    mod.rollValidate ()                                                 // TnT with Rolling Validation
    println (s"Final TnT Forecast Matrix yf = ${mod.getYf}")

end aRY_DTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRY_DTest3` main function tests the `ARY_D` class on real data:
 *  Forecasting COVID-19 using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRY_DTest3
 */
@main def aRY_DTest3 (): Unit =

    val yy = loadData_y ()
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon

    for p <- 1 to 5 do                                                  // number of lags
        hp("p") = p  
        val mod = ARY_D (y, hh)                                         // create model for time series data
        banner (s"In-ST Forecasts: ${mod.modelName} on COVID-19 Dataset")
        mod.trainNtest_x ()()                                           // train and test on full dataset

        mod.forecastAll (mod.getYy)                                     // forecast h-steps ahead (h = 1 to hh) for all y
        mod.diagnoseAll (y, mod.getYf)
//      ARY_D.evalForecasts (mod, mod.getYy, hh)
//      println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")
    end for

end aRY_DTest3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRY_DTest4` main function tests the `ARY_D` class on real data:
 *  Forecasting COVID-19 using Train-n-Test Split (TnT) with Rolling Validation.
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRY_DTest4
 */
@main def aRY_DTest4 (): Unit =

    val yy = loadData_y ()
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon

    for p <- 2 to 2 do                                                  // number of lags
        hp("p") = p
        val mod = ARY_D (y, hh)                                         // create model for time series data
        banner (s"TnT Forecasts: ${mod.modelName} on COVID-19 Dataset")
        mod.trainNtest_x ()()                                           // use customized trainNtest_x

        mod.setSkip (0)
        mod.rollValidate ()
        mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim))        // only diagnose on the testing set
        println (s"Final TnT Forecast Matrix yf = ${mod.getYf}")
    end for

end aRY_DTest4


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRY_DTest5` main function tests the `ARY_D` object's ability to build input
 *  matrices.  Build an input/predictor data matrix for the COVID-19 dataset.
 *  > runMain scalation.modeling.forecasting.aRY_DTest5
 */
@main def aRY_DTest5 (): Unit =

    val yy    = loadData_y ()
//  val y     = yy                                                      // full
    val y     = yy(0 until 116)                                         // clip the flat end
    val p     = 3                                                       // the number of lags
    val hh    = 2                                                       // the number of horizons
    val spec  = 1                                                       // additional trend terms
    val lwave = 20                                                      // wavelength

    println (s"y = $y")

    val (x, y_) = ARY_D.buildMatrix4TS (y, p, hh, spec, lwave)

    println (s"y.dim = ${y.dim}, x.dims = ${x.dims}, y_.dims = ${y_.dims}")

end aRY_DTest5

