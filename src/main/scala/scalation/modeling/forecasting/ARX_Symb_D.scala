
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Yousef Fekri Dabanloo
 *  @version 2.0
 *  @date    Thu Jan 30 21:15:45 EST 2025
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Auto-Regressive on lagged y and xe with SR terms (ARX_Symb_D) using OLS - Direct Forecasting
 *
 *  @see `scalation.modeling.Regression`
 */


package scalation
package modeling
package forecasting

import scala.collection.mutable.ArrayBuffer
import scalation.mathstat._
import scalation.modeling.neuralnet.RegressionMV as REGRESSION
import MakeMatrix4TS._

import scala.math.min

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_Symb_D` class provides time series analysis capabilities for ARX_D Symbolic
 *  Regression (SR) models.  These models include trend, linear, power, root, and cross terms
 *  for the single endogenous (y) variable and zero or more exogenous (xe) variables.
 *  Given time series data stored in vector y and matrix xe, its next value y_t = combination
 *  of last p values of y, y^p, y^r and the last q values of each exogenous variable xe_j,
 *  again in linear, power and root forms (as well as ENDO-EXO cross terms).
 *
 *      y_t = b dot x_t + e_t
 *
 *  where y_t is the value of y at time t, x_t is a vector of inputs, and e_t is the
 *  residual/error term.
 *  @see `MakeMatrix4TS` for hyper-parameter specifications.
 *  @param x        the data/input matrix (lagged columns of y and xe) @see `ARX_Symb_D.apply`
 *  @param y        the response/output vector (main time series data) 
 *  @param hh       the maximum forecasting horizon (h = 1 to hh)
 *  @param n_exo    the number of exogenous variables
 *  @param fname    the feature/variable names
 *  @param tRng     the time range, if relevant (time index may suffice)
 *  @param hparam   the hyper-parameters (defaults to `MakeMatrix4TS.hp`)
 *  @param itran    the inverse transformation to return to the original scale (defaults to null)
 *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
 */
class ARX_Symb_D (x: MatrixD, y: MatrixD, hh: Int, n_exo: Int, fname: Array [String],
                tRng: Range = null, hparam: HyperParameter = hp,
                val itran: FunctionS2S = null, bakcast: Boolean = false)    // backcast value used only `MakeMatrix4TS`
      extends Forecaster_D (x, y, hh, tRng, hparam, bakcast):            // no automatic backcasting

    private val debug = debugf ("ARX_Symb_D", true)                      // debug function
    private val p     = hparam("p").toInt                                // use the last p values (p lags)
    private val q     = hparam("q").toInt                                // use the last q exogenous values (q lags)
    private val spec  = hparam("spec").toInt                             // trend terms: 0 - none, 1 - constant, 2 - linear, 3 - quadratic
                                                                         //              4 - sine, 5 cosine
    private val nneg = hparam("nneg").toInt == 1                         // 0 => unrestricted, 1 => predictions must be non-negative
    private val reg = new REGRESSION (x, y, fname, hparam)               // delegate training to multi-variate regression

    modelName = s"ARX_Symb_D($p, $q, $n_exo)"

    debug ("init", s"$modelName with with $n_exo exogenous variables and additional term spec = $spec")
//  debug ("init", s"[ x | y ] = ${x ++^ y}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `ARX_Symb_D` model to the times-series data in vector y_.
     *  Estimate the coefficient mattrix bb for a p-th order Auto-Regressive ARX_Symb_D(p) model.
     *  Uses OLS Matrix Fatorization to determine the coefficients, i.e., the bb matrix.
     *  @param x_  the data/input matrix (e.g., full x)
     *  @param y_  the training/full response vector (e.g., full y)
     */
    def train_x (x_ : MatrixD, y_ : MatrixD): Unit =
        debug ("train_x", s"$modelName, x_.dim = ${x_.dim}, y_.dim = ${y_.dim}")
        reg.train (x_, y_)                                              // train the multi-variate regression model
        bb = reg.parameter                                              // coefficients from regression
    end train_x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a QoF summary for a model with diagnostics for each predictor 'x_j'
     *  and the overall Quality of Fit (QoF).
     *  @param x_      the testing/full data/input matrix
     *  @param fname_  the array of feature/variable names
     *  @param b_      the parameters/coefficients for the model
     *  @param vifs    the Variance Inflation Factors (VIFs)
     */
    override def summary (x_ : MatrixD = getX, fname_ : Array[String] = reg.getFname,
                          b_ : VectorD = b, vifs: VectorD = reg.vif()): String =
        super.summary (x_, fname_, b_, vifs)                            // summary from `Fit`
    end summary

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
        if t < y_.dim then
            debug ("predict", s"@t = $t, x(t) = ${x(t)}, yp = $yp vs. y_ = ${y_(t)}")
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
        val pred = predict (t, MatrixD(y_).transpose)
        for h <- 1 to hh do yf(t - 1, h) = pred(h - 1)
        pred                                                              // yh is pred
    end forecast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points all horizons h (h-steps ahead).
     *  Assign into FORECAST MATRIX and return the forecast matrix.
     *  @param y_ the matrix of actual response values
     */
    override def forecastAll (y_ : MatrixD): MatrixD =
        for t <- y_.indices do
            val pred = predict (t, y_)
            for h <- 1 to hh do yf(t, h) = pred(h - 1)
        yf
    end forecastAll

end ARX_Symb_D

import Example_Covid._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_Symb_D` companion object provides factory methods for the `ARX_Symb_D` class.
 */
object ARX_Symb_D:

    private val bounds = (1.0, 5.0)                                     // (lower, upper) bounds for rescaling

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARX_Symb_D` object by building an input matrix xy and then calling the
     *  `ARX_Symb_D` constructor.
     *  @param xe       the matrix of exogenous variable values
     *  @param y        the endogenous/response vector (main time series data)
     *  @param hh       the maximum forecasting horizon (h = 1 to hh)
     *  @param fname_   the feature/variable names
     *  @param tRng     the time range, if relevant (time index may suffice)
     *  @param hparam   the hyper-parameters
     *  @param tran     the transformation function (defaults to null, could use log1p)
     *  @param itran    the inverse transformation function to rescale predictions to original y scale
     *                      (defaults to null, could use expm1)
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     */
    def apply (xe: MatrixD, y: VectorD, hh: Int, fname_ : Array [String] = null,
               tRng: Range = null, hparam: HyperParameter = hp,
               tran: FunctionS2S = null, itran: FunctionS2S = null,
               bakcast: Boolean = false): ARX_Symb_D =
        val p     = hparam("p").toInt                                   // use the last p values
        val pp    = hparam("pp").toDouble                               // use the last p values raised to pp power
        val pr    = hparam("pr").toDouble                               // use the last p values taken to the pr root
        val q     = hparam("q").toInt                                   // use the last q exogenous values
        val qp    = hparam("qp").toDouble                               // use the last q exogenous values raised to pp power
        val qr    = hparam("qr").toDouble                               // use the last q exogenous values taken to the pr root
        val spec  = hparam("spec").toInt                                // 0 - none, 1 - constant, 2 - linear, 3 -quadratic, 4 - sin, 5 = cos
        val lwave = hparam("lwave").toDouble                            // wavelength (distance between peaks)
        val cross = hparam("cross").toInt == 1                          // whether to include ENDO-EXO cross terms
        val yt    = if tran != null then y.map (tran)                   // y transformed
                    else y
        val (xy, yy)    = buildMatrix4TS (xe, yt, p, pp, pr, q, qp, qr, spec, lwave, cross, hh, bakcast)
        val n_exo = xe.dim2
        val fname = if fname_ == null then formNames (spec, p, n_exo, q)
                    else fname_
        new ARX_Symb_D (xy, yy, hh, n_exo, fname, tRng, hparam, itran, bakcast)
    end apply 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARX_Symb_D` object by building an input matrix xy and then calling the
     *  `ARX_Symb_D` constructor, with rescaling of endogneous and exogenous variable values.
     *  @param xe       the matrix of exogenous variable values
     *  @param y        the endogenous/response vector (main time series data)
     *  @param hh       the maximum forecasting horizon (h = 1 to hh)
     *  @param tRng     the time range, if relevant (time index may suffice)
     *  @param hparam   the hyper-parameters
     *  @param tran     the transformation function (defaults to null, could use log1p)
     *  @param itran    the inverse transformation function to rescale predictions to original y scale
     *                      (defaults to null, could use expm1)
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     */
    def rescale (xe: MatrixD, y: VectorD, hh: Int, fname: Array [String] = null,
                 tRng: Range = null, hparam: HyperParameter = hp,
                 tran: FunctionS2S = null, itran: FunctionS2S = null,
                 bakcast: Boolean = false): ARX_Symb_D =
        val p     = hparam("p").toInt                                   // use the last p values
        val pp    = hparam("pp").toDouble                               // use the last p values raised to pp power
        val pr    = hparam("pr").toDouble                               // use the last p values taken to the pr root
        val q     = hparam("q").toInt                                   // use the last q exogenous values
        val qp    = hparam("qp").toDouble                               // use the last q exogenous values raised to pp power
        val qr    = hparam("qr").toDouble                               // use the last q exogenous values taken to the pr root
        val spec  = hparam("spec").toInt                                // 0 - none, 1 - constant, 2 - linear, 3 -quadratic, 4 - sin, 5 = cos
        val lwave = hparam("lwave").toDouble                            // wavelength (distance between peaks)
        val cross = hparam("cross").toInt == 1                          // whether to include ENDO-EXO cross terms
        val xet   = scale (extreme (xe), bounds)(xe)                    // rescale x matrix to bounds
        val yt    = if tran != null then y.map (tran)                   // y transformed
                    else y
        val (xy, yy)    = buildMatrix4TS (xet, yt, p, pp, pr, q, qp, qr, spec, lwave, cross, hh, bakcast)
        new ARX_Symb_D (xy, yy, hh, xe.dim2, fname, tRng, hparam, itran, bakcast)
    end rescale

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build an input matrix by combining up 5 trend terms, endogenous terms from
     *  endogenous variable y, exogenous terms from n = xe.dim2 exogenous variables xe,
     *  and endogenous-exogenous cross terms.  Specifically,
     *      trend terms        spec (0 to 5) trend terms
     *      endogenous terms:  p lagged linear terms, p lagged power terms, p lagged root terms,
     *      exogenous terms:   q * n lagged linear terms, q * n lagged power terms, q * n lagged root terms,
     *      cross terms:       q * n lagged linear cross terms.
     *  @param xe       the matrix of exogenous variable values
     *  @param y        the response vector (time series data)
     *  @param p        the number of lags for the endogenous variable (lags 1 to p)
     *  @param pp       the power (defaults to quadratic) to raise the lags of the endogenous variable to
     *  @param pr       the root (defaults to sqrt) to take of the lags of the endogenous variable
     *  @param q        the number of lags for each exogenous variable (lags 1 to q)
     *  @param qp       the power (defaults to quadratic) to raise the lags of the exogenous variables to
     *  @param qr       the root (defaults to sqrt) to take of the lags of the exogenous variables
     *  @param spec     the number of trend terms (added columns)
     *                      0 - none, 1 - constant 2 - linear, 3 - quadratic, 4 - sine, 5 - cosine
     *  @param lwave    the wavelength (distance between peaks)
     *  @param cross    whether to include cross terms between endogenous and exogenous variables
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     */
    def buildMatrix4TS (xe: MatrixD, y: VectorD,
                        p: Int, pp: Double, pr: Double,
                        q: Int, qp: Double, qr: Double,
                        spec: Int, lwave: Double, cross: Boolean,
                        hh: Int, bakcast: Boolean): (MatrixD, MatrixD) =
        val xy = ARX_Symb.buildMatrix4TS (xe, y, p, pp, pr, q, qp, qr, spec, lwave, cross, bakcast)

        val yb = if bakcast then WeightedMovingAverage.backcast(y) +: y      // y prepended with one backcast
                 else y

        val m  = y.dim
        val yy = new MatrixD(m, hh)                                          // yy = [ y_h ] for h = 1 to hh
        for t <- yy.indices do
            for h <- yy.indices2 do
                yy(t, h) = if t + h >= m then -0.0 else yb(t + h)            // yy -> actual and horizons
        (xy, yy)
    end buildMatrix4TS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form an array of names for the features included in the model.
     *  @param spec   the number of trend terms
     *  @param p      the number of lags for the endogenous variable (lags 1 to p)
     *  @param n_exo  the number of exogenous variable
     *  @param q      the number of lags for each exogenous variable (lags 1 to q)
     */
    def formNames (spec: Int, p: Int, n_exo: Int, q: Int): Array [String] =
        val names = ArrayBuffer [String] ()
        for i <- p to 1 by -1 do names += s"yl$i~"
        for j <- 0 until n_exo; k <- q to 1 by -1 do names += s"xe${j}l$k"
        for j <- 0 until n_exo; k <- q to 1 by -1 do names += s"xe${j}l$k^"
        for j <- 0 until n_exo; k <- q to 1 by -1 do names += s"xe${j}l$k~"
        MakeMatrix4TS.formNames (spec, p, true) ++ names.toArray
    end formNames

end ARX_Symb_D


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_Symb_DTest3` main function tests the `ARX_Symb_D` class on real data:
 *  Forecasting COVID-19 using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRX_Symb_DTest3
 */
@main def aRX_Symb_DTest3 (): Unit =

//  val exo_vars  = NO_EXO
    val exo_vars  = Array ("hosp_patients")
//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val xe = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon
    hp("lwave") = 20                                                    // wavelength (distance between peaks)

    for p <- 1 to 6; s <- 1 to 5 do                                     // number of lags; trend; number of exo lags
        hp("p")    = p                                                  // endo lags
        hp("q")    = 2                                                  // exo lags
        hp("spec") = s                                                  // trend specification: 0, 1, 2, 3, 5
        val mod = ARX_Symb_D (xe, y, hh)                                // create model for time series data
        banner (s"In-ST Forecasts: ${mod.modelName} on COVID-19 Dataset")
        mod.trainNtest_x ()()                                           // train and test on full dataset
//      println (mod.summary ())                                        // statistical summary of fit
        println (s"Before forecastAll Matrix yf = ${mod.getYf}")

//      mod.setSkip (p)                                                 // full AR-formula available when t >= p
        mod.forecastAll (mod.getYy)                                     // forecast h-steps ahead (h = 1 to hh) for all y
        mod.diagnoseAll (y, mod.getYf)                                  // QoF for each horizon
//      Forecaster.evalForecasts (mod, mod.getYb, hh)
        println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")
//      println (s"Final In-ST Forecast Matrix yf = ${mod.getYf.shiftDiag}")
    end for

end aRX_Symb_DTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_Symb_DTest4` main function tests the `ARX_Symb_D` class on real data:
 *  Forecasting COVID-19 using Train and Test (TnT).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRX_Symb_DTest4
 */
@main def aRX_Symb_DTest4 (): Unit =

//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val exo_vars  = Array ("hosp_patients")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val xe = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon
    hp("lwave") = 20                                                    // wavelength (distance between peaks)

    for p <- 1 to 6; s <- 1 to 5 do                                     // number of lags; trend
        hp("p")    = p                                                  // endo lags
//      hp("q")    = 1                                                  // exo lags
        hp("q")    = min (2, p)                                         // try various rules
        hp("spec") = s                                                  // trend specification: 0, 1, 2, 3, 5
        val mod = ARX_Symb_D (xe, y, hh)                                // create model for time series data
        banner (s"TnT Forecasts: ${mod.modelName} on COVID-19 Dataset")
        mod.trainNtest_x ()()                                           // use customized trainNtest_x

        mod.setSkip (0)
        mod.rollValidate ()                                             // TnT with Rolling Validation
        println (s"After Roll TnT Forecast Matrix yf = ${mod.getYf}")
        mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim), 0)     // only diagnose on the testing set
//      println (s"Final TnT Forecast Matrix yf = ${mod.getYf}")
    end for

end aRX_Symb_DTest4

