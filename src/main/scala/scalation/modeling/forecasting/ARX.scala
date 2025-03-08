
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Jun 30 13:27:00 EDT 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Auto-Regressive on lagged y and xe (ARX) using OLS
 *
 *  @see `scalation.modeling.Regression`
 *  @see `scalation.modeling.forecasting.ARY` when no exogenous variable are needed
 */

package scalation
package modeling
package forecasting

import scala.collection.mutable.ArrayBuffer
import scala.math.min

import scalation.mathstat._

import MakeMatrix4TS._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX` class provides basic time series analysis capabilities for ARX models.
 *  ARX models build on `ARY` by including one or more exogenous (xe) variables.
 *  Given time series data stored in vector y, its next value y_t = combination of
 *  last p values of y and the last q values of each exogenous variable xe_j.
 *
 *      y_t = b dot x_t + e_t
 *
 *  where y_t is the value of y at time t and e_t is the residual/error term.
 *  @param x        the data/input matrix (lagged columns of y and xe) @see `ARX.apply`
 *  @param y        the response/output vector (time series data) 
 *  @param hh       the maximum forecasting horizon (h = 1 to hh)
 *  @param n_exo    the number of exogenous variables
 *  @param fname    the feature/variable names
 *  @param tRng     the time range, if relevant (time index may suffice)
 *  @param hparam   the hyper-parameters (defaults to `MakeMatrix4TS.hp`)
 *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
 */
class ARX (x: MatrixD, y: VectorD, hh: Int, n_exo: Int, fname: Array [String],
           tRng: Range = null, hparam: HyperParameter = hp,
           bakcast: Boolean = false)                                    // backcasted values only used in `MakeMatrix4TS`
      extends Forecaster_Reg (x, y, hh, fname, tRng, hparam, bakcast):  // no automatic backcasting

    private val debug = debugf ("ARX", true)                            // debug function
    private val p     = hparam("p").toInt                               // use the last p endogenous values (p lags)
    private val q     = hparam("q").toInt                               // use the last q exogenous values (q lags)
    private val spec  = hparam("spec").toInt                            // trend terms: 0 - none, 1 - constant, 2 - linear, 3 - quadratic
                                                                        //              4 - sine, 5 cosine
    modelName = s"ARX($p, $q, $n_exo)"

    debug ("init", s"$modelName with $n_exo exogenous variables and additional term spec = $spec")
    debug ("init", s"[ x | y ] = ${x :^+ y}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forge a new vector from the first spec values of x, the last p-h+1 values
     *  of x (past values), values 1 to h-1 from the forecasts, and available values
     *  from exogenous variables.
     *  @param xx  the t-th row of the input matrix (lagged actual values)
     *  @param yy  the t-th row of the forecast matrix (forecasted future values)
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forge (xx: VectorD, yy: VectorD, h: Int): VectorD =
        val n_endo  = spec + p                                         // number of trend + endogenous values
        val x_trend = xx(0 until spec)                                 // get trend values
        val x_act   = xx(n_endo-(p+1-h) until n_endo)                  // get actual lagged y-values (endogenous)
        val nyy     = p - x_act.dim                                    // number of forecasted values needed
//      println (s"forge: h = $h, n_nedo = $n_endo, [ ${x_trend.dim}, ${x_act.dim} ], nyy = $nyy")
        val x_fcast = yy(h-nyy until h)                                // get forecasted y-values
        var xy      = x_trend ++ x_act ++ x_fcast
 
        for j <- 0 until n_exo do                                      // for the j-th exogenous variable
            xy = xy ++ hide (xx(n_endo+j*q until n_endo+(j+1)*q), h)   // imple 1: get actual lagged xe-values for exogenous variable j
//          xy = xy ++ xx(n_endo+j*q until n_endo+(j+1)*q)             // imple 2: get actual lagged xe-values for exogenous variable j
        xy
    end forge

// smape for p = 5, q = 3:
// imple 1: 14.2526,	20.5118,	28.8433,	35.6349,	41.8718,	47.1786) with hide (s = 1)
// imple 1: 14.2334,	22.0243,	30.1842,	36.4040,	44.6398,	51.3710  with hide (s = 2)
// imple 2: 14.2526,	17.7696,	22.7818,	28.5917,	35.1854,	40.7525  but has data-leakage (s = 1)
// imple 2: 14.2334,	19.4486,	24.6708,	30.6217,	38.4295,	42.9449  but has data-leakage (s = 2)
// imple 3: use the q most recent exegonous non-future values for each exo variable 
// imple 4: use Weighted Moving Average for simple local forecasts for each exo variable

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Hide values at the end of vector z (last h-1 values) as the increasing horizon  
     *  turns them in future values (hence unavailable).  Set these values to either
     *  zero (the default) or the last available value.
     *  @param z     the vector to shift
     *  @param h     the current horizon (number of steps ahead to forecast)
     *  @param fill  whether to backfill with the rightmost value (true) or with 0 (false)
     */
    def hide (z: VectorD, h: Int, fill: Boolean = true): VectorD =
        val lst = z.dim - h                                           // last available index position in z
        val zl  = if lst >= 0 then z(lst) else 0.0                    // last available z value per horizon
        val z_  = new VectorD (z.dim)
        for k <- z.indices do
            z_(k) = if k <= lst then z(k) else if fill then zl else 0.0
        z_
    end hide

end ARX

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX` companion object provides factory methods for the `ARX` class.
 */
object ARX:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARX` object by building an input matrix xy and then calling the
     *  `ARX` constructor.
     *  @param xe       the matrix of exogenous variable values
     *  @param y        the response vector (time series data)
     *  @param hh       the maximum forecasting horizon (h = 1 to hh)
     *  @param fname_   the feature/variable names
     *  @param tRng     the time range, if relevant (time index may suffice)
     *  @param hparam   the hyper-parameters
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     */
    def apply (xe: MatrixD, y: VectorD, hh: Int, fname_ : Array [String] = null,
               tRng: Range = null, hparam: HyperParameter = hp,
               bakcast: Boolean = false): ARX =
        val p     = hparam("p").toInt                                   // use the last p endogenous values
        val q     = hparam("q").toInt                                   // use the last q exogenous values
        val spec  = hparam("spec").toInt                                // 0 - none, 1 - constant, 2 - linear, 3 -quadratic, 4 - sin, 5 = cos
        val lwave = hparam("lwave").toDouble                            // wavelength (distance between peaks)
        val n_exo = xe.dim2
        val fname = if fname_ == null then formNames (spec, p, n_exo, q)
                    else fname_
        val x     = buildMatrix4TS (xe, y, p, q, spec, lwave, bakcast)
        new ARX (x, y, hh, n_exo, fname, tRng, hparam, bakcast)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the input matrix by combining the p + spec columns for the trend and
     *  endogenous variable with the q * xe.dim2 columns for the exogenous variables.
     *  @param xe       the matrix of exogenous variable values
     *  @param y        the response vector (time series data)
     *  @param p        the number of lags for the endogenous variable (lags 1 to p)
     *  @param q        the number of lags for each exogenous variable (lags 1 to q)
     *  @param spec     the number of trend terms (added columns)
     *                      0 - none, 1 - constant 2 - linear, 3 - quadratic, 4 - sine, 5 - cosine
     *  @param lwave    the wavelength (distance between peaks)
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     */
    def buildMatrix4TS (xe: MatrixD, y: VectorD, p: Int, q: Int, spec: Int, lwave: Double,
                        bakcast: Boolean): MatrixD =
        val xt = makeMatrix4T (y, spec, lwave, bakcast)              // trend terms
        val xl = makeMatrix4L (y, p, bakcast)                        // regular lag terms
        if xe.dim2 == 0 then xt ++^ xl
        else xt ++^ xl ++^ makeMatrix4EXO (xe, q, 1, bakcast)        // add exogenous terms
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
        for j <- 0 until n_exo; k <- 1 to q do names += s"xe${j}l$k"
        MakeMatrix4TS.formNames (spec, p) ++ names.toArray
    end formNames

end ARX

import Example_Covid._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRXTest` main function tests the `ARX` class on real data:
 *  Forecasting Lake Levels using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRXTest
 *
@main def aRXTest (): Unit =

    val hh = 3                                                          // maximum forecasting horizon

    val mod = ARX (y, hh)                                               // create model for time series data
    banner (s"In-ST Forecasts: ${mod.modelName} on LakeLevels Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset

    mod.forecastAll ()                                                  // forecast h-steps ahead (h = 1 to hh) for all y
    Forecaster.evalForecasts (mod, mod.getYb, hh)
    println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")

end aRXTest
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRXTest2` main function tests the `ARX` class on real data:
 *  Forecasting Lake Levels using Train-n-Test Split (TnT) with Rolling Validation.
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRXTest2
 *
@main def aRXTest2 (): Unit =

    val hh = 3                                                          // maximum forecasting horizon

    val mod = ARX (y, hh)                                               // create model for time series data
    banner (s"TnT Forecasts: ${mod.modelName} on LakeLevels Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset

    mod.rollValidate ()                                                 // TnT with Rolling Validation
    println (s"Final TnT Forecast Matrix yf = ${mod.getYf}")

end aRXTest2
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRXTest3` main function tests the `ARX` class on real data:
 *  Forecasting COVID-19 using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRXTest3
 */
@main def aRXTest3 (): Unit =

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

    new Plot (null, y, null, s"y (new_deaths) vs. t", lines = true)
    for j <- exo_vars.indices do
        new Plot (null, xe(?, j), null, s"x_$j (${exo_vars(j)}) vs. t", lines = true)

    for p <- 5 to 5; q <- 3 to 3; s <- 1 to 1 do                        // number of endo lags; exo lags; trend
        hp("p")    = p                                                  // number of endo lags  
        hp("q")    = q                                                  // number of exo lags
        hp("spec") = s                                                  // trend specification: 0, 1, 2, 3, 5
        val mod = ARX (xe, y, hh)                                       // create model for time series data
        banner (s"In-ST Forecasts: ${mod.modelName} on COVID-19 Dataset")
        mod.trainNtest_x ()()                                           // train and test on full dataset
        println (mod.summary ())                                        // statistical summary of fit

//      mod.setSkip (p)                                                 // full AR-formula available when t >= p
        mod.forecastAll ()                                              // forecast h-steps ahead (h = 1 to hh) for all y
        mod.diagnoseAll (y, mod.getYf)
//      Forecaster.evalForecasts (mod, mod.getYb, hh)
//      println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")
//      println (s"Final In-ST Forecast Matrix yf = ${mod.getYf.shiftDiag}")
    end for

end aRXTest3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRXTest4` main function tests the `ARX` class on real data:
 *  Forecasting COVID-19 using Train-n-Test Split (TnT) with Rolling Validation.
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRXTest4
 */
@main def aRXTest4 (): Unit =

//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val exo_vars  = Array ("icu_patients", "hosp_patients")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val xe = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon
    hp("lwave") = 20                                                    // wavelength (distance between peaks)

    for p <- 1 to 10; s <- 1 to 5 do                                    // number of lags; trend
        hp("p")    = p                                                  // number of endo lags
//      hp("q")    = 1                                                  // number of exo lags
        hp("q")    = min (2, p)                                         // try various rules
        hp("spec") = s                                                  // trend specification: 0, 1, 2, 3, 5
        val mod = ARX (xe, y, hh)                                       // create model for time series data
        banner (s"TnT Forecasts: ${mod.modelName} on COVID-19 Dataset")
        mod.trainNtest_x ()()                                           // use customized trainNtest_x

        mod.setSkip (0)
        mod.rollValidate ()                                             // TnT with Rolling Validation
//      println (s"After Roll TnT Forecast Matrix yf = ${mod.getYf}")
        mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim), 0)     // only diagnose on the testing set
//      println (s"Final TnT Forecast Matrix yf = ${mod.getYf}")
    end for

end aRXTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRXTest5` main function tests the `ARX` class on real data:
 *  Forecasting COVID-19 using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  This version performs feature selection.
 *  > runMain scalation.modeling.forecasting.aRXTest5
 */
@main def aRXTest5 (): Unit =

    val exo_vars  = Array ("icu_patients", "hosp_patients")
//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val xe = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon
    hp("p")     = 10                                                    // endo lags
    hp("q")     = 10                                                    // exo lags
    hp("spec")  = 5                                                     // trend specification: 0, 1, 2, 3, 5
    hp("lwave") = 20                                                    // wavelength (distance between peaks)

    val mod = ARX (xe, y, hh)                                           // create model for time series data
    banner (s"In-ST Forecasts: ${mod.modelName} on COVID-19 Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset
    println (mod.summary ())                                            // statistical summary of fit

    mod.forecastAll ()                                                  // forecast h-steps ahead (h = 1 to hh) for all y
    Forecaster.evalForecasts (mod, mod.getYb, hh)
    println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")

    for tech <- SelectionTech.values do
        banner (s"Feature Selection Technique: $tech")
        val (cols, rSq) = mod.selectFeatures (tech, false)              // R^2, R^2 bar, sMAPE, R^2 cv
        val k = cols.size
        println (s"k = $k")
        new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "R^2 cv"),
                   s"R^2 vs k for ${mod.modelName} with $tech", lines = true)
        banner (s"Feature Importance with $tech")
        println (s"$tech: rSq = $rSq")
        val imp = mod.importance (cols.toArray, rSq)
        println (s"feature importance imp = $imp")
//      for (c, r) <- imp do println (s"col = $c, \t ${ox_fname(c)}, \t importance = $r")
    end for

end aRXTest5

