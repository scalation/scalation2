
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue Jan 14 15:47:45 EST 2025
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Auto-Regressive on lagged y and xe with SR terms (ARX_Symb) using OLS
 *
 *  @see `scalation.modeling.Regression`
 */

package scalation
package modeling
package forecasting

import scala.collection.mutable.ArrayBuffer
import scala.math.min

import scalation.mathstat._

import MakeMatrix4TS._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_Symb` class provides time series analysis capabilities for ARX Symbolic
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
 *  @param x        the data/input matrix (lagged columns of y and xe) @see `ARX_Symb.apply`
 *  @param y        the response/output vector (main time series data) 
 *  @param hh       the maximum forecasting horizon (h = 1 to hh)
 *  @param n_exo    the number of exogenous variables
 *  @param fname    the feature/variable names
 *  @param tRng     the time range, if relevant (time index may suffice)
 *  @param hparam   the hyper-parameters (defaults to `MakeMatrix4TS.hp`)
 *  @param itran    the inverse transformation to return to the original scale (defaults to null)
 *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
 */
class ARX_Symb (x: MatrixD, y: VectorD, hh: Int, n_exo: Int, fname: Array [String],
                tRng: Range = null, hparam: HyperParameter = hp,
                val itran: FunctionS2S = null, bakcast: Boolean = false)     // backcast value used only `MakeMatrix4TS`
      extends Forecaster_Reg (x, y, hh, fname, tRng, hparam, bakcast):       // no automatic backcasting

    private val debug = debugf ("ARX_Symb", true)                        // debug function
    private val p     = hparam("p").toInt                                // use the last p values (p lags)
    private val pp    = hparam("pp").toDouble                            // power to raise the endogenous lags to (defaults to quadratic)
    private val pr    = hparam("pr").toDouble                            // root to take the endogenous lags to (defaults to square root)
    private val q     = hparam("q").toInt                                // use the last q exogenous values (q lags)
    private val qp    = hparam("qp").toDouble                            // power to raise the exogenous lags to (defaults to quadratic)
    private val qr    = hparam("qr").toDouble                            // root to take the exogenous lags to (defaults to square root)
    private val spec  = hparam("spec").toInt                             // trend terms: 0 - none, 1 - constant, 2 - linear, 3 - quadratic
                                                                         //              4 - sine, 5 cosine
    modelName = s"ARX_Symb($p, $q, $n_exo)"

    debug ("init", s"$modelName with with $n_exo exogenous variables and additional term spec = $spec")
    debug ("init", s"[ x | y ] = ${x :^+ y}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forge a new vector from the first spec values of x, the last p-h+1 values
     *  of x (past values) and recent values 1 to h-1 from the forecasts.
     *  @param xx  the t-th row of the input matrix (lagged actual values)
     *  @param yy  the t-th row of the forecast matrix (forecasted future values)
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forge (xx: VectorD, yy: VectorD, h: Int): VectorD =
        val x_trend   = xx(0 until spec)                                 // get trend values

        // add terms for the endogenous variable
        val n_endo    = spec + p                                         // number of trend + endogenous values
        val x_act     = xx(n_endo-(p+1-h) until n_endo)                  // get actual lagged y-values (endogenous)
        val nyy       = p - x_act.dim                                    // number of forecasted values needed
//      println (s"forge: h = $h, n_nedo = $n_endo, [ ${x_trend.dim}, ${x_act.dim} ], nyy = $nyy")
        val x_fcast   = yy(h-nyy until h)                                // get forecasted y-values
        val xpp_act   = x_act ~^ pp                                      // get actual y^pp-values
        val xpp_fcast = x_fcast ~^ pp                                    // get forecasted y^pp-values
        val xpr_act   = x_act ~^ pr                                      // get actual y_root-values
        val xpr_fcast = x_fcast ~^ pr                                    // get forecasted y_root-values

        val xy = x_trend ++ x_act ++ x_fcast ++ xpp_act ++ xpp_fcast ++ xpr_act ++ xpr_fcast

        // add terms for the exogenous variables
        var exo = hide (xx(n_endo+2*p until n_endo+2*p + q), h)
        for j <- 1 until n_exo do                                                    // for the j-th exogenous variable
            exo = exo ++ hide (xx(n_endo+2*p + j*q until n_endo+2*p + (j+1)*q), h)   // get actual lagged xe-values for exogenous variable j
        val exo_pp = exo ~^ qp                                           // get exogenous y^qp-values
        val exo_pr = exo ~^ qr                                           // get exogenous y_root-values

        // add endogenous-exogenous cross terms
        // FIX - to be implemented

        xy ++ exo ++ exo_pp ++ exo_pr
    end forge

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Hide values at the end of vector z (last h-1 values) as the increasing horizon
     *  turns them in future values (hence unavailable).  Set these values to either
     *  zero (the default) or the last available value.
     *  @param z     the vector to shift
     *  @param h     the current horizon (number of steps ahead to forecast)
     *  @param fill  whether to backfill with the rightmost value (true) or with 0 (false)
     */
    def hide (z: VectorD, h: Int, fill: Boolean = true): VectorD =
        val lst = z.dim - h                                              // last available index position in z
        val zl  = if lst >= 0 then z(lst) else 0.0                       // last available z value per horizon
        val z_  = new VectorD (z.dim)
        for k <- z.indices do
            z_(k) = if k <= lst then z(k) else if fill then zl else 0.0
        z_
    end hide

end ARX_Symb

import Example_Covid._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_Symb` companion object provides factory methods for the `ARX_Symb` class.
 */
object ARX_Symb:

    private val bounds = (1.0, 5.0)                                     // (lower, upper) bounds for rescaling

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARX_Symb` object by building an input matrix xy and then calling the
     *  `ARX_Symb` constructor.
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
               bakcast: Boolean = false): ARX_Symb =
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
        val xy    = buildMatrix4TS (xe, yt, p, pp, pr, q, qp, qr, spec, lwave, cross, bakcast)
        val n_exo = xe.dim2
        val fname = if fname_ == null then formNames (spec, p, n_exo, q)
                    else fname_
        new ARX_Symb (xy, y, hh, n_exo, fname, tRng, hparam, itran, bakcast)
    end apply 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARX_Symb` object by building an input matrix xy and then calling the
     *  `ARX_Symb` constructor, with rescaling of endogneous and exogenous variable values.
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
                 bakcast: Boolean = false): ARX_Symb =
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
        val xy    = buildMatrix4TS (xet, yt, p, pp, pr, q, qp, qr, spec, lwave, cross, bakcast)
        new ARX_Symb (xy, y, hh, xe.dim2, fname, tRng, hparam, itran, bakcast)
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
                        bakcast: Boolean): MatrixD =

        // add trend terms and terms for the endogenous variable
        val xt = makeMatrix4T (y, spec, lwave, bakcast)                           // trend terms
        val xl = makeMatrix4L (y, p, bakcast)                                     // lagged linear terms
        var xy = xt ++^ xl ++^ xl~^pp ++^ xl~^pr

        // add terms for the exogenous variables
        if xe.dim2 > 0 then xy = xy ++^ makeMatrix4EXO (xe, q, 1, bakcast) ++^    // lagged linear terms
                                        makeMatrix4EXO (xe, q, qp, bakcast) ++^   // lagged power/quadratic terms
                                        makeMatrix4EXO (xe, q, qr, bakcast)       // lagged root/square-root terms

            // add cross terms of the endogenous and exogenous variables
            if cross then
                val yxe = y *~: xe                                                // element-wise multiplication of vector y and matrix xe 
                xy = xy ++^ makeMatrix4EXO (yxe, q, 1, bakcast)                   // lagged linear cross terms
        end if
        xy
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

end ARX_Symb


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_SymbTest` main function tests the `ARX_Symb` class on real data:
 *  Forecasting Lake Levels using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRX_SymbTest
 *
@main def aRX_SymbTest (): Unit =

    val hh = 3                                                          // maximum forecasting horizon

    val mod = ARX_Symb (y, hh)                                          // create model for time series data
    banner (s"In-ST Forecasts: ${mod.modelName} on LakeLevels Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset

    mod.forecastAll ()                                                  // forecast h-steps ahead (h = 1 to hh) for all y
    Forecaster.evalForecasts (mod, mod.getYb, hh)
    println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")

end aRX_SymbTest
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_SymbTest2` main function tests the `ARX_Symb` class on real data:
 *  Forecasting Lake Levels using Train-n-Test Split (TnT) with Rolling Validation.
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRX_SymbTest2
 *
@main def aRX_SymbTest2 (): Unit =

    val hh = 3                                                          // maximum forecasting horizon

    val mod = ARX_Symb (y, hh)                                          // create model for time series data
    banner (s"TnT Forecasts: ${mod.modelName} on LakeLevels Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset

    mod.rollValidate ()                                                 // TnT with Rolling Validation
    println (s"Final TnT Forecast Matrix yf = ${mod.getYf}")

end aRX_SymbTest2
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_SymbTest3` main function tests the `ARX_Symb` class on real data:
 *  Forecasting COVID-19 using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRX_SymbTest3
 */
@main def aRX_SymbTest3 (): Unit =

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

    for p <- 1 to 5; s <- 1 to 2; q <- 1 to 3 do                        // number of lags; trend; number of exo lags
        hp("p")    = p                                                  // endo lags
        hp("q")    = q                                                  // exo lags
        hp("spec") = s                                                  // trend specification: 0, 1, 2, 3, 5
        val mod = ARX_Symb (xe, y, hh)                                  // create model for time series data
        banner (s"In-ST Forecasts: ${mod.modelName} on COVID-19 Dataset")
        mod.trainNtest_x ()()                                           // train and test on full dataset
        println (mod.summary ())                                        // statistical summary of fit

//      mod.setSkip (p)                                                 // full AR-formula available when t >= p
        mod.forecastAll ()                                              // forecast h-steps ahead (h = 1 to hh) for all y
        mod.diagnoseAll (y, mod.getYf)                                  // QoF for each horizon
//      Forecaster.evalForecasts (mod, mod.getYb, hh)
//      println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")
//      println (s"Final In-ST Forecast Matrix yf = ${mod.getYf.shiftDiag}")
    end for

end aRX_SymbTest3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_SymbTest4` main function tests the `ARX_Symb` class on real data:
 *  Forecasting COVID-19 using Train-n-Test Split (TnT) with Rolling Validation.
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRX_SymbTest4
 */
@main def aRX_SymbTest4 (): Unit =

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

    for p <- 1 to 5; s <- 1 to 1 do                                     // number of lags; trend
        hp("p")    = p                                                  // endo lags
//      hp("q")    = 1                                                  // exo lags
        hp("q")    = min (2, p)                                         // try various rules
        hp("spec") = s                                                  // trend specification: 0, 1, 2, 3, 5
        val mod = ARX_Symb (xe, y, hh)                                  // create model for time series data
        banner (s"TnT Forecasts: ${mod.modelName} on COVID-19 Dataset")
        mod.trainNtest_x ()()                                           // use customized trainNtest_x

        mod.setSkip (0)
        mod.rollValidate ()                                             // TnT with Rolling Validation
        println (s"After Roll TnT Forecast Matrix yf = ${mod.getYf}")
        mod.diagnoseAll (y, mod.getYf, Forecaster.teRng (y.dim), 0)     // only diagnose on the testing set
//      println (s"Final TnT Forecast Matrix yf = ${mod.getYf}")
    end for

end aRX_SymbTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_SymbTest5` main function tests the `ARX_Symb` class on real data:
 *  Forecasting COVID-19 using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  This version performs feature selection.
 *  > runMain scalation.modeling.forecasting.aRX_SymbTest5
 */
@main def aRX_SymbTest5 (): Unit =

    val exo_vars  = Array ("icu_patients", "hosp_patients")
//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val xe = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon
    val p  = 10 
    val q  = 10
    hp("p")     = p                                                     // endo lags
    hp("pp")    = 1.5                                                   // use 1.5 for the power/exponent (default is 2)
    hp("q")     = q                                                     // exo lags
    hp("qp")    = 1.5                                                   // use 1.5 for the power/exponent (default is 2)
    hp("spec")  = 5                                                     // trend specification: 0, 1, 2, 3, 5
    hp("lwave") = 20                                                    // wavelength (distance between peaks)

    val mod = ARX_Symb (xe, y, hh)                                      // create model for time series data
    banner (s"In-ST Forecasts: ${mod.modelName} on COVID-19 Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset
    println (mod.summary ())                                            // statistical summary of fit

//  mod.setSkip (p)                                                     // full AR-formula available when t >= p
    mod.forecastAll ()                                                  // forecast h-steps ahead (h = 1 to hh) for all y
    mod.diagnoseAll (y, mod.getYf)                                      // QoF for each horizon
//  Forecaster.evalForecasts (mod, mod.getYb, hh)
//  println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")

    banner ("Feature Selection Technique: Forward")
    val (cols, rSq) = mod.forwardSelAll ()                              // R^2, R^2 bar, sMAPE, R^2 cv
//  val (cols, rSq) = mod.backwardElimAll ()                            // R^2, R^2 bar, sMAPE, R^2 cv
    val k = cols.size
    println (s"k = $k")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "R^2 cv"),
               s"R^2 vs n for ${mod.modelName}", lines = true)
    println (s"rSq = $rSq")

end aRX_SymbTest5

