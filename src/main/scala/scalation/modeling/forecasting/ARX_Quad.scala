
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Yousef Fekri Dabanloo
 *  @version 2.0
 *  @date    Sun Jun 30 13:27:00 EDT 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Auto-Regressive on lagged y and xe with quadratic terms (ARX_Quad) using OLS
 *
 *  @see `scalation.modeling.Regression`
 */

package scalation
package modeling
package forecasting

import scala.collection.mutable.ArrayBuffer

import scalation.mathstat._

import MakeMatrix4TS._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_Quad` class provides basic time series analysis capabilities for ARX quadratic models.
 *  ARX quadratic models utilize quadratic multiple linear regression based on lagged values of y.
 *  ARX models build on `ARY` by including one or more exogenous (xe) variables.
 *  Given time series data stored in vector y, its next value y_t = combination of
 *  last p values of y, y^2 and the last q values of each exogenous variable xe_j.
 *
 *      y_t = b dot x_t + e_t
 *
 *  where y_t is the value of y at time t and e_t is the residual/error term.
 *  @param x        the data/input matrix (lagged columns of y, y^2 and xe) @see `ARX_Quad.apply`
 *  @param y        the response/output vector (time series data) 
 *  @param hh       the maximum forecasting horizon (h = 1 to hh)
 *  @param n_exo    the number of exogenous variables
 *  @param fname    the feature/variable names
 *  @param tRng     the time range, if relevant (time index may suffice)
 *  @param hparam   the hyper-parameters (defaults to `MakeMatrix4TS.hp`)
 *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
 *  @param tForms   the map of transformation applied
 */
class ARX_Quad (x: MatrixD, y: VectorD, hh: Int, n_exo: Int, fname: Array [String],
                tRng: Range = null, hparam: HyperParameter = hp,
                bakcast: Boolean = false,                               // backcast value used only `MakeMatrix4TS`
                tForms: TransformMap = Map ("tForm_y" -> null))
      extends ARX (x, y, hh, n_exo, fname, tRng, hparam, bakcast, tForms):  // no automatic backcasting

    private val debug = debugf ("ARX_Quad", true)                       // debug function

    modelName = s"ARX_Quad($p, $q, $n_exo)"

    debug ("init", s"$modelName with with $n_exo exogenous variables and additional term spec = $spec")
    debug ("init", s"[ x | y ] = ${x :^+ y}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forge a new vector from the first spec values of x, the last p-h+1 values
     *  of x (past values) and recent values 1 to h-1 from the forecasts.
     *  @param xx  the t-th row of the input matrix (lagged actual values)
     *  @param yy  the t-th row of the forecast matrix (forecasted future values)
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    override def forge (xx: VectorD, yy: VectorD, h: Int): VectorD =
        // add terms for the endogenous variable
        val n_endo  = spec + p                                           // number of trend + endogenous values
        val x_act   = xx(n_endo - (p+1-h) until n_endo)                  // get actual lagged y-values (endogenous)
        val nyy     = p - x_act.dim                                      // number of forecasted values needed
        val x_fcast = yy(h-nyy until h)                                  // get forecasted y-values

        val x_act_pp = xx(n_endo+p - (p+1-h) until n_endo+p)             // get transformed lagged endogenous variable
        val x_fcast_pp = scaleCorrection (x_fcast)

        var xy = x_act ++ x_fcast ++ x_act_pp ++ x_fcast_pp              // add transformed lagged forecasted y-values
        for j <- 0 until n_exo do                                        // for the j-th exogenous variable
            xy = xy ++ hide (xx(n_endo+p + j*q until n_endo+p + (j+1)*q), h)
        xx(0 until spec) ++ xy
    end forge

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Apply scale correction to x_fcast.
     *  @param x_fcast  the vector to apply the scale correction to
     */
    def scaleCorrection (x_fcast: VectorD): VectorD =
        if tForms("tForm_y") != null then
            val f_pp = (tForms("tForm_endo").asInstanceOf [Transform].f(_: VectorD)) ⚬
                       (tForms("ppForm").asInstanceOf [Transform].f(_: VectorD)) ⚬
                       (tForms("tForm_y").asInstanceOf [Transform].fi(_: VectorD))
            f_pp (x_fcast)
        else
            tForms("ppForm").asInstanceOf [Transform].f(x_fcast)
    end scaleCorrection

end ARX_Quad


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_Quad` companion object provides factory methods for the `ARX_Quad` class.
 */
object ARX_Quad extends MakeMatrix4TS:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARX_Quad` object by building an input matrix xy and then calling the
     *  `ARX_Quad` constructor.
     *  @param xe       the matrix of exogenous variable values
     *  @param y        the endogenous/response vector (main time series data)
     *  @param hh       the maximum forecasting horizon (h = 1 to hh)
     *  @param fname_   the feature/variable names
     *  @param tRng     the time range, if relevant (time index may suffice)
     *  @param hparam   the hyper-parameters
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     */
    def apply (xe: MatrixD, y: VectorD, hh: Int, fname_ : Array [String] = null,
               tRng: Range = null, hparam: HyperParameter = hp,
               bakcast: Boolean = false): ARX_Quad =

        val pp = hparam("pp").toDouble
        val ppForm = powForm (VectorD (pp))
        val tForms: TransformMap = Map ("tForm_y" -> null, "yForm_endo" -> null, "ppForm" -> ppForm)
        val y_ypp = MatrixD (y, ppForm.f(y)).transpose
        println(ppForm.f(y))
        val xy    = buildMatrix (xe, y_ypp, hparam, bakcast)
        val fname = if fname_ == null then formNames (xe.dim2, hparam) else fname_
        new ARX_Quad (xy, y, hh, xe.dim2, fname, tRng, hparam, bakcast, tForms)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARX_Quad` object by building an input matrix xy and then calling the
     *  `ARX_Quad` constructor.  Also rescale the input data.
     *  @param xe       the matrix of exogenous variable values
     *  @param y        the endogenous/response vector (main time series data)
     *  @param hh       the maximum forecasting horizon (h = 1 to hh)
     *  @param tRng     the time range, if relevant (time index may suffice)
     *  @param hparam   the hyper-parameters
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     *  @param tForm    the z-transform (rescale to standard normal)
     */
    def rescale (xe: MatrixD, y: VectorD, hh: Int, fname_ : Array [String] = null,
                 tRng: Range = null, hparam: HyperParameter = hp, bakcast: Boolean = false,
                 tForm: VectorD | MatrixD => Transform = x => zForm(x)): ARX_Quad =
        val pp = hparam("pp").toDouble
        val tForm_y = tForm(y)
        if tForm_y.getClass.getSimpleName == "zForm" then hp("nneg") = 0
        val y_scal = tForm_y.f(y)
        val ppForm = powForm (VectorD (pp))

        var ypp = ppForm.f(y)
        val tForm_endo = tForm (ypp)
        ypp = tForm_endo.f(ypp)

        val tForms = Map ("tForm_y" -> tForm_y, "tForm_endo" -> tForm_endo, "ppForm" -> ppForm)
        val y_ypp = MatrixD (y_scal, ypp).transpose

        val n_exo = xe.dim2
        val x_exo: MatrixD =
        if n_exo > 0 then
            val xe_bfill = new MatrixD (xe.dim, xe.dim2)
            for j <- xe.indices2 do xe_bfill(?, j) = backfill (xe(?, j))
            val tForm_exo = tForm (xe_bfill)
            tForm_exo.f(xe_bfill)
        else
            new MatrixD (0, 0)

        val xy    = buildMatrix (x_exo, y_ypp, hparam, bakcast)
        val fname = if fname_ == null then formNames (n_exo, hparam) else fname_
        new ARX_Quad (xy, y_scal, hh, n_exo, fname, tRng, hparam, bakcast, tForms)
    end rescale

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build the input matrix by combining the p + spec columns for the trend and
     *  endogenous variable with the q * xe.dim2 columns for the exogenous variables.
     *  @param xe       the matrix of exogenous variable values
     *  @param y_ypp    the response vector (time series data) and raised to power pp
     *  @param hp_      the hyper-parameters
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     */
    def buildMatrix (xe: MatrixD, y_ypp: MatrixD, hp_ : HyperParameter, bakcast: Boolean): MatrixD =
        val (p, q, spec, lwave) = (hp_("p").toInt, hp_("q").toInt , hp_("spec").toInt, hp_("lwave").toDouble)
        makeMatrix4T (y_ypp(?, 0), spec, lwave, bakcast) ++^         // trend terms
        makeMatrix4L (y_ypp, p, bakcast) ++^                         // regular lag terms for y and y^pp
        makeMatrix4EXO (xe, q, 1, bakcast)                           // add exogenous terms
    end buildMatrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form an array of names for the features included in the model.
     *  @param n_exo  the number of exogenous variable
     *  @param hp_    the hyper-parameters
     */
    def formNames (n_exo: Int, hp_ : HyperParameter): Array [String] =
        val (p, q, spec) = (hp_("p").toInt, hp_("q").toInt , hp_("spec").toInt)
        val names = ArrayBuffer [String] ()
        for j <- 0 until n_exo; k <- q to 1 by -1 do names += s"xe${j}l$k"
        MakeMatrix4TS.formNames (spec, p, true) ++ names.toArray
    end formNames

end ARX_Quad

import Example_Covid._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_QuadTest` main function tests the `ARX_Quad` class on real data:
 *  Forecasting Lake Levels using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRX_QuadTest
 *
@main def aRX_QuadTest (): Unit =

    val hh = 3                                                          // maximum forecasting horizon

    val mod = ARX_Quad (y, hh)                                          // create model for time series data
    banner (s"In-ST Forecasts: ${mod.modelName} on LakeLevels Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset

    mod.forecastAll ()                                                  // forecast h-steps ahead (h = 1 to hh) for all y
    Forecaster.evalForecasts (mod, mod.getYb, hh)
    println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")

end aRX_QuadTest
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_QuadTest2` main function tests the `ARX_Quad` class on real data:
 *  Forecasting Lake Levels using Train-n-Test Split (TnT) with Rolling Validation.
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRX_QuadTest2
 *
@main def aRX_QuadTest2 (): Unit =

    val hh = 3                                                          // maximum forecasting horizon

    val mod = ARX_Quad (y, hh)                                          // create model for time series data
    banner (s"TnT Forecasts: ${mod.modelName} on LakeLevels Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset

    mod.rollValidate ()                                                 // TnT with Rolling Validation
    println (s"Final TnT Forecast Matrix yf = ${mod.getYf}")

end aRX_QuadTest2
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_QuadTest3` main function tests the `ARX_Quad` class on real data:
 *  Forecasting COVID-19 using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRX_QuadTest3
 */
@main def aRX_QuadTest3 (): Unit =

//  val exo_vars  = NO_EXO
    val exo_vars  = Array ("icu_patients")
//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val xe = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon
    hp("pp")    = 1.9                                                   // use 1.9 for the power/exponent (default is 2)
    hp("lwave") = 20                                                    // wavelength (distance between peaks)

    for p <- 6 to 6; q <- 4 to 4; s <- 1 to 1 do                        // number of endo lags; exo lags; trend
        hp("p")    = p                                                  // endo lags
        hp("q")    = q                                                  // exo lags
        hp("spec") = s                                                  // trend specification: 0, 1, 2, 3, 5
        val mod = ARX_Quad.rescale (xe, y, hh)                          // create model for time series data
        banner (s"In-ST Forecasts: ${mod.modelName} on COVID-19 Dataset")
        mod.trainNtest_x ()()                                           // train and test on full dataset
        println (mod.summary ())                                        // statistical summary of fit

//      mod.setSkip (p)                                                 // full AR-formula available when t >= p
        mod.forecastAll ()                                              // forecast h-steps ahead (h = 1 to hh) for all y
        mod.diagnoseAll (mod.getY, mod.getYf)
//      Forecaster.evalForecasts (mod, mod.getYb, hh)
//      println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")
//      println (s"Final In-ST Forecast Matrix yf = ${mod.getYf.shiftDiag}")
    end for

end aRX_QuadTest3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_QuadTest4` main function tests the `ARX_Quad` class on real data:
 *  Forecasting COVID-19 using Train-n-Test Split (TnT) with Rolling Validation.
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRX_QuadTest4
 */
@main def aRX_QuadTest4 (): Unit =

    val exo_vars  = Array ("icu_patients")
//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val xe = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon
    hp("pp")    = 1.5                                                   // use 1.5 for the power/exponent (default is 2)
    hp("lwave") = 20                                                    // wavelength (distance between peaks)

    for p <- 6 to 6; q <- 4 to 4; s <- 1 to 1 do                                     // number of lags; trend
        hp("p")    = p                                                  // endo lags
        hp("q")    = q                                                  // exo lags
        hp("spec") = s                                                  // trend specification: 0, 1, 2, 3, 5
        val mod = ARX_Quad.rescale (xe, y, hh)                          // create model for time series data

        banner (s"TnT Forecasts: ${mod.modelName} on COVID-19 Dataset")
        mod.trainNtest_x ()()                                           // use customized trainNtest_x

        mod.setSkip (0)
        mod.rollValidate ()                                             // TnT with Rolling Validation
        println (s"After Roll TnT Forecast Matrix yf = ${mod.getYf}")
        mod.diagnoseAll (mod.getY, mod.getYf, Forecaster.teRng (y.dim), 0)     // only diagnose on the testing set
//      println (s"Final TnT Forecast Matrix yf = ${mod.getYf}")
    end for

end aRX_QuadTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_QuadTest5` main function tests the `ARX_Quad` class on real data:
 *  Forecasting COVID-19 using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  This version performs feature selection.
 *  > runMain scalation.modeling.forecasting.aRX_QuadTest5
 */
@main def aRX_QuadTest5 (): Unit =

    val exo_vars  = Array ("icu_patients")
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
    hp("spec")  = 5                                                     // trend specification: 0, 1, 2, 3, 5
    hp("lwave") = 20                                                    // wavelength (distance between peaks)

    val mod = ARX_Quad (xe, y, hh)                                      // create model for time series data
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

end aRX_QuadTest5

