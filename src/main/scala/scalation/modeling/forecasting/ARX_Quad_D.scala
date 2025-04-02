
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Yousef Fekri Dabanloo
 *  @version 2.0
 *  @date    Mon Mar 31 23:28:32 EDT 2025
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Quadratic, Auto-Regressive on lagged y and xe (ARX_Quad_D) using OLS - Direct Forecasting
 */

package scalation
package modeling
package forecasting

import scalation.mathstat._

import MakeMatrix4TS._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_Quad_D` class provides basic time series analysis capabilities for
 *  ARX_Quad_D models.  ARX_Quad_D models are often used for forecasting.
 *  `ARX_Quad_D` uses DIRECT (as opposed to RECURSIVE) multi-horizon forecasting.
 *  Given time series data stored in vector y, its next value y_t = combination of last p values.
 *
 *      y_t = b dot x_t + e_t
 *
 *  where y_t is the value of y at time t and e_t is the residual/error term.
 *  @param x        the data/input matrix (lagged columns of y) @see `ARX_Quad_D.apply`
 *  @param y        the response/output matrix (column per horizon) (time series data) 
 *  @param hh       the maximum forecasting horizon (h = 1 to hh)
 *  @param n_exo    the number of exogenous variables
 *  @param fname    the feature/variable names
 *  @param tRng     the time range, if relevant (time index may suffice)
 *  @param hparam   the hyper-parameters (defaults to `MakeMatrix4TS.hp`)
 *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
 *  @param tForms   the map of transformations applied
 */
class ARX_Quad_D (x: MatrixD, y: MatrixD, hh: Int, n_exo: Int, fname: Array [String] = null,
                  tRng: Range = null, hparam: HyperParameter = hp,
                  bakcast: Boolean = false,  
                  tForms: TransformMap = Map ("tForm_y" -> null))
      extends ARX_D (x, y, hh, n_exo, fname, tRng, hparam, bakcast, tForms):

    private val debug = debugf ("ARX_Quad_D", true)                     // debug function

    modelName = s"ARX_Quad_D($p, $q, $n_exo)"

    debug ("init", s"$modelName with $n_exo exogenous variables and additional term spec = $spec")
//  debug ("init", s"[ x | y ] = ${x ++^ y}")

end ARX_Quad_D


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_Quad_D` companion object provides factory methods for the `ARX_Quad_D` class.
 */
object ARX_Quad_D extends MakeMatrix4TS:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARX_Quad_D` object by building an input matrix x and then calling the constructor.
     *  @param xe      the matrix of exogenous variable values
     *  @param y       the response vector (time series data)
     *  @param hh      the maximum forecasting horizon (h = 1 to hh)
     *  @param fname_  the feature/variable names
     *  @param tRng    the time range, if relevant (time index may suffice)
     *  @param hparam  the hyper-parameters (defaults for `MakeMatrix4TS.hp`)
     *  @param fEndo    the array of functions used to transform endogenous variables
     *  @param fExo     the array of functions used to transform exogenous variables
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     */
    def apply (xe: MatrixD, y: VectorD, hh: Int, fname_ : Array [String] = null,
               tRng: Range = null, hparam: HyperParameter = hp,
               fEndo: Array [Transform] = null, fExo: Array [Transform] = null,
               bakcast: Boolean = false): ARX_Quad_D =

        val (xy, tForms) = ARX_Quad.buildMatrix (xe, y, hparam, bakcast)
        val yy    = makeMatrix4Y (y, hh, bakcast)
        val fname = if fname_ == null then ARX_Quad.formNames (xe.dim2, hparam) else fname_
        new ARX_Quad_D (xy, yy, hh, xe.dim2, fname, tRng, hparam, bakcast, tForms)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARX_Quad_D` object by building an input matrix xy and then calling the
     * `ARX_Quad_D` constructor.  Also rescale the input data.
     *  @param xe       the matrix of exogenous variable values
     *  @param y        the endogenous/response vector (main time series data)
     *  @param hh       the maximum forecasting horizon (h = 1 to hh)
     *  @param tRng     the time range, if relevant (time index may suffice)
     *  @param hparam   the hyper-parameters
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     *  @param tForm    the z-transform (rescale to standard normal)
     */
    def rescale (xe: MatrixD, y: VectorD, hh: Int, fname_ : Array [String] = null,
                 tRng: Range = null, hparam: HyperParameter = hp,
                 fEndo: Array [Transform] = null, fExo: Array [Transform] = null,
                 bakcast: Boolean = false,
                 tForm: VectorD | MatrixD => Transform = x => zForm(x)): ARX_Quad_D =

        val (xy, tForms) = ARX_Quad.buildMatrix (xe, y, hparam, bakcast, tForm)
        val y_scl = tForms ("tForm_y").f(y)
        val yy = makeMatrix4Y (y_scl, hh, bakcast)
        if tForms("tForm_y").getClass.getSimpleName == "zForm" then hp("nneg") = 0
        val fname = if fname_ == null then formNames (xe.dim2, hparam) else fname_
        new ARX_Quad_D (xy, yy, hh, xe.dim2, fname, tRng, hparam, bakcast, tForms)
    end rescale

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form an array of names for the features included in the model.
     *  @param n_exo  the number of exogenous variable
     *  @param hp_    the hyper-parameters
     *  @param n_fEn  the number of functions used to map endogenous variables
     *  @param n_fEx  the number of functions used to map exogenous variables
     */
    def formNames (n_exo: Int, hp_ : HyperParameter, n_fEn: Int, n_fEx: Int): Array [String] =
        ARX_Quad.formNames (n_exo, hp_, n_fEn, n_fEx)
    end formNames

end ARX_Quad_D

import Example_Covid.{loadData, response}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_Quad_DTest` main function tests the `ARX_Quad_D` class on real data:
 *  Forecasting Lake Levels using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRX_Quad_DTest
 *
@main def aRX_Quad_DTest (): Unit =

    val hh = 3                                                          // maximum forecasting horizon

    val mod = ARX_Quad_D (y, hh)                                             // create model for time series data
    banner (s"In-ST Forecasts: ${mod.modelName} on LakeLevels Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset

    mod.forecastAll ()                                                  // forecast h-steps ahead (h = 1 to hh) for all y
    println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")

end aRX_Quad_DTest
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_Quad_DTest2` main function tests the `ARX_Quad_D` class on real data:
 *  Forecasting Lake Levels using Train-n-Test Split (TnT) with Rolling Validation.
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRX_Quad_DTest2
 *
@main def aRX_Quad_DTest2 (): Unit =

    val hh = 3                                                          // maximum forecasting horizon

    val mod = ARX_Quad_D (y, hh)                                             // create model for time series data
    banner (s"TnT Forecasts: ${mod.modelName} on LakeLevels Dataset")
    mod.trainNtest_x ()()                                               // train and test on full dataset

    mod.rollValidate ()                                                 // TnT with Rolling Validation
    println (s"Final TnT Forecast Matrix yf = ${mod.getYf}")

end aRX_Quad_DTest2
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_Quad_DTest3` main function tests the `ARX_Quad_D` class on real data:
 *  Forecasting COVID-19 using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRX_Quad_DTest3
 */
@main def aRX_Quad_DTest3 (): Unit =

//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val exo_vars  = Array ("icu_patients")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val xe = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon
    hp("pp")    = 1.5
    hp("lwave") = 20                                                    // wavelength (distance between peaks)

    for p <- 6 to 6; q <- 4 to 4; s <- 1 to 1 do                        // number of lags (endo, exo); trend
        hp("p")    = p                                                  // mumber of endo lags
        hp("q")    = q                                                  // mumber of exo lags
        hp("spec") = s                                                  // trend specification: 0, 1, 2, 3, 5
        val mod = ARX_Quad_D.rescale (xe, y, hh)                        // create model for time series data
        mod.inSampleTest ()                                             // In-sample Testing
        println (mod.summary ())                                        // statistical summary of fit  FIX - crashes
    end for

end aRX_Quad_DTest3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_Quad_DTest4` main function tests the `ARX_Quad_D` class on real data:
 *  Forecasting COVID-19 using Train-n-Test Split (TnT) with Rolling Validation.
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRX_Quad_DTest4
 */
@main def aRX_Quad_DTest4 (): Unit =

//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val exo_vars  = Array ("icu_patients")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val xe = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon
    hp("pp")    = 1.5
    hp("lwave") = 20                                                    // wavelength (distance between peaks)

    for p <- 6 to 6; q <- 4 to 4; s <- 1 to 1  do                       // number of lags (endo, exo); trend
        hp("p")    = p                                                  // number of endo lags
        hp("q")    = q                                                  // try various rules
        hp("spec") = s                                                  // trend specification: 0, 1, 2, 3, 5
//        val mod = ARX_Quad_D (xe, y, hh)
        val mod = ARX_Quad_D.rescale (xe, y, hh)                        // create model for time series data
        banner (s"TnT Forecasts: ${mod.modelName} on COVID-19 Dataset")
        mod.trainNtest_x ()()                                           // use customized trainNtest_x

        mod.setSkip (0)
        mod.rollValidate ()
//      println (s"After Roll TnT Forecast Matrix yf = ${mod.getYf}")
        mod.diagnoseAll (mod.getY, mod.getYf, Forecaster.teRng (y.dim))        // only diagnose on the testing set
//      println (s"Final TnT Forecast Matrix yf = ${mod.getYf}")
    end for

end aRX_Quad_DTest4


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_Quad_DTest5` main function tests the `ARX_Quad_D` object's ability to build input
 *  matrices.  Build an input/predictor data matrix for the COVID-19 dataset.
 *  > runMain scalation.modeling.forecasting.aRX_Quad_DTest5
 *
@main def aRX_Quad_DTest5 (): Unit =

//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val exo_vars  = Array ("icu_patients", "hosp_patients")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe    = xxe                                                      // full
    val xe    = xxe(0 until 116)                                         // clip the flat end
//  val y     = yy                                                       // full
    val y     = yy(0 until 116)                                          // clip the flat end
    val p     = 3                                                        // the number of endo lags
    val pp    = 3
    val q     = 2                                                        // the number of exo lags
    val spec  = 1                                                        // additional terms
    val lwave = 20                                                       // wavelength (distance between peaks)
    val hh    = 2                                                        // maximum forecasting horizon

    println (s"y = $y")

    val (x, y_) = ARX_Quad_D.buildMatrix4TS (xe, y, p, pp, q, hh, spec, lwave)

    println (s"y.dim = ${y.dim}, x.dims = ${x.dims}, y_.dims = ${y_.dims}")

end aRX_Quad_DTest5
 */

