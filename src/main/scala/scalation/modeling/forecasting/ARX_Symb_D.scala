
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

import scalation.mathstat._

import MakeMatrix4TS._

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
 *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
 *  @param tForms   the map of transformations applied
 */
class ARX_Symb_D (x: MatrixD, y: MatrixD, hh: Int, n_exo: Int, fname: Array [String],
                  tRng: Range = null, hparam: HyperParameter = hp,
                  bakcast: Boolean = false,
                  tForms: TransformMap = Map ("tForm_y" -> null))
      extends ARX_D (x, y, hh, n_exo, fname, tRng, hparam, bakcast, tForms):

    private val debug = debugf ("ARX_Symb_D", true)                          // debug function

    modelName = s"ARX_Symb_D($p, $q, $n_exo)"

    debug ("init", s"$modelName with with $n_exo exogenous variables and additional term spec = $spec")
    debug ("init", s"[ x | y ] = ${x ++^ y}")

end ARX_Symb_D


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_Symb_D` companion object provides factory methods for the `ARX_Symb_D` class.
 */
object ARX_Symb_D extends MakeMatrix4TS:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARX_Symb_D` object by building an input matrix xy and then calling the
     *  `ARX_Symb_D` constructor.
     *  @param xe       the matrix of exogenous variable values
     *  @param y        the endogenous/response vector (main time series data)
     *  @param hh       the maximum forecasting horizon (h = 1 to hh)
     *  @param fname_   the feature/variable names
     *  @param tRng     the time range, if relevant (time index may suffice)
     *  @param hparam   the hyper-parameters
     *  @param fEndo    the array of transforms used to transform endogenous variables
     *  @param fExo     the array of transforms used to transform exogenous variables
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     */
    def apply (xe: MatrixD, y: VectorD, hh: Int, fname_ : Array [String] = null,
               tRng: Range = null, hparam: HyperParameter = hp,
               fEndo: Array [Transform] = Array (log1pForm),
               fExo: Array [Transform] = Array (log1pForm),
               bakcast: Boolean = false): ARX_Symb_D =

        val (n_fEndo, n_fExo) = (fEndo.length, fExo.length)
        val (xy, tForms)      = ARX_Symb.buildMatrix (xe, y, hparam, fEndo, fExo, bakcast)
        val yy                = makeMatrix4Y (y, hh, bakcast)
        val fname = if fname_ == null then formNames (xe.dim2, hparam, n_fEndo, n_fExo) else fname_
        new ARX_Symb_D (xy, yy, hh, xe.dim2, fname, tRng, hparam, bakcast, tForms)
    end apply 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARX_Symb_D` object by building an input matrix xy and then calling the
     *  `ARX_Symb_D` constructor, with rescaling of endogneous and exogenous variable values.
     *  @param xe       the matrix of exogenous variable values
     *  @param y        the endogenous/response vector (main time series data)
     *  @param hh       the maximum forecasting horizon (h = 1 to hh)
     *  @param fname_   the feature/variable names
     *  @param tRng     the time range, if relevant (time index may suffice)
     *  @param hparam   the hyper-parameters
     *  @param fEndo    the array of transforms used to transform endogenous variables
     *  @param fExo     the array of transforms used to transform exogenous variables
     *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
     *  @param tForm    the transform for y
     */
    def rescale (xe: MatrixD, y: VectorD, hh: Int, fname_ : Array [String] = null,
                 tRng: Range = null, hparam: HyperParameter = hp,
                 fEndo: Array [Transform] = Array (log1pForm),
                 fExo: Array [Transform] = Array (log1pForm),
                 bakcast: Boolean = false,
                 tForm: VectorD | MatrixD => Transform = x => zForm(x)): ARX_Symb_D =

        val (n_fEndo, n_fExo) = (fEndo.length, fExo.length)
        val (xy, tForms)      = ARX_Symb.buildMatrix (xe, y, hparam, fEndo, fExo, bakcast, tForm)
        val fname = if fname_ == null then formNames (xe.dim2, hparam, n_fEndo, n_fExo) else fname_
        val y_scl = tForms("tForm_y").f(y)
        if tForms("tForm_y").getClass.getSimpleName == "zForm" then hp("nneg") = 0
        val yy    = makeMatrix4Y (y_scl, hh, bakcast)
        new ARX_Symb_D (xy, yy, hh, xe.dim2, fname, tRng, hparam, bakcast, tForms)
    end rescale

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form an array of names for the features included in the model.
     *  @param n_exo  the number of exogenous variable
     *  @param hp_    the hyper-parameters
     *  @param n_fEn  the number of functions used to map endogenous variables
     *  @param n_fEx  the number of functions used to map exogenous variables
     */
    def formNames (n_exo: Int, hp_ : HyperParameter, n_fEn: Int, n_fEx: Int): Array [String] =
        ARX_Symb.formNames (n_exo, hp_, n_fEn, n_fEx)
    end formNames

end ARX_Symb_D

import Example_Covid._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_Symb_DTest3` main function tests the `ARX_Symb_D` class on real data:
 *  Forecasting COVID-19 using In-Sample Testing (In-ST).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRX_Symb_DTest3
 */
@main def aRX_Symb_DTest3 (): Unit =

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
    hp("lwave") = 20                                                    // wavelength (distance between peaks)
    hp("cross") = 1                                                     // 1 => add cross terms

    for p <- 6 to 6; q <- 4 to 4; s <- 1 to 1 do                        // number of lags (endo, exo); trend
        hp("p")    = p                                                  // endo lags
        hp("q")    = 2                                                  // exo lags
        hp("spec") = s                                                  // trend specification: 0, 1, 2, 3, 5
        val mod = ARX_Symb_D (xe, y, hh)                                // create model for time series data
        mod.inSampleTest ()                                             // In-sample Testing
        println (mod.summary ())                                        // statistical summary of fit
    end for

end aRX_Symb_DTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_Symb_DTest4` main function tests the `ARX_Symb_D` class on real data:
 *  Forecasting COVID-19 using Train and Test (TnT).
 *  Test forecasts (h = 1 to hh steps ahead forecasts).
 *  > runMain scalation.modeling.forecasting.aRX_Symb_DTest4
 */
@main def aRX_Symb_DTest4 (): Unit =

    val exo_vars  = Array ("icu_patients")
//  val exo_vars  = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xxe, yy) = loadData (exo_vars, response)
    println (s"xxe.dims = ${xxe.dims}, yy.dim = ${yy.dim}")

//  val xe = xxe                                                        // full
    val xe = xxe(0 until 116)                                           // clip the flat end
//  val y  = yy                                                         // full
    val y  = yy(0 until 116)                                            // clip the flat end
    val hh = 6                                                          // maximum forecasting horizon
    val pp = 1.5
    hp("lwave") = 20                                                    // wavelength (distance between peaks)
//  hp("cross") = 1                                                     // 1 => add cross terms

    val ff = Array [Transform] (powForm (VectorD (pp)))
    val gg = Array [Transform] ()

    for p <- 6 to 6; q <- 4 to 4; s <- 1 to 1 do                        // number of lags (endo, exo); trend
        hp("p")    = p                                                  // endo lags
        hp("q")    = q                                                  // exo lags
        hp("spec") = s                                                  // trend specification: 0, 1, 2, 3, 5
//        val mod = ARX_Symb_D (xe, y, hh, fEndo = ff, fExo = gg)         // create model for time series data
        val mod = ARX_Symb_D.rescale (xe, y, hh, fEndo = ff, fExo = gg)         // create model for time series data
        banner (s"TnT Forecasts: ${mod.modelName} on COVID-19 Dataset")
        mod.trainNtest_x ()()                                           // use customized trainNtest_x
//        println (mod.summary ())                                        // statistical summary of fit

        mod.setSkip (0)
        mod.rollValidate ()                                             // TnT with Rolling Validation
        println (s"After Roll TnT Forecast Matrix yf = ${mod.getYf}")
        mod.diagnoseAll (mod.getY, mod.getYf, Forecaster.teRng (y.dim), 0)    // only diagnose on the testing set
//      println (s"Final TnT Forecast Matrix yf = ${mod.getYf}")
    end for

end aRX_Symb_DTest4

