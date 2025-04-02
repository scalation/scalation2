
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Feb 13 16:22:21 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Quadratic AutoRegressive with eXogenous Variables
 *                  (Time Series Generalized Quadratic Multi-Variate Regression)
 */

package scalation
package modeling
package forecasting_old

import scala.math.max

import scalation.mathstat._

import neuralnet.RegressionMV

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_Quad_MV` object supports quadratic regression for Time Series data.
 *  Multi-horizon forecasting supported via the DIRECT method.
 *  Given a response vector y, a predictor matrix x is built that consists of
 *  lagged y vectors.   Additional future response vectors are built for training.
 *      y_t = b dot x
 *  where x = [y_{t-1}, y_{t-2}, ... y_{t-lags}].
 *  Matrix x includes constant, linear and generalized quadratic terms (x^pw where pw defaults to 2.0).
 *  For example, a lower power such as 1.5 may work better for longer horizons (pw may be tuned).
 */
object ARX_Quad_MV:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `RegressionMV` object from a Time Series response vector y.
     *  The input/data matrix x is formed from the lagged y vectors as columns in matrix x.
     *  Quadratic terms are added to the model, one for each lag.
     *  @param y          the original un-expanded output/response vector
     *  @param lags       the maximum lag included (inclusive)
     *  @param h          the forecasting horizon (1, 2, ... h)
     *  @param pw         the power to raise the variables to (x_j ^ pw) defaults to 2.0
     *  @param intercept  whether to add a column of all ones to the matrix (intercept)
     *  @param hparam     the hyper-parameters ((use Regression.hp for default)
     */
    def apply (y: VectorD, lags: Int, h: Int, pw: Double = 2.0, intercept: Boolean = true,
               hparam: HyperParameter = Regression.hp): RegressionMV =
        val (x_, yy) = buildMatrix4TS (y, lags, h)                        // column for each lag
        var x = x_ ++^ x_ ~^2                                             // add quadratic-ish terms
        if intercept then x = VectorD.one (yy.dim) +^: x                  // add first column of all ones

        val mod = new RegressionMV (x, yy, null, hparam)
        mod.modelName = s"ARX_Quad_MV_$lags"
        mod
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `RegressionMV` object from a response matrix.  The input/data matrix
     *  x is formed from the lagged y vectors as columns in matrix x.
     *  This method provides data rescaling.
     *  @param y          the original un-expanded output/response vector
     *  @param lags       the maximum lag included (inclusive)
     *  @param h          the forecasting horizon (1, 2, ... h)
     *  @param pw         the power to raise the variables to (x_j ^ pw) defaults to 2.0
     *  @param intercept  whether to add a column of all ones to the matrix (intercept)
     *  @param hparam     the hyper-parameters (use Regression.hp for default)
     */
    def rescale (y: VectorD, lags: Int, h: Int, pw: Double = 2.0, intercept: Boolean = true,
                 hparam: HyperParameter = Regression.hp): RegressionMV =
        val (x_, yy) = buildMatrix4TS (y, lags, h)                       // column for each lag
        var x = x_ ++^ x_ ~^ pw                                          // add quadratic-ish terms
        x = scale (extreme (x), (1.0, 5.0))(x)                           // rescale vector x matrix to [1, 5]
        if intercept then x = VectorD.one (yy.dim) +^: x                 // add first column of all ones

        val mod = new RegressionMV (x, yy, null, hparam)
        mod.modelName = s"ARX_Quad_MV_$lags"
        mod
    end rescale

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `RegressionMV` object from a response vector to fit a quadratic
     *  surface to Time Series data.  The input/data matrix x is formed from the
     *  lagged y vectors as columns in matrix x.
     *  In addition, lagged exogenous variables are added.
     *  @param y          the original un-expanded output/response vector
     *  @param lags       the maximum lag included (inclusive)
     *  @parax ex         the input matrix for exogenous variables (one per column)
     *  @param h          the forecasting horizon (1, 2, ... h)
     *  @param pw         the power to raise the variables to (x_j ^ pw) defaults to 2.0
     *  @param intercept  whether to add a column of all ones to the matrix (intercept)
     *  @param hparam     the hyper-parameters (use Regression.hp for default)
     *  @param elag1      the minimum exo lag included (inclusive)
     *  @param elag2      the maximum exo lag included (inclusive)
     */
    def exo (y: VectorD, lags: Int, ex: MatrixD, h: Int, pw: Double = 2.0,
             intercept: Boolean = true, hparam: HyperParameter = Regression.hp)
            (elag1: Int = max (1, lags / 5),
             elag2: Int = max (1, lags)): RegressionMV =
        val (x_, yy) = buildMatrix4TS (y, lags, h)                        // column for each lag
        var x = x_ ++^ x_ ~^ pw                                           // add quadratic-ish terms
        if intercept then x = VectorD.one (yy.dim) +^: x                  // add first column of all ones
        val endoCols = x.dim2
        println (s"endogenous: columns = $endoCols")

        val z = ARX.makeExoCols (lags, ex, elag1, elag2)                  // columns for exo vars
        x = x ++^ z ++^ z ~^ pw                                           // add linear and quad-ish terms for exo vars
        println (s"exogenous: columns = ${x.dim2 - endoCols}")

        println (s"exo: x.dims = ${x.dims}, yy.dim = ${yy.dim}")
//      println (s"exo: x = $x \n yy = $yy")
        mathstat.diagnoseMat (x) 
        val mod = new RegressionMV (x, yy, null, hparam)
        mod.modelName = s"ARX_Quad_MV.exo$lags"
        mod
    end exo

end ARX_Quad_MV


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_Quad_MVTest` main function tests the `ARX_Quad_MV` object.
 *  This test is used to CHECK that the buildMatrix4TS function is working correctly.
 *  May get NaN for some maximum lags (p) due to multi-collinearity.
 *  > runMain scalation.modeling.forecasting.aRX_Quad_MVTest
 */
@main def aRX_Quad_MVTest (): Unit =

    val m = 30
    val y = VectorD.range (1, m)                                       // used to CHECK the buildMatrix4TS function
    val h = 3                                                          // the forecasting horizon

    for p <- 5 to 5 do                                                 // autoregressive hyper-parameter p
        banner (s"Test: ARX_Quad_MV with $p lags")
        val mod = ARX_Quad_MV (y, p, h)                            // create model for time series data
        mod.trainNtest ()()                                            // train the model on full dataset
        println (mod.summary)

        val yy = mod.getYY
        val yp = mod.predict (mod.getX)
        for k <- yp.indices2 do
            new Plot (null, yy(?, k), yp(?, k), s"yy_$k vs. yp_$k for ${mod.modelName} (h=${k+1}) with $p lags", lines = true)
        end for
    end for

end aRX_Quad_MVTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_Quad_MVTest2` main function tests the `ARX_Quad_MV` class on real data:
 *  Forecasting lake levels.  Uses quadratic regression.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRX_Quad_MVTest2
 */
@main def aRX_Quad_MVTest2 (): Unit =

    import forecasting.Example_LakeLevels.y

    val h = 2                                                          // the forecasting horizon

    for p <- 1 to 7 do                                                 // autoregressive hyper-parameter p
        banner (s"Test: ARX_Quad_MV with $p lags")
        val mod = ARX_Quad_MV (y, p, h)                            // create model for time series data
        mod.trainNtest ()()                                            // train the model on full dataset
        println (mod.summary)

        banner ("Predictions/Forecasts")                               // direct forecasting technique
        val yy = mod.getYY
        val yf = mod.predict (mod.getX)
        for k <- yf.indices2 do
            new Plot (null, yy(?, k), yf(?, k), s"yy_$k vs. yf_$k for ${mod.modelName} (h=${k+1}) with $p lags", lines = true)
        end for
        println (s"yf = $yf")
        println (s"yf.dims = ${yf.dims}")
    end for

end aRX_Quad_MVTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_Quad_MVTest3` main function tests the `ARX_Quad_MV` class on real data:
 *  Forecasting COVID-19 Weekly Data.  Uses quadratic regression, In-Sample Testing using
 *  endogenous variable.
 *  > runMain scalation.modeling.forecasting.aRX_Quad_MVTest3
 */
@main def aRX_Quad_MVTest3 (): Unit =

    val LAGS = 10                                                           // number of lags
    val h    = 6                                                            // forecasting horizon

    val exo_vars = Array.ofDim [String] (0)                                 // no exogenous variables in this case
    val (xx, yy) = forecasting.Example_Covid.loadData (exo_vars, "new_deaths")
    val iskip = yy.indexWhere (_ >= 6.0)                                    // find day with at least 6 deaths
    println (s"iskip = $iskip is first day with at least 6 deaths")

    val ex = xx(iskip until xx.dim)                                         // trim away the first iskip rows
    val y  = yy(iskip until yy.dim)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sample ARX_Quad_MV on COVID-19 Weekly Data")
    val mod = ARX_Quad_MV (y, LAGS, h)                                      // create model for time series data
//  val mod = ARX_Quad_MV.rescale (y, LAGS, h)                              // create model for time series data - scaling
    val (yp, qof) = mod.trainNtest ()()                                     // train on full and test on full
    val yy_ = y(LAGS until y.dim)

    for k <- 0 until h do
        new Plot (null, yy_, yp(?, k), s"${mod.modelName}, yy vs. yp @ h = $k", lines = true)
    end for

    banner (s"Feature Selection Technique: Stepwise")
    val (cols, rSq) = mod.stepwiseSelAll (cross = false)                     // R^2, R^2 bar, sMAPE, NA
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "NA"),
               s"R^2 vs n for ARX_Quad_MV with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"Stepwise: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

end aRX_Quad_MVTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_Quad_MVTest4` main function tests the `ARX_Quad_MV` class on real data:
 *  Forecasting COVID-19 Weekly Data.  Uses quadratic regression, In-Sample Testing using endogenous
 *  and exogeneous variables.
 *  > runMain scalation.modeling.forecasting.aRX_Quad_MVTest4
 */
@main def aRX_Quad_MVTest4 (): Unit =

    val LAGS = 10                                                           // number of lags
    val h    = 6                                                            // forecasting horizon

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xx, yy) = forecasting.Example_Covid.loadData (exo_vars, "new_deaths")
    val iskip = yy.indexWhere (_ >= 6.0)                                    // find day with at least 6 deaths
    println (s"iskip = $iskip is first day with at least 6 deaths")

    val ex = xx(iskip until xx.dim)                                         // trim away the first iskip rows
    val y  = yy(iskip until yy.dim)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sample ARX_Quad_MV.exo on COVID-19 Weekly Data")
    val mod = ARX_Quad_MV.exo (y, LAGS, ex, h, 0.5)(1, LAGS+1)              // create model for time series data with exo
    val (yp, qof) = mod.trainNtest ()()                                     // train on full and test on full
    val yy_ = y(LAGS until y.dim)
    new Plot (null, yy_, yp(?, 0), s"${mod.modelName}, yy vs. yp", lines = true)

//  val tech = SelectionTech.Forward                                        // pick one feature selection technique
//  val tech = SelectionTech.Backward
    val tech = SelectionTech.Stepwise

    banner (s"Feature Selection Technique: $tech")
    val (cols, rSq) = mod.selectFeatures (tech, cross = false)              // R^2, R^2 bar, sMAPE, NA
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "NA"),
               s"R^2 vs n for ARX_Quad_MV with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"$tech: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

end aRX_Quad_MVTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_Quad_MVTest5` main function tests the `ARX_Quad_MV` class on real data:
 *  Forecasting COVID-19 Weekly Data.  Uses Quadratic Regression.  Does TnT Testing on endogenous
 *  and exogenous variables.  Determine the terms to include in the model for TnT from using
 *  Stepwise on In-Sample.
 *  > runMain scalation.modeling.forecasting.aRX_Quad_MVTest5
 */
@main def aRX_Quad_MVTest5 (): Unit =

    val LAGS = 10                                                           // number of lags
    val h    = 6                                                            // forecasting horizon

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xx, yy) = forecasting.Example_Covid.loadData (exo_vars, "new_deaths")
    val iskip = yy.indexWhere (_ >= 6.0)                                    // find day with at least 6 deaths
    println (s"iskip = $iskip is first day with at least 6 deaths")

    val ex = xx(iskip until xx.dim)                                         // trim away the first iskip rows
    val y  = yy(iskip until yy.dim)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sample ARX_Quad_MV.exo on COVID-19 Weekly Data")
    val mod = ARX_Quad_MV.exo (y, LAGS, ex, h)(1, LAGS+1)           // create model for time series data with exo
    val (yp, qof) = mod.trainNtest ()()                                     // train on full and test on full
    val yy_ = y(LAGS until y.dim)
    new Plot (null, yy_, yp(?, 0), s"${mod.modelName}, yy vs. yp", lines = true)

//  val tech = SelectionTech.Forward                                        // pick one feature selection technique
//  val tech = SelectionTech.Backward
    val tech = SelectionTech.Stepwise

    banner (s"Feature Selection Technique: $tech")
    val (cols, rSq) = mod.selectFeatures (tech, cross = false)              // R^2, R^2 bar, sMAPE, NA
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "NA"),
               s"R^2 vs n for ARX_Quad_MV with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"$tech: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

    banner ("Run TnT on Best model")
    val bmod = mod.getBest._3                                               // get the best model from feature selection
    val (x_, y_, xtest, ytest) = ARX_MV.split_TnT (bmod.getX, bmod.getYY)
    val (yptest, qoftest) = bmod.asInstanceOf [RegressionMV].trainNtest (x_, y_)(xtest, ytest)          // train on (x_, y_) and test on (xtest, ytest)
    new Plot (null, ytest(?, 0), yptest(?, 0), s"${mod.modelName}, ytest vs. yptest", lines = true)

end aRX_Quad_MVTest5


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_Quad_MVTest6` main function tests the `ARX_Quad_MV` class on real data:
 *  Forecasting COVID-19 Weekly Data.  Uses Quadratic Regression.  Does TnT Testing on endogenous
 *  and exogenous variables.  Determine the terms to include in the model for TnT from using
 *  Stepwise on In-Sample.
 *  > runMain scalation.modeling.forecasting.aRX_Quad_MVTest6
 */
@main def aRX_Quad_MVTest6 (): Unit =

    val LAGS = 10                                                           // number of lags
    val rc   = 1                                                            // retraining cycle
    val h    = 6                                                            // forecasting horizon

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xx, yy) = forecasting.Example_Covid.loadData (exo_vars, "new_deaths")
    val iskip = yy.indexWhere (_ >= 6.0)                                    // find day with at least 6 deaths
    println (s"iskip = $iskip is first day with at least 6 deaths")

    val ex = xx(iskip until xx.dim)                                         // trim away the first iskip rows
    val y  = yy(iskip until yy.dim)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sample ARX_Quad_MV.exo on COVID-19 Weekly Data")
    val mod = ARX_Quad_MV.exo (y, LAGS, ex, h)(1, LAGS+1)           // create model for time series data with exo
    val (yp, qof) = mod.trainNtest ()()                                     // train on full and test on full
    val yy_ = y(LAGS until y.dim)
    new Plot (null, yy_, yp(?, 0), s"${mod.modelName}, yy vs. yp", lines = true)

//  val tech = SelectionTech.Forward                                        // pick one feature selection technique
//  val tech = SelectionTech.Backward
    val tech = SelectionTech.Stepwise

    banner (s"Feature Selection Technique: $tech")
    val (cols, rSq) = mod.selectFeatures (tech, cross = false)              // R^2, R^2 bar, sMAPE, NA
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "NA"),
               s"R^2 vs n for ARX_Quad_MV with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"$tech: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

    banner ("Run Rolling Validation on ARX_Quad_MV Best model")
    val bmod = mod.getBest._3                                               // get the best model from feature selection
    ARX_MV.rollValidate (bmod.asInstanceOf [RegressionMV], rc)

end aRX_Quad_MVTest6

