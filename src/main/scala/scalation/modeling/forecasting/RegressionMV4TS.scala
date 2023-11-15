
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Feb 13 16:22:21 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: Multi-Variate Regression for Time Series
 */

package scalation
package modeling
package forecasting

import scala.math.max

import scalation.mathstat._
import scalation.modeling.neuralnet.{PredictorMV, RegressionMV}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionMV4TS` object supports regression for Time Series data.
 *  Given a response vector y, a predictor matrix x is built that consists of
 *  lagged y vectors.   Additional future response vectors are built for training.
 *      y_t = b dot x
 *  where x = [y_{t-1}, y_{t-2}, ... y_{t-lags}].
 */
object RegressionMV4TS:

    private val debug   = debugf ("RegressionMV4TS", true)                // debug function
    private val flaw    = flawf ("RegressionMV4TS")                       // flaw function
    private val MISSING = -0.0                                            // missing value

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `RegressionMV` object from a response vector.  The input/data matrix
     *  x is formed from the lagged y vectors as columns in matrix x.
     *  @param y          the original un-expanded output/response vector
     *  @param lags       the maximum lag included (inclusive)
     *  @param h          the forecasting horizon (1, 2, ... h)
     *  @param intercept  whether to add a column of all ones to the matrix (intercept)
     *  @param hparam     the hyper-parameters (use Regression.hp for default)
     */
    def apply (y: VectorD, lags: Int, h: Int, intercept: Boolean = true,
               hparam: HyperParameter = Regression.hp): RegressionMV =
        var (x, yy) = buildMatrix4TS (y, lags, h)                         // column for each lag
        if intercept then x = VectorD.one (yy.dim) +^: x                  // add first column of all ones

        val mod = new RegressionMV (x, yy, null, hparam)
        mod.modelName = s"RegressionMV4TS_$lags"
        mod
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `RegressionMV` object from a response matrix.  The input/data matrix
     *  x is formed from the lagged y vectors as columns in matrix x.
     *  This method provides data rescaling.
     *  @param y          the original un-expanded output/response vector
     *  @param lags       the maximum lag included (inclusive)
     *  @param h          the forecasting horizon (1, 2, ... h)
     *  @param intercept  whether to add a column of all ones to the matrix (intercept)
     *  @param hparam     the hyper-parameters (use Regression.hp for default)
     */
    def rescale (y: VectorD, lags: Int, h: Int, intercept: Boolean = true,
                 hparam: HyperParameter = Regression.hp): RegressionMV =
        var (x, yy) = buildMatrix4TS (y, lags, h)                         // column for each lag
        x = scale (extreme (x), (1.0, 5.0))(x)                            // rescale vector x matrix to [1, 5]
        if intercept then x = VectorD.one (yy.dim) +^: x                  // add first column of all ones

        val mod = new RegressionMV (x, yy, null, hparam)
        mod.modelName = s"RegressionMV4TS_$lags"
        mod
    end rescale

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `RegressionMV` object from a response vector.  The input/data matrix
     *  x is formed from the lagged y vectors as columns in matrix x.
     *  In addition, lagged exogenous variables are added.
     *  @param y          the original un-expanded output/response vector
     *  @param lags       the maximum lag included (inclusive)
     *  @parax ex         the input matrix for exogenous variables (one per column)
     *  @param h          the forecasting horizon (1, 2, ... h)
     *  @param intercept  whether to add a column of all ones to the matrix (intercept)
     *  @param hparam     the hyper-parameters (use Regression.hp for default)
     *  @param elag1      the minimum exo lag included (inclusive)
     *  @param elag2      the maximum exo lag included (inclusive)
     */
    def exo (y: VectorD, lags: Int, ex: MatrixD, h: Int,
             intercept: Boolean = true, hparam: HyperParameter = Regression.hp)
            (elag1: Int = max (1, lags / 5),
             elag2: Int = max (1, lags)): RegressionMV =
        var (x, yy) = buildMatrix4TS (y, lags, h)                         // column for each lag
        if intercept then x = VectorD.one (yy.dim) +^: x                  // add first column of all ones
        val endoCols = x.dim2
        println (s"endogenous: columns = $endoCols")

        x = x ++^ ARX.makeExoCols (lags, ex, elag1, elag2)                // add columns for each lagged exo var
        println (s"exogenous: columns = ${x.dim2 - endoCols}")

        println (s"exo: x.dims = ${x.dims}, yy.dim = ${yy.dim}")
//      println (s"exo: x = $x \n yy = $yy")
        val mod = new RegressionMV (x, yy, null, hparam)
        mod.modelName = s"RegressionMV4TS.exo_$lags"
        mod
    end exo

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Split the x matrix and y matrix into training and testing sets.
     *  @param x      the x data/input matrix
     *  @param y      the y response/output matrix
     *  @param ratio  the ratio of the TESTING set to the full dataset (most common 70-30, 80-20)
     */
    def split_TnT (x: MatrixD, y: MatrixD, ratio: Double = 0.30): (MatrixD, MatrixD, MatrixD, MatrixD) =
        val n       = x.dim
        val tr_size = (n * (1.0 - ratio)).toInt
        println (s"RegressionMV4TS.split_TnT: tr_size = $tr_size, te_size = ${n - tr_size}")
        (x(0 until tr_size), y(0 until tr_size), x(tr_size until n), y(tr_size until n))
    end split_TnT

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use rolling-validation to compute test Quality of Fit (QoF) measures
     *  by dividing the dataset into a TESTING SET (tr) and a TRAINING SET (te)
     *  as follows:  [ <-- tr_size --> | <-- te_size --> ]
     *  This version calls predict for one-step ahead out-of-sample forecasts.
     *  @see `RollingValidation`
     *  @param mod  the forecasting model being used (e.g., `RegressionMV4TS`)
     *  @param rc   the retraining cycle (number of forecasts until retraining occurs)
     */
    def rollValidate (mod: PredictorMV & Fit, rc: Int): Unit =
        val x       = mod.getX                                            // get data/input matrix
        val y       = mod.getY                                            // get response/output vector
        val tr_size = RollingValidation.trSize (y.dim)                    // size of initial training set
        val te_size = y.dim - tr_size                                     // size of testing set
        debug ("rollValidate", s"train: tr_size = $tr_size; test: te_size = $te_size, rc = $rc")

        val yp = new MatrixD (te_size, y.dim2)                            // y-predicted over testing set
        for i <- 0 until te_size do                                       // iterate through testing set
            val t = tr_size + i                                           // next time point to forecast
            if i % rc == 0 then mod.train (x(0 until t), y(0 until t))    // retrain on sliding training set
            yp(i) = mod.predict (x(t-1))                                  // predict the next value
        end for

        val df = max (0, mod.parameter(0).dim - 1)                        // degrees of freedom for model
        mod.resetDF (df, te_size - df)                                    // reset degrees of freedom
        for k <- y.indices2 do
            val (t, yk) = RollingValidation.align (tr_size, y(?, k))      // align vectors
            val ypk = yp(?, k)
            banner (s"QoF for horizon ${k+1} with yk.dim = ${yk.dim}, ypk.dim = ${ypk.dim}")
            new Plot (t, yk, ypk, s"Plot yy, yp vs. t for horizon ${k+1}", lines = true)
            println (FitM.fitMap (mod.diagnose (yk, ypk), QoF.values.map (_.toString)))
        end for
    end rollValidate

end RegressionMV4TS


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `regressionMV4TSTest` main function tests the `RegressionMV4TS` class.
 *  This test is used to CHECK that the buildMatrix4TS function is working correctly.
 *  May get NaN for some maximum lags (p) due to multi-collinearity.
 *  > runMain scalation.modeling.forecasting.regressionMV4TSTest
 */
@main def regressionMV4TSTest (): Unit =

    val m = 30
    val y = VectorD.range (1, m)                                       // used to CHECK the buildMatrix4TS function
    val h = 3                                                          // the forecasting horizon

    for p <- 5 to 5 do                                                 // autoregressive hyper-parameter p
        banner (s"Test: RegressionMV4TS with $p lags")
        val mod = RegressionMV4TS (y, p, h)                            // create model for time series data
        mod.trainNtest ()()                                            // train the model on full dataset
        println (mod.summary)

        val yy = mod.getY
        val yp = mod.predict (mod.getX)
        for k <- yp.indices2 do
            new Plot (null, yy(?, k), yp(?, k), s"yy_$k vs. yp_$k for ${mod.modelName} (h=${k+1}) with $p lags", lines = true)
        end for
    end for

end regressionMV4TSTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `regressionMV4TSTest2` main function tests the `RegressionMV4TS` class on real data:
 *  Forecasting lake levels.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.regressionMV4TSTest2
 */
@main def regressionMV4TSTest2 (): Unit =

    import Example_LakeLevels.y
    val m = y.dim
    val h = 2                                                          // the forecasting horizon

    for p <- 1 to 7 do                                                 // autoregressive hyper-parameter p
        banner (s"Test: RegressionMV4TS with $p lags")
        val mod = RegressionMV4TS (y, p, h)                            // create model for time series data
        mod.trainNtest ()()                                            // train the model on full dataset
        println (mod.summary)

        banner ("Predictions/Forecasts")                               // direct forecasting technique
        val yy = mod.getY
        val yf = mod.predict (mod.getX)
        for k <- yf.indices2 do
            new Plot (null, yy(?, k), yf(?, k), s"yy_$k vs. yf_$k for ${mod.modelName} (h=${k+1}) with $p lags", lines = true)
        end for
        println (s"yf = $yf")
        println (s"yf.dims = ${yf.dims}")
    end for

end regressionMV4TSTest2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `regressionMV4TSTest3` main function tests the `RegressionMV4TS` class on real data:
 *  Forecasting COVID-19 Weekly Data.  Performs In-Sample Testing using endogenous variable.
 *  > runMain scalation.modeling.forecasting.regressionMV4TSTest3
 */
@main def regressionMV4TSTest3 (): Unit =

    val LAGS = 10                                                           // number of lags
    val h    = 4                                                            // forecasting horizon

    val exo_vars = Array.ofDim [String] (0)                                 // no exogenous variables in this case
    val (xx, yy) = Example_Covid.loadData (exo_vars, "new_deaths")
    val iskip = yy.indexWhere (_ >= 6.0)                                    // find day with at least 6 deaths
    println (s"iskip = $iskip is first day with at least 6 deaths")

    val ex = xx(iskip until xx.dim)                                         // trim away the first iskip rows
    val y  = yy(iskip until yy.dim)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sample RegressionMV4TS on COVID-19 Weekly Data")
    val mod = RegressionMV4TS (y, LAGS, h)                                  // create model for time series data
//  val mod = RegressionMV4TS.rescale (y, LAGS, h)                          // create model for time series data - scaling
    val (yp, qof) = mod.trainNtest ()()                                     // train on full and test on full
    val yy_ = y(LAGS until y.dim)

    for k <- 0 until h do
        new Plot (null, yy_, yp(?, k), s"${mod.modelName}, yy vs. yp @ h = $k", lines = true)
    end for

    banner (s"Feature Selection Technique: Stepwise")
    val (cols, rSq) = mod.stepRegressionAll (cross = false)                 // R^2, R^2 bar, sMAPE, NA
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "NA"),
               s"R^2 vs n for RegressionMV4TS with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"Stepwise: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

end regressionMV4TSTest3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `regressionMV4TSTest4` main function tests the `RegressionMV4TS` class on real data:
 *  Forecasting COVID-19 Weekly Data.
 *  > runMain scalation.modeling.forecasting.regressionMV4TSTest4
 */
@main def regressionMV4TSTest4 (): Unit =

    val LAGS = 10                                                           // number of lags
    val h    = 4                                                            // forecasting horizon

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xx, yy) = Example_Covid.loadData (exo_vars, "new_deaths")
    val iskip = yy.indexWhere (_ >= 6.0)                                    // find day with at least 6 deaths
    println (s"iskip = $iskip is first day with at least 6 deaths")

    val ex = xx(iskip until xx.dim)                                         // trim away the first iskip rows
    val y  = yy(iskip until yy.dim)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sampl RegressionMV4TS.exo on COVID-19 Weekly Data")
    val mod = RegressionMV4TS.exo (y, LAGS, ex, h)(1, LAGS+1)               // create model for time series data - with exo
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
               s"R^2 vs n for RegressionMV4TS with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"$tech: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

end regressionMV4TSTest4


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `regressionMV4TSTest5` main function tests the `RegressionMV4TS` class on real data:
 *  Forecasting COVID-19 Weekly Data. Does TnT Testing on endogenous and exogenous variables.
 *  Determine the terms to include in the model using Stepwise on In-Sample.
 *  > runMain scalation.modeling.forecasting.regressionMV4TSTest5
 */
@main def regressionMV4TSTest5 (): Unit =

    val LAGS = 10                                                           // number of lags
    val h    = 4                                                            // forecasting horizon

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xx, yy) = Example_Covid.loadData (exo_vars, "new_deaths")
    val iskip = yy.indexWhere (_ >= 6.0)                                    // find day with at least 6 deaths
    println (s"iskip = $iskip is first day with at least 6 deaths")

    val ex = xx(iskip until xx.dim)                                         // trim away the first iskip rows
    val y  = yy(iskip until yy.dim)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sample RegressionMV4TS.exo on COVID-19 Weekly Data")
    val mod = RegressionMV4TS.exo (y, LAGS, ex, h)(1, LAGS+1)               // create model for time series data with exo
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
               s"R^2 vs n for RegressionMV4TS with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"$tech: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

    banner ("Run TnT on Best model")
    val bmod = mod.getBest._3                                               // get the best model from feature selection
    val (x_, y_, xtest, ytest) = RegressionMV4TS.split_TnT (bmod.getX, bmod.getY)
    val (yptest, qoftest) = bmod.trainNtest (x_, y_)(xtest, ytest)          // train on (x_, y_) and test on (xtest, ytest)
    new Plot (null, ytest(?, 0), yptest(?, 0), s"${mod.modelName}, ytest vs. yptest", lines = true)

end regressionMV4TSTest5


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `regressionMV4TSTest6` main function tests the `RegressionMV4TS` class on real data:
 *  Forecasting COVID-19 Weekly Data.  Does Rolling Validation on endogenous
 *  and exogenous variables.  Determine the terms to include in the model using
 *  Stepwise on In-Sample.
 *  > runMain scalation.modeling.forecasting.regressionMV4TSTest6
 */
@main def regressionMV4TSTest6 (): Unit =

    val LAGS = 7
    val h    = 4

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xx, yy) = Example_Covid.loadData (exo_vars, "new_deaths")
    val iskip = yy.indexWhere (_ >= 6.0)                                    // find day with at least 6 deaths
    println (s"iskip = $iskip is first day with at least 6 deaths")

    val ex = xx(iskip until xx.dim)                                         // trim away the first iskip rows
    val y  = yy(iskip until yy.dim)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sample RegressionMV4TS.exo on COVID-19 Weekly Data")
    val mod = RegressionMV4TS.exo (y, LAGS, ex, h)(1, LAGS+1)               // create model for time series data with exo
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
               s"R^2 vs n for RegressionMV4TS with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"$tech: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

    banner ("Run Rolling Validation on RegressionMV4TS Best model")
    val bmod = mod.getBest._3                                               // get the best model from feature selection
    RegressionMV4TS.rollValidate (bmod, 1)

end regressionMV4TSTest6

