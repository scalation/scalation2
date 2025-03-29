
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Aug 27 14:03:25 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Vector AutoRegressive (VAR)
 */

package scalation
package modeling
package forecasting_old

import scala.math.max

import scalation.mathstat._
import scalation.modeling.neuralnet.{PredictorMV, RegressionMV}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VAR` object supports regression for Multivariate Time Series data.
 *  Given a response matrix y, a predictor matrix x is built that consists of
 *  lagged y vectors.   Additional future response vectors are built for training.
 *      y_t = b dot x
 *  where x = [y_{t-1}, y_{t-2}, ... y_{t-lag}].
 */
object VAR:

    private val debug = debugf ("VAR", true)                              // debug function

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `RegressionMV` object from a response matrix.  The input/data matrix
     *  x is formed from the lagged y vectors as columns in matrix x.
     *  @param y          the original un-expanded output/response matrix
     *  @param lags       the maximum lag included (inclusive)
     *  @param h          the forecasting horizon (1, 2, ... h)
     *  @param intercept  whether to add a column of all ones to the matrix (intercept)
     *  @param hparam     the hyper-parameters (use Regression.hp for default)
     */
    def apply (y: MatrixD, lags: Int, h: Int, intercept: Boolean = true,
               hparam: HyperParameter = Regression.hp): RegressionMV =

        var x = ARX.makeExoCols (lags, y, 1, lags+1)                      // add columns for each lagged vars
        val yy = y(1 until y.dim)                                         // trim y to match x
        if intercept then x = VectorD.one (yy.dim) +^: x                  // add first column of all ones

        println (s"x.dims = ${x.dims}, yy.dim = ${yy.dim}")
        println (s"x = $x \n yy = $yy")
        val mod = new RegressionMV (x, yy, null, hparam)
        mod.modelName = s"VAR_$lags"
        mod
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Split the x matrix and y matrix into training and testing sets.
     *  @param x      the x data/input matrix
     *  @param y      the y response/output matrix
     *  @param ratio  the ratio of the TESTING set to the full dataset (most common 70-30, 80-20)
     */
    def split_TnT (x: MatrixD, y: MatrixD, ratio: Double = 0.30): (MatrixD, MatrixD, MatrixD, MatrixD) =
        val n       = x.dim
        val tr_size = (n * (1.0 - ratio)).toInt
        println (s"VAR.split_TnT: tr_size = $tr_size, te_size = ${n - tr_size}")
        (x(0 until tr_size), y(0 until tr_size), x(tr_size until n), y(tr_size until n))
    end split_TnT

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use rolling-validation to compute test Quality of Fit (QoF) measures
     *  by dividing the dataset into a TRAINING SET (tr) and a TESTING SET (te)
     *  as follows:  [ <-- tr_size --> | <-- te_size --> ]
     *  This version calls predict for one-step ahead out-of-sample forecasts.
     *  @see `RollingValidation`
     *  @param mod  the forecasting model being used (e.g., `VAR`)
     *  @param rc   the retraining cycle (number of forecasts until retraining occurs)
     */
    def rollValidate (mod: PredictorMV & Fit, rc: Int): Unit =
        val x       = mod.getX                                            // get data/input matrix
        val y       = mod.getYY                                           // get response/output matrix
        val te_size = RollingValidation.teSize (y.dim)                    // size of testing set
        val tr_size = y.dim - te_size                                     // size of initial training set
        debug ("rollValidate", s"train: tr_size = $tr_size; test: te_size = $te_size, rc = $rc")

        val yp = new MatrixD (te_size, y.dim2)                            // y-predicted over testing set
        for i <- 0 until te_size do                                       // iterate through testing set
            val t = tr_size + i                                           // next time point to forecast
//          if i % rc == 0 then mod.train (x(0 until t), y(0 until t))    // retrain on sliding training set (growing set)
            if i % rc == 0 then mod.train (x(i until t), y(i until t))    // retrain on sliding training set (fixed size set)
            yp(i) = mod.predict (x(t-1))                                  // predict the next value
        end for

        val df = max (0, mod.parameter(0).dim - 1)                        // degrees of freedom for model
        mod.resetDF (df, te_size - df)                                    // reset degrees of freedom
        for k <- y.indices2 do
            val (t, yk) = RollingValidation.align (tr_size, y(?, k))      // align vectors
            val ypk = yp(?, k)
            banner (s"QoF for horizon ${k+1} with yk.dim = ${yk.dim}, ypk.dim = ${ypk.dim}")
            new Plot (t, yk, ypk, s"Plot yy, yp vs. t for horizon ${k+1}", lines = true)
            println (FitM.fitMap (mod.diagnose (yk, ypk), qoF_names))
        end for
    end rollValidate

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Plot actual vs. predicted values for all variables (columns of the matrices).
     *  @param y     the original un-expanded output/response matrix
     *  @param yp    the predicted values (one-step ahead forecasts) matrix
     *  @param name  the name of the model run to produce yp
     */
    def plotAll (y: MatrixD, yp: MatrixD, name: String): Unit =
        for j <- y.indices2  do
            new Plot (null, y(?, j), yp(?, j), s"$name, y vs. yp @ var j = $j", lines = true)
    end plotAll

end VAR


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `varTest` main function tests the `VAR` class.
 *  This test is used to CHECK that the makeExoCols method (@see `apply`) is working correctly.
 *  May get NaN for some maximum lags (p) due to multi-collinearity.
 *  > runMain scalation.modeling.forecasting.varTest
 */
@main def varTest (): Unit =

    val m = 30
    val z = VectorD.range (1, m)                                       // used to CHECK the makeExoCols method
    val y = MatrixD (z, -z + m)
    val h = 3                                                          // the forecasting horizon

    for p <- 5 to 5 do                                                 // autoregressive hyper-parameter p
        banner (s"Test: VAR with $p lags")
        val mod = VAR (y, p, h)                                        // create model for time series data
        mod.trainNtest ()()                                            // train the model on full dataset
        println (mod.summary)

        val yy = mod.getYY
        val yp = mod.predict (mod.getX)
        VAR.plotAll (yy, yp, mod.modelName)
//      for k <- yp.indices2 do
//          new Plot (null, yy(?, k), yp(?, k), s"yy_$k vs. yp_$k for ${mod.modelName} (h=${k+1}) with $p lags", lines = true)
    end for

end varTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `varTest2` main function tests the `VAR` class on real data:
 *  Forecasting Gas Furnace Data.  Performs In-Sample Testing.
 *  > runMain scalation.modeling.forecasting.varTest2
 */
@main def varTest2 (): Unit =

    import forecasting.Example_GasFurnace._

    val LAGS = 5                                                            // number of lags
    val h    = 4                                                            // forecasting horizon

    val y = forecasting.Example_GasFurnace.loadData_yy (header)
    println (s"y.dims = ${y.dims}")

    banner ("Test In-Sample VAR on GasFurnace Data")
    val mod = VAR (y, LAGS, h)                                              // create model for time series data
    val (yp, qof) = mod.trainNtest ()()                                     // train on full and test on full
    println (mod.summary)
    val yy_ = y(1 until y.dim)                                              // can't forecast first values at t = 0
    VAR.plotAll (yy_, yp, mod.modelName)

end varTest2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `varTest3` main function tests the `VAR` class on real data:
 *  Forecasting COVID-19 Weekly Data.  Performs In-Sample Testing.
 *  Goal:  Find the variable that works best with "new_deaths"
 *  > runMain scalation.modeling.forecasting.varTest3
 */
@main def varTest3 (): Unit =

    val LAGS = 5                                                            // number of lags
    val h    = 6                                                            // forecasting horizon

    val vars = Array ("new_deaths", "icu_patients")
    val yy = forecasting.Example_Covid.loadData_yy (vars)
    val iskip = yy(?, 0).indexWhere (_ >= 6.0)                              // find day with at least 6 deaths
    println (s"iskip = $iskip is first day with at least 6 deaths")
    val y = yy(iskip until yy.dim)                                          // trim away the first iskip rows
    println (s"y.dims = ${y.dims}")

    banner ("Test In-Sample VAR on COVID-19 Weekly Data")
    val mod = VAR (y, LAGS, h)                                              // create model for time series data
    val (yp, qof) = mod.trainNtest ()()                                     // train on full and test on full
    println (mod.summary ())
    val yy_ = y(1 until y.dim)                                              // can't forecast first values at t = 0
    VAR.plotAll (yy_, yp, mod.modelName)

    banner (s"Feature Selection Technique: Stepwise")
    val (cols, rSq) = mod.stepwiseSelAll (cross = false)                    // R^2, R^2 bar, sMAPE, NA
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "NA"),
               s"R^2 vs n for VAR with tech", lines = true)

    banner ("Feature Importance")
    println (s"Stepwise: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

end varTest3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `varTest4` main function tests the `VAR` class on real data:
 *  Forecasting COVID-19 Weekly Data.  Performs In-Sample Testing.
 *  Goal:  Find the four variables that works best with "new_deaths"
 *  > runMain scalation.modeling.forecasting.varTest4
 */
@main def varTest4 (): Unit =

    val LAGS = 5                                                            // number of lags
    val h    = 6                                                            // forecasting horizon

    val vars = Array ("new_deaths", "icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val yy = forecasting.Example_Covid.loadData_yy (vars)
    val iskip = yy(?, 0).indexWhere (_ >= 6.0)                              // find day with at least 6 deaths
    println (s"iskip = $iskip is first day with at least 6 deaths")
    val y = yy(iskip until yy.dim)                                          // trim away the first iskip rows
    println (s"y.dims = ${y.dims}")

    banner ("Test In-Sample VAR on COVID-19 Weekly Data")
    val mod = VAR (y, LAGS, h)                                              // create model for time series data - with exo
    val (yp, qof) = mod.trainNtest ()()                                     // train on full and test on full
    println (mod.summary ())
    val yy_ = y(1 until y.dim)                                              // can't forecast first values at t = 0
    VAR.plotAll (yy_, yp, mod.modelName)

//  val tech = SelectionTech.Forward                                        // pick one feature selection technique
//  val tech = SelectionTech.Backward
    val tech = SelectionTech.Stepwise

    banner (s"Feature Selection Technique: $tech")
    val (cols, rSq) = mod.selectFeatures (tech, cross = false)              // R^2, R^2 bar, sMAPE, NA
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "NA"),
               s"R^2 vs n for VAR with tech", lines = true)

    banner ("Feature Importance")
    println (s"$tech: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

end varTest4


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `varTest5` main function tests the `VAR` class on real data:
 *  Forecasting COVID-19 Weekly Data. Does TnT Testing on endogenous and exogenous variables.
 *  Determine the terms to include in the model using Stepwise on In-Sample.
 *  > runMain scalation.modeling.forecasting.varTest5
 */
@main def varTest5 (): Unit =

    val LAGS = 5                                                            // number of lags
    val h    = 6                                                            // forecasting horizon

    val vars = Array ("new_deaths", "icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val yy = forecasting.Example_Covid.loadData_yy (vars)
    val iskip = yy(?, 0).indexWhere (_ >= 6.0)                              // find day with at least 6 deaths
    println (s"iskip = $iskip is first day with at least 6 deaths")
    val y = yy(iskip until yy.dim)                                          // trim away the first iskip rows
    println (s"y.dims = ${y.dims}")

    banner ("Test In-Sample VAR on COVID-19 Weekly Data")
    val mod = VAR (y, LAGS, h)                                              // create model for time series data - with exo
    val (yp, qof) = mod.trainNtest ()()                                     // train on full and test on full
    println (mod.summary ())
    val yy_ = y(1 until y.dim)                                              // can't forecast first values at t = 0
    VAR.plotAll (yy_, yp, mod.modelName)

//  val tech = SelectionTech.Forward                                        // pick one feature selection technique
//  val tech = SelectionTech.Backward
    val tech = SelectionTech.Stepwise

    banner (s"Feature Selection Technique: $tech")
    val (cols, rSq) = mod.selectFeatures (tech, cross = false)              // R^2, R^2 bar, sMAPE, NA
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "NA"),
               s"R^2 vs n for VAR with tech", lines = true)

    banner ("Feature Importance")
    println (s"$tech: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

    banner ("Run TnT on Best model")
    val bmod = mod.getBest._3                                               // get the best model from feature selection
    val (x_, y_, xtest, ytest) = VAR.split_TnT (bmod.getX, bmod.getYY)
    val (yptest, qoftest) = bmod.asInstanceOf [RegressionMV].trainNtest (x_, y_)(xtest, ytest)          // train on (x_, y_) and test on (xtest, ytest)
    new Plot (null, ytest(?, 0), yptest(?, 0), s"${mod.modelName}, ytest vs. yptest", lines = true)

end varTest5


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `varTest6` main function tests the `VAR` class on real data:
 *  Forecasting COVID-19 Weekly Data.  Does Rolling Validation on variables.
 *  Determine the terms to include in the model using Stepwise on In-Sample.
 *  > runMain scalation.modeling.forecasting.varTest6
 */
@main def varTest6 (): Unit =

    val LAGS = 5                                                            // number of lags
    val h    = 6                                                            // forecasting horizon

    val vars = Array ("new_deaths", "icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val yy = forecasting.Example_Covid.loadData_yy (vars)
    val iskip = yy(?, 0).indexWhere (_ >= 6.0)                              // find day with at least 6 deaths
    println (s"iskip = $iskip is first day with at least 6 deaths")
    val y = yy(iskip until yy.dim)                                          // trim away the first iskip rows
    println (s"y.dims = ${y.dims}")

    banner ("Test In-Sample VAR on COVID-19 Weekly Data")
    val mod = VAR (y, LAGS, h)                                              // create model for time series data - with exo
    val (yp, qof) = mod.trainNtest ()()                                     // train on full and test on full
    println (mod.summary ())
    val yy_ = y(1 until y.dim)                                              // can't forecast first values at t = 0
    VAR.plotAll (yy_, yp, mod.modelName)

//  val tech = SelectionTech.Forward                                        // pick one feature selection technique
//  val tech = SelectionTech.Backward
    val tech = SelectionTech.Stepwise

    banner (s"Feature Selection Technique: $tech")
    val (cols, rSq) = mod.selectFeatures (tech, cross = false)              // R^2, R^2 bar, sMAPE, NA
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "NA"),
               s"R^2 vs n for VAR with tech", lines = true)

    banner ("Feature Importance")
    println (s"$tech: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

    banner ("Run Rolling Validation on VAR Best model")
    val bmod = mod.getBest._3                                               // get the best model from feature selection
    VAR.rollValidate (bmod.asInstanceOf [RegressionMV], 1)

end varTest6

