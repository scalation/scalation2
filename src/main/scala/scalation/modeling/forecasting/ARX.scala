
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue Feb 22 23:14:31 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: AutoRegressive with eXogenous Variables (Time Series Regression)
 */

package scalation
package modeling
package forecasting

import scala.math.{max, min}

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX` class supports regression for Time Series data.
 *  Multi-horizon forecasting supported via the RECURSIVE method.
 *  Given a response vector y, a predictor matrix x is built that consists of
 *  lagged y vectors,
 *
 *      y_t = b dot x
 *      where x = [1, y_{t-1}, y_{t-2}, ... y_{t-lags}]
 *
 *  @param x       the input/predictor matrix built out of lags of y
                       (and optionally from exogenous variables ex)
 *  @param yy      the output/response vector trimmed to match x.dim
 *  @param lags    the maximum lag included (inclusive)
 *  @param fname   the feature/variable names
 *  @param hparam  the hyper-parameters (use Regression.hp for default)
 */
class ARX (x: MatrixD, yy: VectorD, lags: Int, fname: Array [String] = null,
           hparam: HyperParameter = Regression.hp)
      extends Regression (x, yy, fname, hparam)
         with ForecasterX (lags):

    private val debug   = debugf ("ARX", true)                           // debug function
    private val flaw    = flawf ("ARX")                                  // flaw function

    modelName = s"ARX_$lags"

    debug ("init", s"$modelName: x.dims = ${x.dims}, yy.dim = ${yy.dim}")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a vector of size h, of 1 through h-steps ahead forecasts for the model.
     *      forecast the following time points:  t+1, ..., t-1+h.
     *  Note, must create the yf matrix before calling the forecast method.
     *  Intended to work with rolling validation (analog of predict method)
     *  Must call `forecastAll` first.
     *  @param t   the time point from which to make forecasts
     *  @param yf  the forecasting matrix (time x horizons)
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecast (t: Int, yf: MatrixD, h: Int): VectorD =
        if h < 1 then flaw ("forecast", s"horizon h = $h must be at least 1")
        VectorD (for k <- 1 to h yield yf(t+k, k))                       // get yf diagonal from time t
    end forecast

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points at horizon h (h-steps ahead).
     *  Assign to forecasting matrix and return h-step ahead forecast.
     *  For 1-step ahead (h = 1),
     *      y_t = δ + φ_0 y_t-1 + φ_1 y_t-2 + ... + φ_p-1 y_t-p
     *  When k < 0 let y_k = y_0 (i.e., assume first value repeats back in time).
     *  @param yf  the forecasting matrix for the endogenous variable y (time x horizons)
     *  @param yx  the matrix of endogenous y and exogenous x values
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecastAt (yf: MatrixD, yx: MatrixD, h: Int): VectorD =
        if h < 1 then flaw ("forecastAt", s"horizon h = $h must be at least 1")
        for t <- yx.indices do                                           // make forecasts over all time points for horizon h
            val t1 = t + h - 1                                           // time point prior to horizon
            yf(t+h, h) = b dot yx(min (t1, yx.dim-1))                    // forecast down the diagonal ??
        end for
        yf(?, h)                                                         // return the h-step ahead forecast vector
    end forecastAt

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test FORECASTS of a `ARX` forecasting model y_ = f(x) + e
     *  and return its forecasts and QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train and forecastAll before testF.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the testing/full response/output vector
     *  @param yx  the matrix of endogenous y and exogenous x values
     */
    def testF (h: Int, y_ : VectorD, yx: MatrixD): (VectorD, VectorD) =
        val (yy, yfh) = testSetupF (y_, yx, h)                           // get and align actual and forecasted values
        val params = x.dim2
        resetDF (params, yy.dim - params)                                // reset the degrees of freedom
        println (s"testF: yy.dim = ${yy.dim}, yfh.dim = ${yfh.dim}")
//      differ (yy, yfh)                                                 // uncomment for debugging
        (yfh, diagnose (yy, yfh))                                        // return predictions and QoF vector
    end testF

end ARX


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX` companion object provides factory methods.
 */
object ARX:

    private val debug = debugf ("ARX", true)                             // debug function

    private val TREND = false                                            // include quadratic trend
    private val DAY   = false                                            // include day of the week effect

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `ARX` object from a response vector.  The input/data matrix
     *  x is formed from the lagged y vectors as columns in matrix x.
     *  @param y       the original un-expanded output/response vector
     *  @param lags    the maximum lag included (inclusive)
     *  @param hparam  the hyper-parameters (use Regression.hp for default)
     */
    def apply (y: VectorD, lags: Int, hparam: HyperParameter = Regression.hp): ARX =
        val (x_, yy) = buildMatrix4TS (y, lags)                          // column for each lag
        var x = VectorD.one (yy.dim) +^: x_                              // add first column of all ones

        if TREND then
            x = VectorD.range (0, yy.dim) +^: x                          // add trend/time
            x = VectorD.range (0, yy.dim)~^2 +^: x                       // add quadratic trend/time
        end if
        if DAY then
            val day = VectorI (for t <- yy.indices yield t % 7)
            x = day.toDouble +^: x                                       // add DAY of week as ordinal var

//          val dum = Variable.dummyVars (day)
//          x = x ++^ dum                                                // add DAY of week as dummy vars
        end if

        println (s"apply: x.dims = ${x.dims}, yy.dim = ${yy.dim}")
//      println (s"apply: x = $x \n yy = $yy")
        new ARX (x, yy, lags, null, hparam)
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `ARX` object from a response vector.  The input/data matrix
     *  x is formed from the lagged y vectors as columns in matrix x.
     *  In addition, lagged exogenous variables are added.
     *  @param y       the original un-expanded output/response vector
     *  @param lags    the maximum lag included (inclusive)
     *  @parax ex      the input matrix for exogenous variables (one per column)
     *  @param hparam  the hyper-parameters (use Regression.hp for default)
     *  @param elag1   the minimum exo lag included (inclusive)
     *  @param elag2   the maximum exo lag included (inclusive)
     */
    def exo (y: VectorD, lags: Int, ex: MatrixD, hparam: HyperParameter = Regression.hp)
            (elag1: Int = max (1, lags / 5), elag2: Int = max (1, lags)): ARX =
        val (x_, yy) = buildMatrix4TS (y, lags)                          // column for each lag
        var x = VectorD.one (yy.dim) +^: x_                              // add first column of all ones
        val endoCols = x.dim2
        println (s"endogenous: columns = $endoCols")

        x = x ++^ makeExoCols (lags, ex, elag1, elag2)                   // add columns for each lagged exo var
        println (s"exogenous: columns = ${x.dim2 - endoCols}")

        if TREND then
            x = VectorD.range (0, yy.dim) +^: x                          // add trend/time
            x = VectorD.range (0, yy.dim)~^2 +^: x                       // add quadratic trend/time
        end if
        if DAY then
            val day = VectorI (for t <- yy.indices yield t % 7)
            val dum = Variable.dummyVars (day)
            x = x ++^ dum                                                // add DAY of week as dummy vars
        end if

        println (s"exo: x.dims = ${x.dims}, yy.dim = ${yy.dim}")
//      println (s"exo: x = $x \n yy = $yy")
        new ARX (x, yy, lags, null, hparam)
    end exo

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make a matrix whose columns are lagged exogenous variables to be added to a data matrix. 
     *  @param lags   the maximum lag included (inclusive) for checking purposes
     *  @param ex     the matrix of data for the exogenous variables
     *  @param elag1  the minimum exo lag included (inclusive)
     *  @param elag2  the maximum exo lag included (inclusive)
     */
    def makeExoCols (lags: Int, ex: MatrixD, elag1: Int, elag2: Int): MatrixD =
        var xx: MatrixD = buildMatrix4TS_exo (ex(?, 0), lags, elag1, elag2)
        for j <- 1 until ex.dim2 do
            xx = xx ++^ buildMatrix4TS_exo (ex(?, j), lags, elag1, elag2)
        end for
        println (s"addExoVars: collects lags of ${ex.dim2} exo variables into #xx.dim2 columns")
        xx
    end makeExoCols

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Split the x matrix and y vector into training and testing sets.
     *  @param x      the x data/input matrix
     *  @param y      the y response/output vector
     *  @param ratio  the ratio of the TESTING set to the full dataset (most common 70-30, 80-20)
     */
    def split_TnT (x: MatrixD, y: VectorD, ratio: Double = 0.30): (MatrixD, VectorD, MatrixD, VectorD) =
        val n       = x.dim
        val tr_size = (n * (1.0 - ratio)).toInt
        println (s"ARX.split_TnT: tr_size = $tr_size, te_size = ${n - tr_size}")
        (x(0 until tr_size), y(0 until tr_size), x(tr_size until n), y(tr_size until n))
    end split_TnT

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use rolling-validation to compute test Quality of Fit (QoF) measures
     *  by dividing the dataset into a TESTING SET (tr) and a TRAINING SET (te)
     *  as follows:  [ <-- tr_size --> | <-- te_size --> ]
     *  This version calls predict for one-step ahead out-of-sample forecasts.
     *  @see `RollingValidation`
     *  @param mod  the forecasting model being used (e.g., `ARX`)
     *  @param rc   the retraining cycle (number of forecasts until retraining occurs)
     */
    def rollValidate (mod: Predictor & Fit, rc: Int): Unit =
        val x       = mod.getX                                           // get data/input matrix
        val y       = mod.getY                                           // get response/output vector
        val tr_size = RollingValidation.trSize (y.dim)                   // size of initial training set
        val te_size = y.dim - tr_size                                    // size of testing set
        debug ("rollValidate", s"train: tr_size = $tr_size; test: te_size = $te_size, rc = $rc")

        val yp = new VectorD (te_size)                                   // y-predicted over testing set
        for i <- 0 until te_size do                                      // iterate through testing set
            val t = tr_size + i                                          // next time point to forecast
            if i % rc == 0 then mod.train (x(0 until t), y(0 until t))   // retrain on sliding training set
            yp(i) = mod.predict (x(t-1))                                 // predict the next value
        end for

        val (t, yy) = RollingValidation.align (tr_size, y)               // align vectors
        val df = max (0, mod.parameter.size - 1)                         // degrees of freedom for model
        mod.resetDF (df, te_size - df)                                   // reset degrees of freedom
        new Plot (t, yy, yp, "Plot yy, yp vs. t", lines = true)
        println (FitM.fitMap (mod.diagnose (yy, yp), QoF.values.map (_.toString)))
    end rollValidate

end ARX


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRXTest` main function tests the `ARX` class.
 *  This test is used to CHECK that the buildMatrix4TS function is working correctly.
 *  May get NaN for some maximum lags (p) due to multi-collinearity.
 *  > runMain scalation.modeling.forecasting.aRXTest
 */
@main def aRXTest (): Unit =

    val m = 30
    val y = VectorD.range (1, m)                                         // used to CHECK the buildMatrix4TS function

    for p <- 1 to 10 do                                                  // autoregressive hyper-parameter p
        banner (s"Test: ARX with $p lags")
        val mod = ARX (y, p)                                             // create model for time series data
        mod.trainNtest ()()                                              // train the model on full dataset
        println (mod.summary)

        val yp = mod.predict (mod.getX)
        new Plot (null, mod.getY, yp, s"y vs. yp for ${mod.modelName} with $p lags", lines = true)
    end for

end aRXTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRXTest2` main function tests the `ARX` class on real data:
 *  Forecasting lake levels.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRXTest2
 */
@main def aRXTest2 (): Unit =

    import Example_LakeLevels.y
    val h = 2                                                            // the forecasting horizon

    for p <- 1 to 8 do                                                   // autoregressive hyper-parameter p
        banner (s"Test: ARX with $p lags")
        val mod = ARX (y, p)                                             // create model for time series data
        mod.trainNtest ()()                                              // train the model on full dataset
        println (mod.summary)                                            // parameter/coefficient statistics

        banner ("Predictions")
        val yy = mod.getY                                                // trimmed actual response vector
        val xx = mod.getX
        val yp = mod.predict (xx)                                        // predicted response vector
        new Plot (null, yy, yp, s"y vs. yp for ${mod.modelName} with $p lags", lines = true)
        println (s"yp = $yp")

        banner ("Forecasts")
//      val yf = mod.forecast (yp, h)                                    // forecasted response matrix
        val yf = mod.forecastAll (yy, xx, h)                             // forecasted response matrix
        for k <- yf.indices2 do
            new Plot (null, yy, yf(?, k), s"yy vs. yf_$k for ${mod.modelName} with $p lags", lines = true)
        end for
        println (s"yf = $yf")
        println (s"yf.dims = ${yf.dims}")
        assert (yf(?, 0) == yp)                                          // first forecast = predicted values
/*
        banner ("Forecast QoF")
        println (testForecast (mod, y, yf, p))                           // QoF
//      println (Fit.fitMap (mod.testf (k, y)))                          // evaluate k-units ahead forecasts
*/
    end for

end aRXTest2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRXTest3` main function tests the `ARX` class on real data:
 *  Forecasting COVID-19 Daily Data.  Does In-Sample Testing on Endogenous variable.
 *  > runMain scalation.modeling.forecasting.aRXTest3
 */
@main def aRXTest3 (): Unit =

    val LAGS = 5                                                         // number of lags of y
    val h    = 2                                                         // forecasting horizon

    val exo_vars = Array.ofDim [String] (0)                              // no exogenous variables in this case
    val (xx, yy) = Example_Covid.loadData (exo_vars, "new_deaths")
    val iskip = yy.indexWhere (_ >= 6.0)                                 // find day with at least 6 deaths
    println (s"iskip = $iskip is first week with at least 6 deaths")

    val ex = xx(iskip until xx.dim)                                      // trim away the first iskip rows
    val y  = yy(iskip until yy.dim)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sample ARX on COVID-19 Weekly Data")
    val mod = ARX (y, LAGS)                                              // create model for time series data
    val (yp, qof) = mod.trainNtest ()()                                  // train the model on full dataset
    new Plot (null, mod.getY, yp, s"${mod.modelName}, y vs. yp", lines = true)

    banner (s"Multi-horizon forecasting using the recursive method")
    val yx = mod.getX
    val yf = mod.forecastAll (y, yx, h)                                  // forecasted response matrix
    for k <- 0 to h do
        new Plot (null, y, yf(?, k), s"y vs. yf_$k for ${mod.modelName} with $LAGS lags", lines = true)
    end for
    println (s"yf = $yf")
    println (s"yf.dims = ${yf.dims}, y.dim = ${y.dim}, yp.dim = ${yp.dim}")
    val yf0 = yf(?, 0)(0 until y.dim)
    val yf1 = yf(?, 1)(1 until y.dim)
    Forecaster.differ (yf0, y)
    Forecaster.differ (yf1, yp)
    assert (yf0 =~ y)                                                    // zeroth forecast = actual values
    assert (yf1 =~ yp)                                                   // first forecast = predicted values

    for k <- 1 to h do
        val (yfh, qof) = mod.testF (k, y, yx)                            // k-steps ahead forecast and its QoF
        println (s"Evaluate QoF for horizon $k:")
        println (FitM.fitMap (qof, QoF.values.map (_.toString)))         // evaluate k-steps ahead forecasts
    end for

    banner (s"Feature Selection Technique: stepRegression")
    val (cols, rSq) = mod.stepRegressionAll (cross = false)              // R^2, R^2 bar, sMAPE, NA
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "NA"),
               s"R^2 vs n for ARX with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"Stepwise: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

end aRXTest3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRXTest4` main function tests the `ARX` class on real data:
 *  Forecasting COVID-19 Weekly Data.  Does In-Sample Testing on endogenous and exogenous variables.
 *  > runMain scalation.modeling.forecasting.aRXTest4
 */
@main def aRXTest4 (): Unit =

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xx, yy) = Example_Covid.loadData (exo_vars, "new_deaths")
    val iskip = yy.indexWhere (_ >= 6.0)                                 // find day with at least 6 deaths
    println (s"iskip = $iskip is first week with at least 6 deaths")

    val ex = xx(iskip until xx.dim)                                      // trim away the first iskip rows
    val y  = yy(iskip until yy.dim)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sample ARX.exo on COVID-19 Weekly Data")
    val mod = ARX.exo (y, 10, ex)(1, 11)                                 // create model for time series data with exo
    val (yp, qof) = mod.trainNtest ()()                                  // train the model on full dataset
    new Plot (null, mod.getY, yp, s"${mod.modelName}, y vs. yp", lines = true)

//  val tech = SelectionTech.Forward                                     // pick one feature selection technique
//  val tech = SelectionTech.Backward
    val tech = SelectionTech.Stepwise

    banner (s"Feature Selection Technique: $tech")
    val (cols, rSq) = mod.selectFeatures (tech, cross = false)           // R^2, R^2 bar, sMAPE, NA
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "NA"),
               s"R^2 vs n for ARX with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"$tech: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

end aRXTest4


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRXTest5` main function tests the `ARX` class on real data:
 *  Forecasting COVID-19 Weekly Data.  Does TnT Testing on endogenous and exogenous variables.
 *  Determine the terms to include in the model using Stepwise on In-Sample.
 *  > runMain scalation.modeling.forecasting.aRXTest5
 */
@main def aRXTest5 (): Unit =

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xx, yy) = Example_Covid.loadData (exo_vars, "new_deaths")
    val iskip = yy.indexWhere (_ >= 6.0)                                 // find day with at least 6 deaths
    println (s"iskip = $iskip is first week with at least 6 deaths")

    val ex = xx(iskip until xx.dim)                                      // trim away the first iskip rows
    val y  = yy(iskip until yy.dim)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sample ARX.exo on COVID-19 Weekly Data")
    val mod = ARX.exo (y, 10, ex)(1, 11)                                 // create model for time series data with exo
    val (yp, qof) = mod.trainNtest ()()                                  // train on full and test on full
    new Plot (null, mod.getY, yp, s"${mod.modelName}, y vs. yp", lines = true)

//  val tech = SelectionTech.Forward                                     // pick one feature selection technique
//  val tech = SelectionTech.Backward
    val tech = SelectionTech.Stepwise

    banner (s"Feature Selection Technique: $tech")
    val (cols, rSq) = mod.selectFeatures (tech, cross = false)           // R^2, R^2 bar, sMAPE, NA
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "NA"),
               s"R^2 vs n for ARX with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"$tech: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

    banner ("Run TnT on Best model")
    val bmod = mod.getBest._3                                            // get the best model from feature selection
    val (x_, y_, xtest, ytest) = ARX.split_TnT (bmod.getX, bmod.getY)
    val (yptest, qoftest) = bmod.trainNtest (x_, y_)(xtest, ytest)       // train on (x_, y_) and test on (xtest, ytest)
    new Plot (null, ytest, yptest, s"${mod.modelName}, ytest vs. yptest", lines = true)

end aRXTest5


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRXTest6` main function tests the `ARX` class on real data:
 *  Forecasting COVID-19 Weekly Data.  Does Rolling Validation on endogenous and exogenous
 *  variables.  Determine the terms to include in the model using Stepwise on In-Sample.
 *  > runMain scalation.modeling.forecasting.aRXTest6
 */
@main def aRXTest6 (): Unit =

    val LAGS = 7

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xx, yy) = Example_Covid.loadData (exo_vars, "new_deaths")
    val iskip = yy.indexWhere (_ >= 6.0)                                 // find day with at least 6 deaths
    println (s"iskip = $iskip is first week with at least 6 deaths")

    val ex = xx(iskip until xx.dim)                                      // trim away the first iskip rows
    val y  = yy(iskip until yy.dim)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sample ARX.exo on COVID-19 Weekly Data")
    val mod = ARX.exo (y, LAGS, ex)(1, LAGS+1)                           // create model for time series data with exo

    val (yp, qof) = mod.trainNtest ()()                                  // train on full and test on full
    new Plot (null, mod.getY, yp, s"${mod.modelName}, y vs. yp", lines = true)

//  val tech = SelectionTech.Forward                                     // pick one feature selection technique
//  val tech = SelectionTech.Backward
    val tech = SelectionTech.Stepwise

    banner (s"Feature Selection Technique: $tech")
    val (cols, rSq) = mod.selectFeatures (tech, cross = false)           // R^2, R^2 bar, sMAPE, NA
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "NA"),
               s"R^2 vs n for ARX with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"$tech: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

    banner ("Run Rolling Validation on ARX Best model")
    val bmod = mod.getBest._3                                            // get the best model from feature selection
    ARX.rollValidate (bmod, 1)

end aRXTest6


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRXTest7` main function tests the `ARX` class on real data:
 *  Forecasting COVID-19 Weekly Data.  Preliminary investigation of Symbolic Regression.
 *  > runMain scalation.modeling.forecasting.aRXTest7
 */
@main def aRXTest7 (): Unit =

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xx, yy) = Example_Covid.loadData (exo_vars, "new_deaths")
    val iskip = yy.indexWhere (_ >= 6.0)                                 // find day with at least 6 deaths
    println (s"iskip = $iskip is first week with at least 6 deaths")

    val ex = xx(iskip until xx.dim)                                      // trim away the first iskip rows
    val y  = yy(iskip until yy.dim)

    banner ("Plot Variables on COVID-19 Weekly Data")

    for lag <- 0 to 4 do
        val xx_ = ex(lag until y.dim)
        val yy_ = y(0 until y.dim - lag)
//      new Plot (xx_, yy_, null, s"deaths vs. exo-vars @ lag = $lag")

        val mod = SymbolicRegression (xx_, yy_, null, collection.mutable.Set (1.0), cross = false)
        mod.trainNtest ()()
        println (mod.summary ())
    end for

end aRXTest7

