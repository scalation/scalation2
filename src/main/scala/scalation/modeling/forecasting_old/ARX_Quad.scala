
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue Feb 22 23:14:31 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Quadratic AutoRegressive with eXogenous Variables (Quadratic Time Series Regression)
 */

package scalation
package modeling
package forecasting_old

import scala.math.{max, min}

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_Quad` class supports quadratic regression for Time Series data.
 *  Given a response vector y, a predictor matrix x is built that consists of
 *  lagged y vectors,
 *
 *      y_t = b dot x
 *      where x = [ 1, y_{t-1}, y_{t-2}, ... y_{t-lag}, y_{t-1}^2, ...].
 *
 *  @param x       the input/predictor matrix built out of lags of y
 *  @param yy      the output/response vector trimmed to match x.dim
 *  @param lags    the maximum lag included (inclusive)
 *  @param fname   the feature/variable names
 *  @param hparam  the hyper-parameters (use Regression.hp for default)
 */
class ARX_Quad (x: MatrixD, yy: VectorD, lags: Int, fname: Array [String] = null,
                hparam: HyperParameter = Regression.hp)
      extends Regression (x, yy, fname, hparam)
         with ForecasterX (lags):

    private val debug   = debugf ("ARX_Quad", true)                      // debug function
    private val flaw    = flawf ("ARX_Quad")                             // flaw function

    modelName = s"ARX_Quad$lags"

    debug ("init", s"$modelName: x.dims = ${x.dims}, yy.dim = ${yy.dim}")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the internally row trimed and column expanded input matrix and response vector.
     */
    def getXY: (MatrixD, VectorD) = (x, yy)                              // (getX, getY)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t+1 using the 1-step ahead forecast.
     *      y_t+1 = f (y_t, ...) + e_t+1
     *  @param t   the time point from which to make prediction
     *  @param yx  the matrix of endogenous y and exogenous x values
     */
    def predict (t: Int, yx: MatrixD): Double = ???

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast h steps ahead using the recursive method, returning forecasts in
     *  matrix yf with columns: [1-step, 2-steps, ... h-steps].
     *  @param yp  the predicted response vector (horizon 1 forecasts)
     *  @param h   the forecasting horizon
     *
    def forecast (yp: VectorD, h: Int): MatrixD =
        val yf   = new MatrixD (yp.dim, h)                               // matrix to hold forecasts
        yf(?, 0) = yp                                                    // column 0 is predicted values
        for k <- 1 until h do                                            // forecast into future: columns 1 to h-1
            for i <- yf.indices do
                val xi = x(i)
                val yi = yf(i)
                var sum = b(0)
                var l = 0
                for j <- 1 until b.dim-1 by 2 do                         // add terms in an interleaved fashion
                    if j+k+1 < b.dim then
                        sum += b(j) * xi(j+k)                            // linear terms
                        sum += b(j+1) * xi(j+k+1)                        // add quadratic terms
                    else
                        sum += b(j) * yi(l)
                        sum += b(j+1) * yi(l)~^2
                        l += 1
                    end if
                end for
                yf(i, k) = sum                                           // record forecasted value
            end for
        end for
        yf
    end forecast
     */

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a vector of size h, of 1 through h-steps ahead forecasts for the model.
     *      forecast the following time points:  t+1, ..., t-1+h.
     *  Note, must create the yf matrix before calling the forecast method.
     *  Intended to work with rolling validation (analog of predict method)
     *  Must call `forecastAll` first.
     *  @param t   the time point from which to make forecasts
     *  @param yf  the forecast matrix (time x horizons)
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecast (t: Int, yf: MatrixD, h: Int): VectorD =
        if h < 1 then flaw ("forecast", s"horizon h = $h must be at least 1")
        VectorD (for k <- 1 to h yield yf(t+k, k))                       // get yf diagonal from time t
    end forecast 

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points at horizon h (h-steps ahead).
     *  Assign to FORECAST MATRIX and return h-step ahead forecast.
     *  Note, `predictAll` provides predictions for h = 1.
     *  @see `forecastAll` method in `Forecaster` trait.
     *  @param yf  the forecast matrix for the endogenous variable y (time x horizons)
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points and all horizons (1 through h-steps ahead).
     *  Record these in the yf matrix, where
     *      yf(t, k) = k-steps ahead forecast for y_t
     *  Note, column 0, yf(?, 0), is set to y (the actual time-series values).
     *  Forecast recursively down diagonals in the yf forecast matrix.
     *  The top right and bottom left triangles in yf matrix are not forecastable.
     *  @param y_  the actual values to use in making forecasts
     *  @param yx  the matrix of endogenous y and exogenous x values
     *  @param h   the maximum forecasting horizon, number of steps ahead to produce forecasts
     */
    override def forecastAll (y_ : VectorD, yx: MatrixD, h: Int): MatrixD =
        debug ("forecastAll", s"y_.dim = ${y_.dim}, yx.dims = ${yx.dims}")
        yf = new MatrixD (y_.dim+h, h+2)                                 // forecasts for all time points t & horizons to h
        for t <- y_.indices do yf(t, 0) = y_(t)                          // first column is the actual endogenous y values
        for t <- yf.indices do yf(t, h+1) = t                            // last column is time (logical day)

        for k <- 1 to h do
            if k > 1 then
                val prev = yf(?, k-1)                                    // the previous forecasted vales
                yx.insert (1, lags, prev)                                // insert previous forecasts for endogenous variable
                yx.insert (1+lags, lags+lags, prev~^2)                   // insert previous forecasts^2 for endogenous variable
                                                                         // FIX - must insert at the right position; maybe rescaling
            end if
            forecastAt (yf, yx, k)                                       // forecast k-steps into the future
        end for
        yf                                                               // return matrix of forecasted values
    end forecastAll

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test FORECASTS of a `ARX_Quad` forecasting model y_ = f(x) + e
     *  and return its forecasts and QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train and forecastAll before testF.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the testing/full response/output vector
     *  @param yx  the matrix of endogenous y and exogenous x values
     */
    def testF (h: Int, y_ : VectorD, yx: MatrixD): (VectorD, VectorD, VectorD) =
        val (yy, yfh) = testSetupF (y_, yx, h)                           // get and align actual and forecasted values
        val params = x.dim2
        resetDF (params, yy.dim - params)                                // reset the degrees of freedom
        println (s"testF: yy.dim = ${yy.dim}, yfh.dim = ${yfh.dim}")
//      differ (yy, yfh)                                                 // uncomment for debugging
        (yy, yfh, diagnose (yy, yfh))                                    // return aligned actual, forecasted and QoF vectors
    end testF

end ARX_Quad


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_Quad` companion object provides factory methods.
 */
object ARX_Quad:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARX_Quad` object to fit a quadratic surface from a response vector.
     *  The input/data matrix x is formed from the lagged y vectors as columns in matrix x.
     *  surface to Time Series data.
     *  @param y       the original un-expanded output/response vector
     *  @param lags    the maximum lag included (inclusive)
     *  @param hparam  the hyper-parameters (use Regression.hp for default)
     */
    def apply (y: VectorD, lags: Int, 
               hparam: HyperParameter = Regression.hp): ARX_Quad =
        val (x, yy) = buildMatrix4TS (y, lags)                           // column for each lag
        val xx = new MatrixD (x.dim, 2*x.dim2+1) 
        xx(?, 0) = VectorD.one (yy.dim)                                  // add first column of all ones
        for j <- x.indices2 do                                           // add terms in an interleaved fashion
            xx(?, 2*j+1) = x(?, j)                                       // linear terms
            xx(?, 2*j+2) = x(?, j)~^2                                    // add quadratic terms
        end for

        println (s"apply: xx.dims = ${xx.dims}, yy.dim = ${yy.dim}")
//      println (s"apply: xx = $xx \n yy = $yy")
        new ARX_Quad (xx, yy, lags, null, hparam)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an `ARX_Quad` object to fit a quadratic surface from a response vector.
     *  The input/data matrix x is formed from the lagged y vectors as columns in matrix x.
     *  In addition, lagged exogenous variables are added.
     *  @param y       the original un-expanded output/response vector
     *  @param lags    the maximum lag included (inclusive)
     *  @parax ex      the input matrix for exogenous variables (one per column)
     *  @param hparam  the hyper-parameters (use Regression.hp for default)
     *  @param elag1   the minimum exo lag included (inclusive)
     *  @param elag2   the maximum exo lag included (inclusive)
     */
    def exo (y: VectorD, lags: Int, ex: MatrixD, hparam: HyperParameter = Regression.hp)
            (elag1: Int = max (1, lags / 5),
             elag2: Int = max (1, lags)): ARX_Quad =
        val (x, yy) = buildMatrix4TS (y, lags)                           // column for each lag
        var xx = new MatrixD (x.dim, 2*x.dim2+1) 
        xx(?, 0) = VectorD.one (yy.dim)                                  // add first column of all ones
        for j <- x.indices2 do                                           // add terms in an interleaved fashion
            xx(?, 2*j+1) = x(?, j)                                       // linear terms
            xx(?, 2*j+2) = x(?, j)~^2                                    // add quadratic terms
        end for
        val endoCols = xx.dim2
        println (s"exo: endogenous: columns = $endoCols")

        xx = xx ++^ ARX.makeExoCols (lags, ex, elag1, elag2)             // add columns for each lagged exo var
        println (s"exogenous: columns = ${xx.dim2 - endoCols}")

        println (s"exo: xx.dims = ${xx.dims}, yy.dim = ${yy.dim}")
//      println (s"exo: xx = $xx \n yy = $yy")
        new ARX_Quad (xx, yy, lags, null, hparam)
    end exo

end ARX_Quad


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_QuadTest` main function tests the `ARX_Quad` class.
 *  This test is used to CHECK that the buildMatrix4TS function is working correctly.
 *  May get NaN for some maximum lags (p) due to multi-collinearity.
 *  > runMain scalation.modeling.forecasting.ARX_QuadTest
 */
@main def ARX_QuadTest (): Unit =

    val m = 30
    val y = VectorD.range (1, m)                                         // used to CHECK the buildMatrix4TS function

    for p <- 1 to 10 do                                                  // autoregressive hyper-parameter p
        banner (s"Test: ARX_Quad with $p lags")
        val mod = ARX_Quad (y, p)                                        // create model for time series data
        mod.trainNtest ()()                                              // train the model on full dataset
        println (mod.summary)

        val yp = mod.predict (mod.getX)
        new Plot (null, mod.getY, yp, s"y vs. yp for ${mod.modelName} with $p lags", lines = true)
    end for

end ARX_QuadTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_QuadTest2` main function tests the `ARX_Quad` class on real data:
 *  Forecasting lake levels.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.ARX_QuadTest2
 */
@main def ARX_QuadTest2 (): Unit =

    import forecasting.Example_LakeLevels.y
    val h = 2                                                            // the forecasting horizon

    for p <- 1 to 8 do                                                   // autoregressive hyper-parameter p
        banner (s"Test: ARX_Quad with $p lags")
        val mod = ARX_Quad (y, p)                                        // create model for time series data
        mod.trainNtest ()()                                              // train the model on full dataset
        println (mod.summary)                                            // parameter/coefficient statistics

        banner ("Predictions")
        val yy = mod.getY                                                // trimmed actual response vector
        println (s"y.dim = ${y.dim}, yy.dim = ${yy.dim}")
        println (s"y = $y")
        println (s"yy = $yy")
        val yx = mod.getX
        val yp = mod.predict (yx)                                        // predicted response vector
        new Plot (null, yy, yp, s"y vs. yp for ${mod.modelName} with $p lags", lines = true)
        println (s"yp = $yp")

        banner ("Forecasts")
//      val yf = mod.forecast (yp, h)                                    // forecasted response matrix
        val yf = mod.forecastAll (yy, yx, h)                             // forecasted response matrix
        for k <- yf.indices2 do
            new Plot (null, yy, yf(?, k), s"yy vs. yf_$k for ${mod.modelName} with $p lags", lines = true)

//      mod.testHorizons (h, y, yx)                                      // calls testF for horizons 1 to h
        ForecasterX.evalForecasts (mod, y, yx, h)

//      banner ("Forecast QoF")
//      println (testForecast (mod, y, yf, p))                           // QoF
//      println (Fit.fitMap (mod.testf (k, y)))                          // evaluate k-units ahead forecasts
    end for

end ARX_QuadTest2

/*
        val mod = ARX (y, p)                                             // create model for time series data
        mod.trainNtest ()()                                              // train the model on full dataset
        println (mod.summary)                                            // parameter/coefficient statistics

        banner ("Predictions")
        val yy = mod.getY                                                // trimmed actual response vector
        val yx = mod.getX
        val yp = mod.predict (yx)                                        // predicted response vector
        new Plot (null, yy, yp, s"y vs. yp for ${mod.modelName} with $p lags", lines = true)
        println (s"yp = $yp")

        banner ("Forecasts")
//      val yf = mod.forecast (yp, h)                                    // forecasted response matrix
        val yf = mod.forecastAll (yy, yx, h)                             // forecasted response matrix
        for k <- yf.indices2 do
            new Plot (null, yy, yf(?, k), s"yy vs. yf_$k for ${mod.modelName} with $p lags", lines = true)

//      mod.testHorizons (h, y, yx)                                      // calls testF for horizons 1 to h
        ForecasterX.evalForecasts (mod, y, yx, h)
*/


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_QuadTest3` main function tests the `ARX_Quad` class on real data:
 *  Forecasting COVID-19.  Does In-Sample Testing on Endogenous variable.
 *  > runMain scalation.modeling.forecasting.ARX_QuadTest3
 */
@main def ARX_QuadTest3 (): Unit =

    val LAGS = 10                                                        // number of lags of y
    val h    = 6                                                         // forecasting horizon

    val exo_vars = Array.ofDim [String] (0)                              // no exogenous variable in this case
    val (xx, yy) = forecasting.Example_Covid.loadData (exo_vars, "new_deaths")
    val iskip = yy.indexWhere (_ >= 6.0)                                 // find day with at least 6 deaths
    println (s"iskip = $iskip is first day with at least 6 deaths")

    val ex = xx(iskip until xx.dim)                                      // trim away the first iskip rows
    val y  = yy(iskip until yy.dim)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test ARX_Quad on COVID-19 Weekly Data")
    val mod = ARX_Quad (y, LAGS)                                         // create model for time series data
    val (yp, qof) = mod.trainNtest ()()                                  // train the model on full dataset
    new Plot (null, mod.getY, yp, s"${mod.modelName}, y vs. yp", lines = true)

    banner (s"Multi-horizon forecasting using the recursive method")
    val yx = mod.getX
    val yf = mod.forecastAll (y, yx, h)                                  // forecasted response matrix
    for k <- 0 to h do
        new Plot (null, y, yf(?, k), s"y vs. yf_$k for ${mod.modelName} with $LAGS lags", lines = true)

    for k <- 1 to h do
        val (yy, yfh, qof) = mod.testF (k, y, yx)                        // k-steps ahead forecast and its QoF
        println (s"Evaluate QoF for horizon $k:")
        println (FitM.fitMap (qof, qoF_names))                           // evaluate k-steps ahead forecasts
    end for

    banner (s"Feature Selection Technique: stepwise")
    val (cols, rSq) = mod.stepwiseSelAll (cross = false)                 // R^2, R^2 bar, sMAPE, NA
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "NA"),
               s"R^2 vs n for ARX_Quad with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"Stepwise: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${ox_fname(c)}, \t importance = $r")

end ARX_QuadTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_QuadTest4` main function tests the `ARX_Quad` class on real data:
 *  Forecasting COVID-19 Weekly Data.  Does In-Sample Testing on endogenous and exogenous variables.
 *  Stepsise gives: 0, 19, 40, 37, 60, 17, 15, 53, 5, 20, 50, 49, 48, 47, 18, 9, 6 best R^2-bar
 *  Stepsise gives: 0, 19, 40, 37, 60, 17, 15, 53, 5, 20, 50, 49, 48, 47           best sMAPE
 *  > runMain scalation.modeling.forecasting.ARX_QuadTest4
 */
@main def ARX_QuadTest4 (): Unit =

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xx, yy) = forecasting.Example_Covid.loadData (exo_vars, "new_deaths")
    val iskip = yy.indexWhere (_ >= 6.0)                                 // find day with at least 6 deaths
    println (s"iskip = $iskip is first day with at least 6 deaths")

    val ex = xx(iskip until xx.dim)                                      // trim away the first iskip rows
    val y  = yy(iskip until yy.dim)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sample ARX_Quad.exo on COVID-19 Weekly Data")
    val mod = ARX_Quad.exo (y, 10, ex)(1, 11)                            // create model for time series data with exo
    val (yp, qof) = mod.trainNtest ()()                                  // train on full and test on full
    new Plot (null, mod.getY, yp, s"${mod.modelName}, yy vs. yp", lines = true)

//  val tech = SelectionTech.Forward                                     // pick one feature selection technique
//  val tech = SelectionTech.Backward
    val tech = SelectionTech.Stepwise

    banner (s"Feature Selection Technique: $tech")
    val (cols, rSq) = mod.selectFeatures (tech, cross = false)           // R^2, R^2 bar, sMAPE, NA
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "NA"),
               s"R^2 vs n for ARX_Quad with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"$tech: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${Example_Covid.header(c)}, \t importance = $r")

end ARX_QuadTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_QuadTest5` main function tests the `ARX_Quad` class on real data:
 *  Forecasting COVID-19 Weekly Data.  Does TnT Testing on endogenous and exogenous variables.
 *  Determine the terms to include in the model for TnT from using Stepwise on In-Sample.
 *  Stepsise gives: 0, 19, 40, 37, 60, 17, 15, 53, 5, 20, 50, 49, 48, 47, 18, 9, 6 best R^2-bar
 *  Stepsise gives: 0, 19, 40, 37, 60, 17, 15, 53, 5, 20, 50, 49, 48, 47           best sMAPE
 *  > runMain scalation.modeling.forecasting.ARX_QuadTest5
 */
@main def ARX_QuadTest5 (): Unit =

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xx, yy) = forecasting.Example_Covid.loadData (exo_vars, "new_deaths")
    val iskip = yy.indexWhere (_ >= 6.0)                                 // find day with at least 6 deaths
    println (s"iskip = $iskip is first day with at least 6 deaths")

    val ex = xx(iskip until xx.dim)                                      // trim away the first iskip rows
    val y  = yy(iskip until yy.dim)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sample ARX_Quad.exo on COVID-19 Weekly Data")
    val mod = ARX_Quad.exo (y, 10, ex)(1, 11)                            // create model for time series data with exo
    val (yp, qof) = mod.trainNtest ()()                                  // train on full and test on full
    new Plot (null, mod.getY, yp, s"${mod.modelName}, yy vs. yp", lines = true)

//  val tech = SelectionTech.Forward                                     // pick one feature selection technique
//  val tech = SelectionTech.Backward
    val tech = SelectionTech.Stepwise

    banner (s"Feature Selection Technique: $tech")
    val (cols, rSq) = mod.selectFeatures (tech, cross = false)           // R^2, R^2 bar, sMAPE, NA
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "NA"),
               s"R^2 vs n for ARX_Quad with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"$tech: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${Example_Covid.header(c)}, \t importance = $r")

    banner ("Run TnT on Best model")
//  val bmod = mod.getBest._3                                            // get the best model from feature selection
    val bmod = mod.getBest.mod.asInstanceOf [ARX_Quad]                   // get the best model from feature selection
    val (x_, y_, xtest, ytest) = ForecasterX.split_TnT (bmod.getX, bmod.getY)
    val (yptest, qoftest) = bmod.trainNtest (x_, y_)(xtest, ytest)       // train on (x_, y_) and test on (xtest, ytest)
    new Plot (null, ytest, yptest, s"${mod.modelName}, ytest vs. yptest", lines = true)

end ARX_QuadTest5


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_QuadTest6` main function tests the `ARX_Quad` class on real data:
 *  Forecasting COVID-19 Weekly Data.  Does Rolling Validation on endogenous and exogenous variables.
 *  Determine the terms to include in the model  HOW?
 *  > runMain scalation.modeling.forecasting.ARX_QuadTest6
 */
@main def ARX_QuadTest6 (): Unit =

    val LAGS = 10                                                        // number of lags (values from past) 
    val rc   = 1                                                         // retraining cycle
    val hh   = 6                                                         // forecasting horizon

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (xx, yy) = forecasting.Example_Covid.loadData (exo_vars, "new_deaths")
    val iskip = yy.indexWhere (_ >= 6.0)                                 // find day with at least 6 deaths
    println (s"iskip = $iskip is first day with at least 6 deaths")

    val ex = xx(iskip until xx.dim)                                      // trim away the first iskip rows
    val y  = yy(iskip until yy.dim)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sample ARX_Quad.exo on COVID-19 Weekly Data")
    val mod = ARX_Quad.exo (y, LAGS, ex)(1, LAGS+1)                      // create model for time series data with exo

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

    banner ("Run Rolling Validation on ARX_Quad Best model")
//  val bmod = mod.getBest._3
    val bmod = mod.getBest.mod.asInstanceOf [ARX_Quad]                    // get the best model from feature selection
    ForecasterX.rollValidate (bmod, rc, hh)

end ARX_QuadTest6

/*
Results for ARX_QuadTest4
key:
0      intercept - forced in the model
1..10  new_deaths
11..20 new_deaths^2
21..30 icu_patients
31..40 hosp_patients
41..50 new_tests
51..60 people_vaccinated
--------------------------------------------------------
Stepwise Results - stops after adding 16 terms out of 61
--------------------------------------------------------
1.  0      intercept
2.  19     new-deaths^2      @ lag 9      94.4965,	94.4614,	14.4024
3.  40     new-test          @ lag 10     96.7542,	96.7126,	12.5111
4.  37     hosp_patients     @ lag 7      97.5908,	97.5442,	10.8841,
5.  60     people_vaccinated @ lag 10     97.7216,	97.6624,	10.7154
6.  17     new-deaths^2      @ lag 7      97.8073,	97.7356,	10.5200
7.  15     new-deaths^2      @ lag 5      97.9416,	97.8603,	10.3548
8.  53     people_vaccinated @ lag 3      98.0055,	97.9130,	10.3473
9.  5      new_deaths        @ lag 5      98.0713,	97.9685,	9.87575
10. 20     new-deaths^2      @ lag 10     98.1416,	98.0294,	10.1097
11. 50     new_tests         @ lag 10     98.1658,	98.0419,	9.72346
12. 49     new_tests         @ lag 9      98.2019,	98.0673,	9.44431
13. 48     new_tests         @ lag 8      98.2456,	98.1014,	9.28453  
14. 47     new_tests         @ lag 7      98.2596,	98.1036,	9.24678 **
15. 18     new-deaths^2      @ lag 8      98.2764,	98.1088,	9.30558
16. 9      new_deaths        @ lag 9      98.2891,	98.1096,	9.40272
17. 6      new_deaths        @ lag 6      98.2805,	98.1133,	9.53115
--------------------------------------------------------
Backward Elimination Results - truncated
--------------------------------------------------------
1.  0      intercept
2.  19     new-deaths^2      @ lag 9     94.4965,	94.4614,	14.4024
3.  30     icu-patients      @ lag 10    96.4830,	96.4379,	12.0952
4.  29     icu-patients      @ lag 9     97.4180,	97.3680,	10.8648
5.  39     hosp_patients     @ lag 9     97.5852,	97.5225,	10.6671
6.  8      new-deaths        @ lag 8     97.6205,	97.5427,	10.5974
7.  15     new-deaths^2      @ lag 5     97.7520,	97.6632,	10.5282
8.  7      new_deaths        @ lag 7     97.8385,	97.7383,	10.7373
9.  9      new_deaths        @ lag 9     97.9804,	97.8727,	10.4594
10. 60     people_vaccinated @ lag 10    98.1069,	97.9925,	10.5465
11. 56     people_vaccinated @ lag 6     98.1850,	98.0624,	10.0418
12. 37     hosp_patients     @ lag 7     98.2482,	98.1172,	9.78951
13. 28     icu-patients      @ lag 8     98.2995,	98.1597,	9.66914
14. 26     icu-patients      @ lag 6     98.3131,	98.1619,	9.65001
15. 27     icu-patients      @ lag 7     98.3450,	98.1841,	9.55611
16. 35     hosp_patients     @ lag 5     98.3621,	98.1903,	9.48847 **
17. 24     icu-patients      @ lag 4     98.3989,	98.2185,	9.49409    
--------------------------------------------------------
Forward Selection Results - truncated
--------------------------------------------------------
1.  0      intercept
2.  19     new-deaths^2      @ lag 9      94.4965,      94.4614,        14.4024
3.  40     new-test          @ lag 10     96.7542,      96.7126,        12.5111
4.  37     hosp_patients     @ lag 7      97.5908,      97.5442,        10.8841,
5.  60     people_vaccinated @ lag 10     97.7216,      97.6624,        10.7154
6.  17     new-deaths^2      @ lag 7      97.8073,      97.7356,        10.5200
7.  10     new_deaths        @ lag 10     97.9416,	97.8603,	10.3548
8.  15     new-deaths^2      @ lag 5      98.0055,	97.9130,	10.3473
9.  53     people_vaccinated @ lag 3      98.0713,	97.9685,	9.87575
10. 5      new_deaths        @ lag 5      98.1416,	98.0294,	10.1097
11. 20     new-deaths^2      @ lag 10     98.1658,	98.0419,	9.72346
12. 50     new_tests         @ lag 10     98.2019,	98.0673,	9.44431
13. 45     new_tests         @ lag 5      98.2456,	98.1014,	9.28453
14. 49     new_tests         @ lag 9      98.2596,	98.1036,	9.24678 **
15. 48     new_tests         @ lag 8      98.2764,	98.1088,	9.30558
16. 47     new_tests         @ lag 7      98.2891,	98.1096,	9.40272
17. 18     new-deaths^2      @ lag 8      98.3041,	98.1131,	9.24220
*/

