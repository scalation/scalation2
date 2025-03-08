
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Feb 13 16:22:21 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: AutoRegressive with eXogenous Variables (Time Series Multi-Variate Regression)
 */

package scalation
package modeling
package forecasting_old

import scala.math.max

import scalation.mathstat._
import scalation.modeling.neuralnet.{PredictorMV, RegressionMV}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ARX_MV` object supports regression for Time Series data.
 *  Multi-horizon forecasting supported via the DIRECT method.
 *  Given a response vector y, a predictor matrix x is built that consists of
 *  lagged y vectors.   Additional future response vectors are built for training.
 *
 *      y_t = b dot x
 *
 *  where x = [y_{t-1}, y_{t-2}, ... y_{t-lags}].
 */
object ARX_MV:

    private val debug = debugf ("ARX_MV", true)                           // debug function

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
        val (x_, yy) = buildMatrix4TS (y, lags, h)                        // column for each lag
        val x = if intercept then VectorD.one (yy.dim) +^: x_ else x_     // add first column of all ones
        debug ("apply", s"x.dims = ${x.dims}, yy.dims = ${yy.dims}")

        val mod = new RegressionMV (x, yy, null, hparam)
        mod.modelName = s"ARX_MV$lags"
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
        val (x_, yy) = buildMatrix4TS (y, lags, h)                        // column for each lag
        var x = scale (extreme (x_), (1.0, 5.0))(x_)                      // rescale vector x matrix to [1, 5]
        if intercept then x = VectorD.one (yy.dim) +^: x                  // add first column of all ones
        debug ("rescale", s"x.dims = ${x.dims}, yy.dims = ${yy.dims}")

        val mod = new RegressionMV (x, yy, null, hparam)
        mod.modelName = s"ARX_MV$lags"
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
        val (x_, yy) = buildMatrix4TS (y, lags, h)                        // column for each lag
        var x = if intercept then VectorD.one (yy.dim) +^: x_ else x_     // add first column of all ones
        val endoCols = x.dim2
        println (s"endogenous: columns = $endoCols")

        x = x ++^ ARX.makeExoCols (lags, ex, elag1, elag2)                // add columns for each lagged exo var
        println (s"exogenous: columns = ${x.dim2 - endoCols}")

        println (s"exo: x.dims = ${x.dims}, yy.dim = ${yy.dim}")
//      println (s"exo: x = $x \n yy = $yy")
        val mod = new RegressionMV (x, yy, null, hparam)
        mod.modelName = s"ARX_MV.exo_$lags"
        mod
    end exo

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Split the x matrix and y matrix into training and testing sets.
     *  @param x      the x data/input matrix
     *  @param y      the y response/output matrix
     *  @param ratio  the ratio of the TESTING set to the full dataset (most common 70-30, 80-20)
     */
    def split_TnT (x: MatrixD, y: MatrixD, ratio: Double = 0.20): (MatrixD, MatrixD, MatrixD, MatrixD) =
        val n       = x.dim
        val tr_size = (n * (1.0 - ratio)).toInt
        println (s"ARX_MV.split_TnT: tr_size = $tr_size, te_size = ${n - tr_size}")
        (x(0 until tr_size), y(0 until tr_size), x(tr_size until n), y(tr_size until n))
    end split_TnT 

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Align (for the testing set) the actual response vector for comparison with
     *  the predicted/forecasted response vector, returning a time vector and sliced
     *  response vectors.
     *  @param tr_size  the size of the intial training set
     *  @param y        the actual response for the full dataset (to be sliced)
     *  @param yp       the predicted response for the full dataset (to be sliced)
     *  @param h_       the current forecasting horizon - 1
     */
    def align (tr_size: Int, y: VectorD, yp: VectorD, h_ : Int): (VectorD, VectorD, VectorD) =
        debug ("align:", s"y.dim = ${y.dim}, yp.dim = ${yp.dim}, h_ = $h_")
        (VectorD.range (tr_size, y.dim - h_), y(tr_size until y.dim - h_), yp(0 until yp.dim - h_))
    end align

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use rolling-validation to compute test Quality of Fit (QoF) measures
     *  by dividing the dataset into a TRAINING SET (tr) and a TESTING SET (te)
     *  as follows:  [ <-- tr_size --> | <-- te_size --> ]
     *  This version calls predict (DIRECT) for h-steps ahead out-of-sample forecasts.
     *  @see `RollingValidation`
     *  @param mod       the forecasting model being used (e.g., `ARX_MV`)
     *  @param rc        the retraining cycle (number of forecasts until retraining occurs)
     *  @param te_size_  the given size of the testing set (negative => take ratio)
     */
    def rollValidate (mod: PredictorMV & Fit, rc: Int, te_size_ : Int = -1): MatrixD =
        val x       = mod.getX                                            // get data/input matrix
        val y       = mod.getYY                                           // get response/output matrix
        val h       = y.dim2
        val ftMat   = new MatrixD (h, Fit.N_QoF)
        banner (s"rollValidate: Evaluate ${mod.modelName}'s QoF for the horizons: 1 to $h")

        val te_size = if te_size_ < 0 then RollingValidation.teSize (y.dim) else te_size_  // size of test set
        val tr_size = y.dim - te_size                                                      // size of initial training set
        debug ("rollValidate", s"y.dims = ${y.dims}, train: tr_size = $tr_size; test: te_size = $te_size, rc = $rc")

        val yp = new MatrixD (te_size, y.dim2)                            // y-predicted over testing set
        for i <- 0 until te_size do                                       // iterate through testing set
            val t = tr_size + i                                           // next time point to forecast
//          if i % rc == 0 then mod.train (x(0 until t), y(0 until t))    // retrain on sliding training set (growing set)
            if i % rc == 0 then mod.train (x(i until t), y(i until t))    // retrain on sliding training set (fixed size set)
            yp(i) = mod.predict (x(t-1))                                  // predict the next value
        end for

        val yte0 = y(tr_size until y.dim, 0)
        println (s"y_yp = ${yte0 +^: yp}")

        val df = max0 (mod.parameter.dim - 1)                             // degrees of freedom for model
        mod.resetDF (df, te_size - df)                                    // reset degrees of freedom

        for k <- y.indices2 do                                            // move thru each horizon 1 to h
            val (t, yk, ypk) = align (tr_size, y(?, k), yp(?, k), k)      // clip ending zeros (0.0 or -0.0)
            debug ("rollValidate", s"horizon $k: yk.dim = ${yk.dim}, ypk.dim = ${ypk.dim}")
            new Plot (t, yk, ypk, s"Plot yy, yp vs. t for horizon ${k+1}", lines = true)
            val qof  = mod.diagnose (yk, ypk)
            ftMat(k) = qof
//          println (FitM.fitMap (qof, qoF_names))
        end for
        println ("fitMap     qof = ")
        println (FitM.showFitMap (ftMat.transpose, QoF.values.map (_.toString)))
        yp
    end rollValidate

end ARX_MV


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_MVTest` main function tests the `ARX_MV` class.
 *  This test is used to CHECK that the `buildMatrix4TS` function is working correctly.
 *  May get NaN for some maximum lags (p) due to multi-collinearity.
 *  > runMain scalation.modeling.forecasting.aRX_MVTest
 */
@main def aRX_MVTest (): Unit =

    val m = 30
    val y = VectorD.range (1, m)                                       // used to CHECK the buildMatrix4TS function
    val h = 3                                                          // the forecasting horizon

    for p <- 5 to 5 do                                                 // autoregressive hyper-parameter p
        banner (s"Test: ARX_MV with $p lags")
        val mod = ARX_MV (y, p, h)                                     // create model for time series data
        mod.trainNtest ()()                                            // train the model on full dataset
        println (mod.summary)

        val yy = mod.getYY
        val yp = mod.predict (mod.getX)
        for k <- yp.indices2 do
            new Plot (null, yy(?, k), yp(?, k), s"yy_$k vs. yp_$k for ${mod.modelName} (h=${k+1}) with $p lags", lines = true)
    end for

end aRX_MVTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_MVTest2` main function tests the `ARX_MV` class on real data:
 *  Forecasting lake levels.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.aRX_MVTest2
 */
@main def aRX_MVTest2 (): Unit =

    import forecasting.Example_LakeLevels.y
    val h = 2                                                          // the forecasting horizon

    for p <- 1 to 7 do                                                 // autoregressive hyper-parameter p
        banner (s"Test: ARX_MV with $p lags")
        val mod = ARX_MV (y, p, h)                                     // create model for time series data
        mod.trainNtest ()()                                            // train the model on full dataset
        println (mod.summary)

        banner ("Predictions/Forecasts")                               // direct forecasting technique
        val yy = mod.getYY
        val yf = mod.predict (mod.getX)
        for k <- yf.indices2 do
            new Plot (null, yy(?, k), yf(?, k), s"yy_$k vs. yf$k for ${mod.modelName} (h=${k+1}) with $p lags", lines = true)
        println (s"yf = $yf")
        println (s"yf.dims = ${yf.dims}")
    end for

end aRX_MVTest2

import forecasting.Example_Covid.{loadData, NO_EXO, response}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_MVTest3` main function tests the `ARX_MV` class on real data:
 *  Forecasts COVID-19 Weekly Data using endogenous variable only.
 *  Does In-Sample Testing (In_ST).
 *  Determines the terms to include in the model using Feature Selection.
 *  > runMain scalation.modeling.forecasting.aRX_MVTest3
 */
@main def aRX_MVTest3 (): Unit =

    val LAGS = 10                                                           // number of lags
    val h    = 6                                                            // forecasting horizon

    val (ex, y) = loadData (NO_EXO, response)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sample ARX_MV on COVID-19 Weekly Data")
    val mod = ARX_MV (y, LAGS, h)                                           // create model for time series data
//  val mod = ARX_MV.rescale (y, LAGS, h)                                   // create model for time series data - scaling
    val (yp, qof) = mod.trainNtest ()()                                     // train on full and test on full
    val yy_ = y(LAGS until y.dim)

    for k <- 0 until h do
        new Plot (null, yy_, yp(?, k), s"${mod.modelName}, yy vs. yp @ h = $k", lines = true)

    banner (s"Feature Selection Technique: Stepwise")
    val (cols, rSq) = mod.stepwiseSelAll (cross = false)                    // R^2, R^2 bar, sMAPE, NA
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "sMAPE", "NA"),
               s"R^2 vs n for ARX_MV with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"Stepwise: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

end aRX_MVTest3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_MVTest4` main function tests the `ARX_MV` class on real data:
 *  Forecasts COVID-19 Weekly Data using endogenous and exogenous variables.
 *  Does In-Sample Testing (In-ST).
 *  Determines the terms to include in the model using Feature Selection.
 *  > runMain scalation.modeling.forecasting.aRX_MVTest4
 */
@main def aRX_MVTest4 (): Unit =

    val LAGS = 10                                                           // number of lags
    val h    = 6                                                            // forecasting horizon

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (ex, y)  = loadData (exo_vars, response)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sample ARX_MV.exo on COVID-19 Weekly Data")
    val mod = ARX_MV.exo (y, LAGS, ex, h)(1, LAGS+1)                        // create model for time series data - with exo
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
               s"R^2 vs n for ARX_MV with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"$tech: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

end aRX_MVTest4


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_MVTest5` main function tests the `ARX_MV` class on real data:
 *  Forecasts COVID-19 Weekly Data using endogenous and exogenous variables.
 *  Does In-Sample Testing (In-ST).
 *  Determines the terms to include in the model using Feature Selection.
 *  Run Train-n-Test (TnT) Split testing on best model.
 *  > runMain scalation.modeling.forecasting.aRX_MVTest5
 */
@main def aRX_MVTest5 (): Unit =

    val LAGS = 10                                                           // number of lags
    val h    = 6                                                            // forecasting horizon

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (ex, y)  = loadData (exo_vars, response)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sample ARX_MV.exo on COVID-19 Weekly Data")
    val mod = ARX_MV.exo (y, LAGS, ex, h)(1, LAGS+1)                        // create model for time series data with exo
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
               s"R^2 vs n for ARX_MV with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"$tech: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

    banner ("Run TnT on Best model")
//  val bmod = mod.getBest._3                                               // get the best model from feature selection
    val bmod = mod.getBest.mod.asInstanceOf [RegressionMV]                  // get the best model from feature selection
    val (x_, y_, xtest, ytest) = ARX_MV.split_TnT (bmod.getX, bmod.getYY)
    val (yptest, qoftest) = bmod.asInstanceOf [PredictorMV].trainNtest (x_, y_)(xtest, ytest)          // train on (x_, y_) and test on (xtest, ytest)
    new Plot (null, ytest(?, 0), yptest(?, 0), s"${mod.modelName}, ytest vs. yptest", lines = true)

end aRX_MVTest5


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `aRX_MVTest6` main function tests the `ARX_MV` class on real data:
 *  Forecasts COVID-19 Weekly Data using endogenous and exogenous variables.
 *  Does In-Sample Testing (In-ST).
 *  Determines the terms to include in the model using Feature Selection.
 *  Run Train-n-Test (TnT) Split testing on best model using Rolling Validation.
 *  > runMain scalation.modeling.forecasting.aRX_MVTest6
 */
@main def aRX_MVTest6 (): Unit =

    val LAGS = 10                                                           // number of lags (values from past)
    val rc   = 1                                                            // retraining cycle
    val h    = 6                                                            // forecasting horizon

    val exo_vars = Array ("icu_patients", "hosp_patients", "new_tests", "people_vaccinated")
    val (ex, y)  = loadData (exo_vars, response)
    println (s"ex.dims = ${ex.dims}, y.dim = ${y.dim}")

    banner ("Test In-Sample ARX_MV.exo on COVID-19 Weekly Data")
    val mod = ARX_MV.exo (y, LAGS, ex, h)(1, LAGS+1)                        // create model for time series data with exo
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
               s"R^2 vs n for ARX_MV with tech", lines = true)
    println (mod.summary ())

    banner ("Feature Importance")
    println (s"$tech: rSq = $rSq")
//  val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${header(c)}, \t importance = $r")

    banner ("Run Rolling Validation on ARX_MV Best model")
//  val bmod = mod.getBest._3                                               // get the best model from feature selection
    val bmod = mod.getBest.mod.asInstanceOf [RegressionMV]                  // get the best model from feature selection
    ARX_MV.rollValidate (bmod.asInstanceOf [PredictorMV & Fit], rc)

end aRX_MVTest6

