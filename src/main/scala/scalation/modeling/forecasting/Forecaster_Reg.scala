
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Jan 17 15:04:21 EST 2025
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Framework: Abstract Class for Forecasters that utilize Regression
 *
 *  @see `scalation.modeling.Regression`
 */

package scalation
package modeling
package forecasting

import scala.collection.mutable.{ArrayBuffer, LinkedHashSet => LSET}
import scala.math.max
import scala.util.control.Breaks.{break, breakable}

import scalation.mathstat._
import scalation.modeling.{Regression => REGRESSION}
//import scalation.modeling.{RidgeRegression => REGRESSION}
//import scalation.modeling.{LassoRegression => REGRESSION}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Forecaster_Reg` abstract class provides base methods for use by extending classes
 *  that utilize regression for time series forecasting.
 *  @param x        the data/input matrix (lagged columns of y and xe) @see `ARX.apply`
 *  @param y        the response/output vector (time series data)
 *  @param hh       the maximum forecasting horizon (h = 1 to hh)
 *  @param fname    the feature/variable names
 *  @param tRng     the time range, if relevant (time index may suffice)
 *  @param hparam   the hyper-parameters (defaults to `MakeMatrix4TS.hp`)
 *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
 */
abstract class Forecaster_Reg (x: MatrixD, y: VectorD, hh: Int, fname: Array [String],
                               tRng: Range = null, hparam: HyperParameter = MakeMatrix4TS.hp,
                               bakcast: Boolean = false)
      extends Forecaster (y, hh, tRng, hparam, bakcast)
         with FeatureSelection:

    private   val debug = debugf ("Forecaster_Reg", false)                // debug function
    private   val flaw  = flawf ("Forecaster_Reg")                        // debug function
    protected val reg   = new REGRESSION (x, y, fname, hparam)            // delegate training to regression
    protected val nneg  = hparam("nneg").toInt == 1                       // 0 => unrestricted, 1 => predictions must be non-negative

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the data/input matrix built from lagged y (optionally xe) values.
     */
    override def getX: MatrixD = x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit an `ARY` model to the times-series data in vector y_.
     *  Estimate the coefficient vector b for a p-th order Auto-Regressive ARY(p) model.
     *  Uses OLS Matrix Fatorization to determine the coefficients, i.e., the b (φ) vector.
     *  @param x_  the data/input matrix (e.g., full x)
     *  @param y_  the training/full response vector (e.g., full y)
     */
    override def train (x_ : MatrixD, y_ : VectorD): Unit =
        debug ("train", s"$modelName, x_.dims = ${x_.dims}, y_.dim = ${y_.dim}")
        reg.train (x_, y_)                                              // train the regression model
        b = reg.parameter                                               // coefficients from regression
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train and test the forecasting model y_ = f(y-past) + e and report its QoF
     *  and plot its predictions.  Return the predictions and QoF.
     *  NOTE: must use `trainNtest_x` when an x matrix is used, such as in `ARY`.
     *  @param x_  the training/full data/input matrix (defaults to full x)
     *  @param y_  the training/full response/output vector (defaults to full y)
     *  @param xx  the testing/full data/input matrix (defaults to full x)
     *  @param yy  the testing/full response/output vector (defaults to full y)
     */
    def trainNtest_x (x_ : MatrixD = x, y_ : VectorD = y)
                     (xx: MatrixD = x, yy: VectorD = y): (VectorD, VectorD) =
        train (x_, y_)                                                  // train the model on training set
        val (yp, qof) = test (xx, yy)                                   // test the model on testing set
        println (report (qof))                                          // report on Quality of Fit (QoF)
        (yp, qof)
    end trainNtest_x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test FORECASTS of a forecasting model y_ = f(lags (y_)) + e
     *  and RETURN (1) aligned actual values, (2) its forecasts and (3) QoF vector.
     *  Testing may be in-sample (on the training set) or out-of-sample (on the testing set)
     *  as determined by the parameters passed in.  Note: must call train and forecastAll
     *  before testF.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the testing/full response/output vector
     */
    override def testF (h: Int, y_ : VectorD): (VectorD, VectorD, VectorD) =
        val h_  = h - 1
        val yy  = y_(h_ until y_.dim)                                   // align the actual values
        val yfh = yf(?, h)(0 until y_.dim-h_)                           // column h of the forecast matrix
        println (s"yy.dim = ${yy.dim}, yfh.dim = ${yfh.dim}")
//      Forecaster.differ (yy, yfh)                                     // uncomment for debugging
        assert (yy.dim == yfh.dim)                                      // make sure the vector sizes agree

        new Plot (null, yy, yfh, s"testF: yy, yfh vs. t for $modelName @h = $h", lines = true)
        mod_resetDF (yy.dim)                                            // reset the degrees of freedom
        (yy, yfh, diagnose (yy, yfh))                                   // return actual, forecasted and QoF vectors
    end testF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t using the 1-step ahead forecast.
     *
     *      y_t = b_0 + b_1 y_t-1 + b_2 y_t-2 + ... + b_p y_t-p = b dot x_t
     *
     *  @see `modeling.rectify` define in `Predictor.scala`
     *  @param t   the time point being predicted
     *  @param y_  the actual values to use in making predictions (ignored)
     */
    override def predict (t: Int, y_ : VectorD): Double =
        val yp = rectify (reg.predict (x(t)), nneg)
        if t < y_.dim then
            debug ("predict", s"@t = $t, b = $b dot x(t) = ${x(t)} = yp = $yp vs. y_ = ${y_(t)}")
        yp
    end predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forge a new vector from the first spec values of x, the last p-h+1 values
     *  of x (past values) and recent values 1 to h-1 from the forecasts.
     *  @param xx  the t-th row of the input matrix (lagged actual values)
     *  @param yy  the t-th row of the forecast matrix (forecasted future values)
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forge (xx: VectorD, yy: VectorD, h: Int): VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a vector of size hh, h = 1 to hh-steps ahead forecasts for the model,
     *  i.e., forecast the following time points:  t+1, ..., t+h.
     *  Intended to work with rolling validation (analog of predict method).
     *  @param t   the time point from which to make forecasts
     *  @param y_  the actual values to use in making predictions
     */
    override def forecast (t: Int, y_ : VectorD = y): VectorD =
        val yh = new VectorD (hh)                                       // hold forecasts for each horizon
        for h <- 1 to hh do
            val xy   = forge (x(t), yf(t), h)                           // pull past and prior forecasted values
            val pred = rectify (reg.predict (xy), nneg)                 // slide in prior forecasted values
//          debug ("forecast", s"h = $h, @t = $t, xy = $xy, yp = $pred, y_ = ${y_(t)}")
            yf(t, h) = pred                                             // record in forecast matrix
            yh(h-1)  = pred                                             // record forecasts for each horizon
        yh                                                              // return forecasts for all horizons
    end forecast

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points at horizon h (h-steps ahead).
     *  Assign into FORECAST MATRIX and return the h-steps ahead forecast.
     *  Note, `predictAll` provides predictions for h = 1.
     *  @see `forecastAll` method in `Forecaster` trait.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the actual values to use in making forecasts
     */
    override def forecastAt (h: Int, y_ : VectorD = y): VectorD =
        if h < 2 then flaw ("forecastAt", s"horizon h = $h must be at least 2")

        for t <- y_.indices do                                          // make forecasts over all time points for horizon h
            val xy   = forge (x(t), yf(t), h)
            val pred = rectify (reg.predict (xy), nneg)
            debug ("forecastAt", s"h = $h, @t = $t, xy = $xy, yp = $pred, y_ = ${y_(t)}")
            yf(t, h) = pred                                             // record in forecast matrix
        yf(?, h)                                                        // return the h-step ahead forecast vector
    end forecastAt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Models need to provide a means for updating the Degrees of Freedom (DF).
     *  @param size  the size of dataset (full, train, or test)
     */
    override def mod_resetDF (size: Int): Unit =
        val dfm = max (1, parameter.size - 1)                           // degrees of freedom for model/dataset
        debug ("mod_resetDF", s"dfm = $dfm, df = ${size-dfm}")
        resetDF (dfm, size - dfm)
    end mod_resetDF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a QoF summary for a model with diagnostics for each predictor 'x_j'
     *  and the overall Quality of Fit (QoF).
     *  @param x_      the testing/full data/input matrix
     *  @param fname_  the array of feature/variable names
     *  @param b_      the parameters/coefficients for the model
     *  @param vifs    the Variance Inflation Factors (VIFs)
     */
    override def summary (x_ : MatrixD = getX, fname_ : Array [String] = fname,
                          b_ : VectorD = b, vifs: VectorD = reg.vif ()): String =
        super.summary (x_, fname_, b_, vifs)                             // summary from `Fit`
    end summary

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform In-Sample Testing (In-ST), i.e. train and test on the full data set.
     *  @param skip    the number of initial time points to skip (due to insufficient past)
     *  @param showYf  whether to show the forecast matrix
     */
    override def inSampleTest (skip: Int = 2, showYf: Boolean = false): Unit =
        banner (s"In-ST Test: $modelName")
        trainNtest_x ()()                                                   // train on full and test on full
        forecastAll ()                                                    // forecast over all horizons
        setSkip (skip)                                                    // diagnose: skip the first 'skip' rows
        diagnoseAll (getY, getYf)                                         // compute metrics for all horizons
//      println (s"Final In-ST Forecast Matrix yf = ${mod.getYf}")
//      println (s"Final In-ST Forecast Matrix yf = ${mod.getYf.shiftDiag}")
    end inSampleTest

//  F E A T U R E   S E L E C T I O N

    private var theBest = BestStep ()()                                      // record the best model from feature selection

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the best-step to default
     */
    def resetBest (): Unit = theBest = BestStep ()()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the best model found from feature selection.
     */
    def getBest: BestStep = theBest

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** When the new best-step is better than theBest, replace theBest.
     *  Note: for QoF where smaller if better, must switch to '<'.
     *  @param best  new best-step found during feature selection
     *  @param qk    index of Quality of Fit (QoF) to use for comparing quality
     *               defaults to smapeIC, could try rSqBar could work better
     */
    private def updateBest (best: BestStep) (using qk: Int): Unit =
        if best.qof != null then
            if theBest.qof == null || (best gt theBest.qof(qk)) then theBest = best
    end updateBest

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to find the most predictive variable to add the
     *  existing model, returning the variable to add and the new model.
     *  May be called repeatedly.
     *  Adapt from regression to time series forecasting.
     *  @see `Fit` for index of QoF measures.
     *  @param cols  the lags/columns currently included in the existing model (currently ignored)
     *  @param qk    index of Quality of Fit (QoF) to use for comparing quality
     */
    def forwardSel (cols: LSET [Int])(using qk: Int): BestStep =
        var best = BestStep ()()                                           // best step so far

        for j <- x.indices2 if ! (cols contains j) do
            val cols_j = cols union LSET (j)                        // try adding variable/column x_j
            val x_cols = x(?, cols_j)                                        // x projected onto cols_j columns
            val mod_j  = reg.buildModel (x_cols)                             // regress with x_j added
            mod_j.train (x_cols, y)                                          // train model
            best = best.better (j, mod_j.test ()._2, mod_j)                  // which is better
        end for

        if best.col == -1 then
            flaw ("forwardSel", "could not find a variable x_j to add: best.col = -1")
        correctQoF (skip, best)
    end forwardSel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Correct the QoF in `best` when the optional skip is applied.
     *  @param skip  the number of time-steps to skip at the beginning
     *  @param best  the best model selected so far
     */
    def correctQoF (skip: Int, best: BestStep): BestStep =
        if skip > 0 && best.mod != null then
            val bmod = best.mod.asInstanceOf [REGRESSION]
            val y    = bmod.getY
            val yp   = bmod.predict (bmod.getX)
            val qof  = bmod.diagnose (y.drop (skip), yp.drop (skip), null)
            BestStep (best.col, qof, bmod)(qof(qk))
        else
            best
    end correctQoF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to find the most predictive variables to have
     *  in the model, returning the variables added and the new Quality of Fit (QoF)
     *  measures for all steps.
     *  @see `modeling.Fit` for index of QoF measures.
     *  @see `modeling.Predictor` for more information
     *  @param cross  whether to include the cross/roll-validation QoF measure
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     */
    override def forwardSelAll (cross: Boolean = false)(using qk: Int): (LSET [Int], MatrixD) =
        resetBest ()
        val rSq  = new MatrixD (x.dim2, Fit.qofVectorSize)                   // QoF: R^2, R^2 Bar, sMAPE, R^2 cv
        val cols = LSET (0)                                                  // start with x_0 in model (e.g., intercept)
        updateQoF (rSq, 0, cross, reg.select0 (qk))                          // update Qof results for 0-th variable

        banner (s"forwardSelAll: (qk = $qk, l = 0) INITIAL variable (0, ${fname(0)}) => cols = $cols")

        breakable {
            for l <- 1 until x.dim2 do
                val best = forwardSel (cols)                                 // add most predictive variable
                if best.col == -1 then break ()                              // could not find variable to add
                updateBest (best)
                cols += best.col                                             // add variable x_j
                updateQoF (rSq, l, cross, best)                              // update QoF results for l-th variable
                val (jj, jj_qof) = (best.col, best.qof(qk))
                banner (s"forwardSelAll: (l = $l) ADD variable ($jj, ${fname(jj)}) => cols = $cols @ $jj_qof")
            end for
        } // breakable

        (cols, rSq)
    end forwardSelAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to find the least predictive variable to remove
     *  from the existing model, returning the variable to eliminate, the new parameter
     *  vector and the new Quality of Fit (QoF).  May be called repeatedly.
     *  Adapt from regression to time series forecasting.
     *  @see `Fit` for index of QoF measures.
     *  @param cols   the columns of matrix x currently included in the existing model
     *  @param first  first variable to consider for elimination
     *                      (default (1) assume intercept x_0 will be in any model)
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     */
     def backwardElim (cols: LSET [Int], first: Int = 1) (using qk: Int): BestStep =
        var best = BestStep ()()                                             // best step so far

        for j <- first until x.dim2 if cols contains j do
            val cols_j = cols diff LSET (j)                                  // try removing variable/column x_j
            val x_cols = x(?, cols_j)                                        // x projected onto cols_j columns
            val mod_j  = reg.buildModel (x_cols)                             // regress with x_j added
            mod_j.train ()                                                   // train model
            best = best.better (j, mod_j.test ()._2, mod_j)                  // which is better
        end for

        if best.col == -1 then
            flaw ("backwardElim", "could not find a variable x_j to eliminate: best.col = -1")
        correctQoF (skip, best)
    end backwardElim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to find the least predictive variables to remove
     *  from the full model, returning the variables left and the new Quality of Fit (QoF)
     *  measures for all steps.
     *  @see `modeling.Fit` for index of QoF measures.
     *  @see `modeling.Predictor` for more information
     *  @param first  first variable to consider for elimination
     *  @param cross  whether to include the cross/roll-validation QoF measure
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     */
    override def backwardElimAll (first: Int = 1, cross: Boolean = false)(using qk: Int):
                                 (LSET [Int], MatrixD) =
        resetBest ()
        val rSq  = new MatrixD (x.dim2, Fit.qofVectorSize)                   // R^2, R^2 Bar, sMAPE, R^2 cv
        val cols = LSET.range (0, x.dim2)                                    // start with all x_j in model
        val rem  = ArrayBuffer [Int] ()                                      // start with no columns removed

        val best0 = reg.fullModel (qk)
        updateQoF (rSq, 0, cross, best0)                                     // update QoF results for full model
        val jj_qof = best0.qof(qk)

        banner (s"backwardElimAll: (qk = $qk, l = 0) INITIAL variables (all) => cols = $cols @ $jj_qof")

        breakable {
            for l <- 1 until x.dim2 - 1 do                                   // l indicates number of variables eliminated
                val best = backwardElim (cols, first)                        // remove least predictive variable
                if best.col == -1 then break ()                              // could not find variable to remove
                updateBest (best)
                cols -= best.col                                             // remove variable x_j
                rem  += best.col                                             // keep track of removed columns
                updateQoF (rSq, l, cross, best)                              // update QoF results
                val (jj, jj_qof) = (best.col, best.qof(qk))
                banner (s"backwardElimAll: (l = $l) REMOVE variable ($jj, ${fname(jj)}) => cols = $cols @ $jj_qof")
            end for
        } // breakable

        updateQoF (rSq, x.dim2-1, cross, reg.select0 (qk))                   // update Qof results for 0-th variable
        rem += cols.max                                                      // remove last non-zero column
        rem += 0                                                             // remove column 0

        (LSET.from (rem.reverse), rSq.reverse)                               // reverse the order results
    end backwardElimAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform stepwise regression to find the most predictive variables to have
     *  in the model, returning the variables left and the new Quality of Fit (QoF)
     *  measures for all steps.  At each step it calls forwardSel and backwardElim
     *  and takes the best of the two actions.  Stops when neither action yields improvement.
     *  @see `modeling.Fit` for index of QoF measures.
     *  @see `modeling.Predictor` for more information
     *  @param cross  whether to include the cross/roll-validation QoF measure
     *  @param swap   whether to allow a swap step (swap out a feature for a new feature in one step)
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     */
    override def stepwiseSelAll (cross: Boolean = false, swap: Boolean = true)(using qk: Int):
                                (LSET [Int], MatrixD) =
        resetBest ()
        val rSq    = new MatrixD (x.dim2 - 1, Fit.qofVectorSize)             // QoF: R^2, R^2 Bar, sMAPE, R^2 cv
        val cols   = LSET (0)                                                // start with x_0 in model
        var last_q = Fit.extreme (qk)                                        // current best QoF initialized to extreme
        val vars   = ArrayBuffer [Int]()

        banner (s"stepwiseSelAll: (qk = $qk, l = 0) INITIAL variable (0, ${fname(0)}) => cols = $cols")

        breakable {
            for l <- 1 until x.dim2 - 1 do
                val bestf = forwardSel (cols)                                // add most predictive variable OR
                val bestb = backwardElim (cols, 1)                           // remove least predictive variable
                debug ("stepwiseSelAll", s"bestf = $bestf, bestb = $bestb")

                val slack = 25.0 / l~^2                                      // increase slack to include more features
                                                                             // slack => likely to ADD features at the beginning

                if (bestb.col == -1 || (bestf ge bestb.qof(qk) + slack)) &&  // forward as good as backward
                   (bestf.col != -1 && (bestf gt last_q + slack)) then       // a better model has been found
                    updateBest (bestf)
                    vars  += bestf.col
                    cols  += bestf.col                                       // ADD variable bestf.col
                    last_q = bestf.qof(qk)
                    updateQoF (rSq, l, cross, bestf)                         // update QoF results
                    println (s"\nstepwiseSelAll: (l = $l) ADD variable $bestf")
                    val (jj, jj_qof) = (bestf.col, last_q)
                    banner (s"stepwiseSelAll: (l = $l) ADD variable ($jj, ${fname(jj)}) => cols = $cols @ $jj_qof")

                else if bestb.col != -1 && (bestb gt last_q) then            // a better model has been found
                    updateBest (bestb)
                    vars  += bestb.col
                    cols  -= bestb.col                                       // REMOVE variable bestb.col
                    last_q = bestb.qof(qk)
                    updateQoF (rSq, l, cross, bestb)                         // update QoF results
                    println (s"\nstepwiseSelAll: (l = $l) REMOVE variable $bestb")
                    val (jj, jj_qof) = (bestb.col, last_q)
                    banner (s"stepwiseSelAll: (l = $l) REMOVE variable ($jj, ${fname(jj)}) => cols = $cols @ $jj_qof")

                else
                    if ! swap then break ()
                    val (out, in) = (bestb.col, bestf.col)
                    val bestfb = reg.swapVars (cols, out, in, qk)
                    updateBest (bestfb)
                    if out != -1 && in != -1 && (bestfb gt last_q) then      // a better model has been found
                        vars  += bestb.col
                        vars  += bestf.col
                        cols  -= bestb.col                                   // REMOVE variable bestb.col (swap out)
                        cols  += bestf.col                                   // ADD variable bestf.col (swap in)
                        last_q = bestfb.qof(qk)
                        updateQoF (rSq, l, cross, bestfb)                    // update QoF results
                        println (s"\nstepwiseSelAll: (l = $l) SWAP variable $bestb with $bestf")
                    else
                        println (s"\nstepwiseSelAll: (l = $l) last_q = $last_q better ($bestb, $bestf)")
                        break ()                                             // can't find a better model -> quit
                    end if
                end if

                val x_cols = x(?, cols)                                      // x projected onto cols columns
                val mod_   = reg.buildModel (x_cols)                         // regress on this x
                mod_.train ()                                                // train model
                println (mod_.report (mod_.test ()._2))                      // test and report
            end for
        } // breakable

        println (s"stepwiseSelAll: selected features = $cols")
        println (s"stepwiseSelAll: selected features = ${cols.map (fname (_))}")
        println (s"stepwiseSelAll: features in/out   = $vars")

        (cols, rSq(1 until cols.size))
    end stepwiseSelAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the relative importance of selected variables, ordered highest to
     *  lowest, rescaled so the highest is one.
     *  @param cols  the selected columns/features/variables
     *  @param rSq   the matrix R^2 values (stand in for sse)
     */
    def importance (cols: Array [Int], rSq: MatrixD): Array [(Int, Double)] =
        reg.importance (cols, rSq)
    end importance

end Forecaster_Reg

