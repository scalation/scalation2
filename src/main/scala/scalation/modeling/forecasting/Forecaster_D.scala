
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Jun 30 13:27:00 EDT 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Framework: Abstract Class for Forecasters with Matrix Input
 *           most models will need to override `train`, `predict`, `forecast` and `forecastAt`
 */

package scalation
package modeling
package forecasting

import scala.math.max
//import scala.math.min

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Forecaster_D` abstract class provides a common framework for several forecasters.
 *  Note, the `train_x` method must be called first followed by `test`.
 *  @param x        the input lagged time series data
 *  @param y        the response matrix (time series data per horizon)
 *  @param hh       the maximum forecasting horizon (h = 1 to hh)
 *  @param tRng     the time range, if relevant (index as time may suffice)
 *  @param hparam   the hyper-parameters for models extending this abstract class
 *  @param bakcast  whether a backcasted value is prepended to the time series (defaults to false)
 */
abstract class Forecaster_D (x: MatrixD, y: MatrixD, hh: Int, tRng: Range = null,
                             hparam: HyperParameter = MakeMatrix4TS.hp,
                             bakcast: Boolean = false)
      extends Forecaster (y(?, 0), hh, tRng, hparam, bakcast):          // no automatic backcasting, @see `ARY_D.apply`

    private val debug = debugf ("Forecaster_D", true)                   // debug function

    protected var bb: MatrixD = null                                    // use parameter matrix bb instead of vector b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the used response/output matrix y.
     */
    def getYy: MatrixD = y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train/fit e.g., an `ARY_D` model to the times-series data in vector y_.
     *  Estimate the coefficient vector b for a p-th order Auto-Regressive ARY_D(p) model.
     *  Uses OLS Matrix Fatorization to determine the coefficients, i.e., the b (φ) vector.
     *  @param x_  the data/input matrix (e.g., full x)
     *  @param y_  the training/full response vector (e.g., full y)
     */
    def train_x (x_ : MatrixD, y_ : MatrixD): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train and test the forecasting model y_ = f(y-past) + e and report its QoF
     *  and plot its predictions.  Return the predictions and QoF.
     *  NOTE: must use `trainNtest_x` when an x matrix is used, such as in `ARY_D`.
     *  @param x_  the training/full data/input matrix (defaults to full x)
     *  @param y_  the training/full response/output vector (defaults to full y)
     *  @param xx  the testing/full data/input matrix (defaults to full x)
     *  @param yy  the testing/full response/output vector (defaults to full y)
     */
    def trainNtest_x (x_ : MatrixD = x, y_ : MatrixD = y)(xx: MatrixD = x, yy: MatrixD = y):
                     (VectorD, VectorD) =
        train_x (x_, y_)                                                // train the model on training set
        val (yp, qof) = test (xx, yy)                                   // test the model on testing set
        println (report (qof))                                          // report on Quality of Fit (QoF)
        (yp, qof)
    end trainNtest_x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test PREDICTIONS of a forecasting model y_ = f(lags (y_)) + e
     *  and return its predictions and  QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train before test.
     *  Must override to get Quality of Fit (QoF).
     *  @param x_null  the data/input matrix
     *  @param y_      the actual testing/full response/output matrix
     */
    def test (x_ : MatrixD, y_ : MatrixD): (VectorD, VectorD) =
        val m = y_.dim - 1
        predictAll (y_)                                                 // make all predictions - saved in yf
        debug ("test", s"x_.dims = ${x_.dims}, y_.dims, ${y_.dims}, yf.dims = ${yf.dims}")

        val y0  = y_(0 until m, 0)                                      // actual values (except last) for h = 1
        val yf1 = yf(0 until m, 1)                                      // forecasted values for h = 1
        new Plot (null, y0, yf1, s"test: Plot of y0, yf1 for $modelName vs. t", true)
        mod_resetDF (y0.dim)                                            // reset the degrees of freedom
        (yf1, diagnose (y0, yf1))                                       // return predicted and QoF vectors
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Models need to provide a means for updating the Degrees of Freedom (DF).
     *  @param size  the size of dataset (full, train, or test)
     */
    override def mod_resetDF (size: Int): Unit =
        val dfm = max (1, parameter.size - 1)                           // degrees of freedom for model
        debug ("mod_resetDF", s"dfm = $dfm, df = ${size-dfm}")
        resetDF (dfm, size - dfm)
    end mod_resetDF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of parameter/coefficient values (they are model specific).
     *  Override for models with other parameters besides bb(?, 0).
     */
    override def parameter: VectorD = bb(?, 0)                          // parameter vector (first column)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t using the 1-step ahead forecast.
     *
     *      y_t = b_0 + b_1 y_t-1 + b_2 y_t-2 + ... + b_p y_t-p = b dot x_t
     *
     *  FIX - parameter order is in conflict with AR models.
     *  @param t   the time point being predicted
     *  @param y_  the actual values to use in making predictions (ignored)
     */
    def predict (t: Int, y_ : MatrixD): VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict all values (for all horizons) corresponding to the given time series vector y_.
     *  Create FORECAST MATRIX yf and return it.
     *  Note `forecastAll` simply returns the values produced by `predictAll`.
     *  @param y_  the actual time series values to use in making predictions
     */
    def predictAll (y_ : MatrixD): MatrixD =
        for t <- 0 until y_.dim do
            val pred = predict (t, y_)
//          debug ("predictAll", s"pred = $pred")
            yf(t, 1 until hh+1) = pred                                  // FIX - yf for VAR is different
        yf
    end predictAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a vector of size hh, h = 1 to hh-steps ahead forecasts for the model,
     *  i.e., forecast the following time points:  t+1, ..., t+h.
     *  Intended to work with rolling validation (analog of predict method).
     *  @param t   the time point from which to make forecasts
     *  @param y_  the actual values to use in making predictions
     */
//  def forecast (t: Int, y_ : MatrixD): VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points and all horizons (1 through hh-steps ahead).
     *  Record these in the FORECAST MATRIX yf, where
     *
     *      yf(t, h) = h-steps ahead forecast for y_t
     *
     *  @param y_  the actual values to use in making forecasts
     */
    def forecastAll (y_ : MatrixD): MatrixD = yf

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use rolling-validation to compute test Quality of Fit (QoF) measures
     *  by dividing the dataset into a TRAINING SET (tr) and a TESTING SET (te).
     *  as follows:  [ <-- tr_size --> | <-- te_size --> ]
     *  Calls forecast for h-steps ahead out-of-sample forecasts.
     *  Return the forecasted values in the FORECAST MATRIX,.
     *  @param rc_      the retraining cycle (number of forecasts until retraining occurs)
     *  @param growing  whether the training grows as it roll or kepps a fixed size
     */
    override def rollValidate (rc: Int = 2, growing: Boolean = false): MatrixD =
        banner (s"rollValidate: Evaluate ${modelName}'s QoF for horizons 1 to $hh:")

        val yf      = getYf                                             // get the full in-sample forecast matrix
        val te_size = Forecaster.teSize (y.dim)                         // size of testing set
        val tr_size = y.dim - te_size                                   // size of initial training set
        debug ("rollValidate", s"y.dims = ${y.dims}, train: tr_size = $tr_size; test: te_size = $te_size, rc = $rc")

        val yp = new VectorD (te_size-1)
        for i <- 0 until te_size-1 do                                   // iterate through testing set
            val is = if growing then 0 else i
            val t  = tr_size + i                                        // next time point to forecast
            if i % rc == 0 then
                val x_ = if x != null then x(is until t) else null
                train_x (x_, y(is until t))                             // retrain on sliding training set
                debug ("rollValidate", s"retrain on i = $i, bb = $bb")
//          val yd = predict (min (t+1, y.dim-1), y)                    // predict the next value (only for h=1)
            val yd = predict (t, y)                                     // predict the next value (only for h=1)
            val yd2 = forecast (t, y(?, 0))                             // forecast the next hh-values, yf is updated
            println (s"n yd = $yd \n yd2 = $yd2")
            yp(i) = yd2(0)
            println (s"yf(t, 0) = ${yf(t, 0)}, yp(i) = ${yp(i)}, yd = $yd")
        end for

        val y_ = y(?, 0)(tr_size until y.dim-1)                         // trim the actual values
        val t  = VectorD.range (tr_size, y.dim-1)
        new Plot (t, y_, yp, s"rollValidate: Plot y_, yp vs. t for $modelName", lines = true)
        yf
    end rollValidate

end Forecaster_D

