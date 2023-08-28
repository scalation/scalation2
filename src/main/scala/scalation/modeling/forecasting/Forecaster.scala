
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 2.0
 *  @date    Sat Dec  8 14:32:12 EST 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model Framework: Forecasters with Vector Input
 *
 *  @see ruqinren.wordpress.com/2020/02/21/all-the-confusion-about-arima-arimax-transfer-function-dynamic-regression-models/
 *       robjhyndman.com/hyndsight/arimax/
 *       medium.com/@xwang222/forecasting-101-ep07-multivariate-models-9f3a11fbb374
 */

package scalation
package modeling
package forecasting

import scala.collection.mutable.Set
import scala.math.{abs, max}
import scala.util.control.Breaks.{break, breakable}

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Compute the sum of squares errors assuming the first 'skip' error is zero.
 *  @param y     the actual response vector
 *  @param yp    the predicted response vector (one-step ahead)
 *  @param skip  skip this many elements at the beginning (defaults to 1)
 */
def ssef (y: VectorD, yp: VectorD, skip: Int = 1): Double =
    var ss = 0.0
    for t <- skip until y.dim do ss += (y(t) - yp(t))~^2
    ss
end ssef

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Compute a reverse dot product of the parameter vector b and the most recent
 *  actual values in the time series y_, going backwards from y_t.
 *  Use max (0, ..) to avoid using negative indices into the y_ vector.
 *  @param b   the parameter/coefficient vector (e.g., φ for AR)
 *  @param y_  the actual values to use in making predictions
 *  @param t   the time point FROM WHICH to make prediction
 */
def rdot (b: VectorD, y_ : VectorD, t: Int): Double =
    var sum = 0.0
    for j <- b.indices do sum += b(j) * y_(max (0, t-j))           // add φ_j y_t-j
    sum
end rdot

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Compute a reverse dot product of the parameter vector b and a diagonal
 *  of the the yf matrix starting at element (r, c) and moving up and back.
 *  Use max (0, ..) to avoid using negative indices into the yf matrix.
 *  @param b   the parameter/coefficient vector (e.g., φ for AR)
 *  @param yf  the forecasting matrix (time x horizons)
 *  @param r   the starting row in the forecasting matrix (time)
 *  @param c   the starting column in the forecasting matrix (horizon)
 */
def rdot (b: VectorD, yf: MatrixD, r: Int, c: Int): Double =
    var sum = 0.0
    for j <- b.indices do sum += b(j) * yf(max (0, r-j), max (0, c-j))
    sum
end rdot

/*----------------------------------------------------------------------------

The FORECASTING MATRIX yf: Example Calculation for AR(3) - move back the diagonal
and up after reaching column 0.

yf  |  h=0   h=1   h=2
-----------------------
t=0 | [1.0]  0.0   0.0
    |      \     \
t=1 | [2.0]  1.1   0.0
    |      \     \
t=2 |  3.0  [1.9]  0.9
    |      \     \
t=3 |  4.0   3.1  [2.1]
    |      \     \
t=4 |  5.0   3.9   2.9
    |      \     \
t=4 |  6.0   5.1   2.9

yf(3, 2) = a + rdot = a + b(0) * yf(2, 1) + b(1) * yf(1, 0) + b(2) * yf(0, 0)

Note: 'a' is the constant term and rdot multiplies the parameter vector 'b' times
elements in a diagonal in reverse.  Also, the upper right triangle is unknowable
unless back-casting is used.

Column h = 0: zeroth horizon forecasts are the actual (e.g., today's known) values in the time series
Column h = 1: horizon one forecasts are the one-step ahead (e.g., tomorrow's) forecasts
Column h = 2: horizon two forecasts are the two-steps ahead (e.g., day after tomorrow's) forecasts

Row time t = 3: yf(3, 0) = 4.0 = the actual value for day 3,
                yf(3, 1) = 3.1 = the one-step ahead forecast for day 3, made yesterday
                yf(3, 2) = 2.1 = the two-steps ahead forecast for day 3, made two days ago

----------------------------------------------------------------------------*/


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Forecaster` trait provides a common framework for several forecasters.
 *  Note, the train method must be called first followed by test.
 *  @param y       the response vector (time-series data)
 *  @param tt      the time vector, if relevant (index as time may suffice)
 *  @param hparam  the hyper-parameters for models extending this trait
 */
trait Forecaster (y: VectorD, tt: VectorD = null, hparam: HyperParameter = null)
      extends Model:

    private   val debug   = debugf ("Forecaster", true)                  // debug function
    private   val flaw    = flawf ("Forecaster")                         // flaw function
    protected val e       = new VectorD (y.dim)                          // residual/error vector [e_0, e_1, ... e_m-1]
    protected var yf: MatrixD = null                                     // forecasts for all time points t & horizons to h

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum lag used by the model (its capacity to look into the past).
     *  Models that use more than one past value to make predictions/forecasts must
     *  override this method, e.g., ARMA (2, 3) should set the cap to max(p, q) = 3.
     */
    def cap: Int = 1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the used response vector y.  Mainly for derived classes where y is
     *  transformed, e.g., `Regression4TS`.
     */
    def getY: VectorD = y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the feature/variable names.  Override for models like SARIMAX.
     */
    def getFname: Array [String] = Array ("no-x features")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a time-series y_, train the forecasting function y_ = f(lags (y_)) + e,
     *  where f(lags (y_)) is a function of the lagged values of y_,
     *  by fitting its parameters.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the testing/full response/output vector (e.g., full y)
     */
    def train (x_null: MatrixD, y_ : VectorD): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test PREDICTIONS of a forecasting model y_ = f(lags (y_)) + e
     *  and return its predictions and  QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train before test.
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the testing/full response/output vector
     */
    def test (x_null: MatrixD, y_ : VectorD): (VectorD, VectorD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set up testing by making PREDICTIONS, and then aligning actual and
     *  predicted values.  Helper method for implementations of test method.
     *  @param y_      the testing/full response/output vector
     *  @param doPlot  whether to plot predicted and actual values vs. time t
     */
    protected def testSetup (y_ : VectorD, doPlot: Boolean = true): (VectorD, VectorD) =
        val yp  = predictAll (y_)                                      // make all predictions
        val yy  = y_(1 until y_.dim)
        val yyp = yp(1 until y_.dim)                                   // align actual and predicted vectors
        assert (yy.dim == yyp.dim)                                     // make sure the vector sizes agree
        if doPlot then new Plot (null, yy, yyp, s"Plot of yy, yp for $modelName vs. t", true)
        (yy, yyp)
    end testSetup

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train and test the forecasting model y_ = f(y-past) + e and report its QoF
     *  and plot its predictions.  Return the predictions and QoF.
     *  @param y_  the training/full response/output vector (defaults to full y)
     *  @param yy  the testing/full response/output vector (defaults to full y)
     */
    def trainNtest (y_ : VectorD = y)(yy: VectorD = y): (VectorD, VectorD) =
        train (null, y_)                                               // train the model on training set
        val (yp, qof) = test (null, yy)                                // test the model on testing set
        println (report (qof))                                         // report on Quality of Fit (QoF)
        println (s"mase = ${Fit.mase (y, yp)}")                        // Means Absolute Scaled Error
        (yp, qof)
    end trainNtest

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test FORECASTS of a forecasting model y_ = f(lags (y_)) + e
     *  and return its forecasts and QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train and forecastAll before testF.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the testing/full response/output vector
     */
    def testF (h: Int, y_ : VectorD): (VectorD, VectorD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set up testing by making h-steps ahead FORECASTS, and then aligning actual
     *  and forecasted values.  Helper method for implementations of testF method.
     *  DROP the first h elements.
     *  @param y_      the testing/full response/output vector
     *  @param h       the forecasting horizon, number of steps ahead to produce forecasts
     *  @param doPlot  whether to plot predicted and actual values vs. time t
     */
    protected def testSetupF (y_ : VectorD, h: Int, doPlot: Boolean = true): (VectorD, VectorD) =
        if yf == null || yf.dim2 < h+2 then yf = forecastAll (y_, h)
        val yh  = yf(?, h)                                             // get column h of yf (y-forecasted)
        val yy  = y_(h until y_.dim)
        val yfh = yh(h until y_.dim)                                   // align actual and forecasted vectors
        assert (yy.dim == yfh.dim)                                     // make sure the vector sizes agree
        if doPlot then new Plot (null, yy, yfh, s"Plot of yy, yfh for $modelName (h = $h) vs. t", true)
        (yy, yfh)
    end testSetupF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the hyper-parameters.
     */
    def hparameter: HyperParameter = hparam

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of parameter/coefficient values (they are model specific).
     *  Override for models with parameters.
     */
    def parameter: VectorD = new VectorD (0)                           // vector with no elements
    def nparams: Int       = parameter.dim                             // number of parameters

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors.
     */
    def residual: VectorD = { if e == null then flaw ("residual", "must call test method first"); e }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The standard signature for prediction does not apply to time-series.
     */
    def predict (z: VectorD): Double =
        throw new UnsupportedOperationException ("predict (VectorD) use an alternative below")
    end predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict a value for y_t+1 using the 1-step ahead forecast.
     *      y_t+1 = f (y_t, ...) + e_t+1
     *  @param t   the time point from which to make prediction
     *  @param y_  the actual values to use in making predictions
     */
    def predict (t: Int, y_ : VectorD): Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict all values corresponding to the given vector y_.
     *  @param y_  the actual values to use in making predictions
     */
    def predictAll (y_ : VectorD): VectorD =
        val yp = new VectorD (y_.dim + 1)                     // can't predict first day (set to 0)
        for t <- y_.indices do yp(t+1) = predict (t, y_)
        yp
    end predictAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a vector of size h, of 1 through h-steps ahead forecasts for the model.
     *      forecast the following time points:  t+1, ..., t-1+h.
     *  Note, must create the yf matrix before calling the forecast method.
     *  Intended to work with rolling validation (analog of predict method)
     *  @param t   the time point from which to make forecasts
     *  @param yf  the forecasting matrix (time x horizons)
     *  @param y_  the actual values to use in making predictions
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecast (t: Int, yf: MatrixD, y_ : VectorD, h: Int): VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points at horizon h (h-steps ahead).
     *  Assign into forecasting matrix and return the h-steps ahead forecast.
     *  @param yf  the forecasting matrix (time x horizons)
     *  @param y_  the actual values to use in making forecasts
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecastAt (yf: MatrixD, y_ : VectorD, h: Int): VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points and all horizons (1 through h-steps ahead).
     *  Record these in the yf matrix, where
     *      yf(t, k) = k-steps ahead forecast for y_t
     *  Note, column 0, yf(?, 0), is set to y (the actual time-series values).
     *  Forecast recursively down diagonals in the yf forecasting matrix.
     *  The top right and bottom left triangles in yf matrix are not forecastable.
     *  @param y_  the actual values to use in making forecasts
     *  @param h   the maximum forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecastAll (y_ : VectorD, h: Int): MatrixD =
        debug ("forecastAll", s"y_.dim = ${y_.dim}, e.dim = ${e.dim}")
        yf = new MatrixD (y_.dim+h, h+2)                               // forecasts for all time points t & horizons to h
        for t <- y_.indices do yf(t, 0) = y_(t)                        // first column is the actual values
        for t <- yf.indices do yf(t, h+1) = t                          // last column is time (logical day)

        for k <- 1 to h do forecastAt (yf, y_, k)                      // forecast k-steps into the future
        yf                                                             // return matrix of forecasted values
    end forecastAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to find the most predictive variable to add the
     *  existing model, returning the variable to add and the new model.
     *  May be called repeatedly.
     *  Note, all lags up and including 'p|q' define the model.
     *  @see `Fit` for index of QoF measures.
     *  @param cols   the lags/columns currently included in the existing model (currently ignored)
     *  @param idx_q  index of Quality of Fit (QoF) to use for comparing quality
     */
    def forwardSel (cols: Set [Int], idx_q: Int = QoF.rSqBar.ordinal): (Int, Forecaster) = ???

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to find the most predictive lags/variables to have
     *  in the model, returning the variables added and the new Quality of Fit (QoF)
     *  measures for all steps.
     *  @see `Fit` for index of QoF measures.
     *  @param idx_q  index of Quality of Fit (QoF) to use for comparing quality
     *  @param cross  whether to include the cross-validation QoF measure (currently ignored)
     */
    def forwardSelAll (idx_q: Int = QoF.rSq.ordinal, cross: Boolean = false): (Set [Int], MatrixD) =
        val rSq  = new MatrixD (MAX_LAGS, 3)                              // R^2, R^2 Bar, R^2 cv
        val cols = Set (1)                                                // start with lag1 in model

        println (s"forwardSelAll (l = 0): cols = $cols")
        breakable {
            for l <- 2 until MAX_LAGS do
                val (j, mod_j) = forwardSel (cols, idx_q)                 // add most predictive variable
                if j == -1 then break ()
                cols     += j                                             // add variable x_j
//              val fit_j = mod_j.fit
                val fit_j = mod_j.test (null, y)._2
                rSq(l)    = Fit.qofVector (fit_j, null)                   // use new model, mod_j, no cross
                val k     = cols.size - 1
                println (s"==> forwardSelAll (l = $l): add (#$k) variable $j, cols = $cols, qof = ${fit_j(idx_q)}")
            end for
        } // breakable

        (cols, rSq(0 until cols.size))
    end forwardSelAll

end Forecaster


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Forecaster` companion object provides functions useful for forecasting
 */
object Forecaster:

    private val flaw = flawf ("Forecaster")                               // flaw function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Point out the differences between two vectors/time-series.
     *  @param u      the first vector/time-series
     *  @param v      the second vector/time-series
     *  @param scale  the scale factor to set the tolerance 'tol'
     *  @param allow  flag indicating whether allow (via assert) any differences
     */
    def differ (u: VectorD, v: VectorD, scale: Double = 1E-9, allow: Boolean = true): Int =
        if u.dim != v.dim then flaw ("differ", s"requires u.dim = ${u.dim} = v.dim = ${v.dim}")
        val tol = u.mean * scale
        var cnt = 0
//      for t <- u.indices if u(t) !=~ v(t) do                            // machine epsilon
        for t <- u.indices if abs (u(t) - v(t)) > tol do                  // application tolerance
            cnt += 1
            println (s"differ at t = $t: ${u(t)} \t ${v(t)}")
        end for
        banner (s"differ (u, v): found $cnt points that differ")
        if ! allow then assert (cnt == 0)
        cnt
    end differ

end Forecaster

