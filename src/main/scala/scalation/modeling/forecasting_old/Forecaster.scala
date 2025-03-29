
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 2.0
 *  @date    Sat Dec  8 14:32:12 EST 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Framework: Base Trait for Forecasters with Vector Input
 *
 *  @see ruqinren.wordpress.com/2020/02/21/all-the-confusion-about-arima-arimax-transfer-function-dynamic-regression-models/
 *       robjhyndman.com/hyndsight/arimax/
 *       medium.com/@xwang222/forecasting-101-ep07-multivariate-models-9f3a11fbb374
 */

package scalation
package modeling
package forecasting_old

import scala.collection.mutable.Set
import scala.math.abs
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
 *  @param y_  the actual time series values to use in making predictions
 *  @param t   the time point FROM WHICH to make prediction
 */
def rdot (b: VectorD, y_ : VectorD, t: Int): Double =
    var sum = 0.0
    for j <- b.indices do sum += b(j) * y_(max0 (t-j))             // add φ_j y_t-j
    sum
end rdot

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Compute a reverse dot product of the parameter vector b and a diagonal
 *  of the the yf matrix starting at element (r, c) and moving up and back.
 *  Use max (0, ..) to avoid using negative indices into the yf matrix.
 *  @param b   the parameter/coefficient vector (e.g., φ for AR)
 *  @param yf  the forecast matrix (time x horizons)
 *  @param r   the starting row in the forecast matrix (time)
 *  @param c   the starting column in the forecast matrix (horizon)
 */
def rdot (b: VectorD, yf: MatrixD, r: Int, c: Int): Double =
    var sum = 0.0
    for j <- b.indices do sum += b(j) * yf(max0 (r-j), max0 (c-j))
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
 *  @param y       the response vector (time series data)
 *  @param tt      the time vector, if relevant (index as time may suffice)
 *  @param hparam  the hyper-parameters for models extending this trait
 */
trait Forecaster (y: VectorD, tt: VectorD = null, hparam: HyperParameter = null)
      extends Model:

    private   val debug       = debugf ("Forecaster", true)              // debug function
    private   val flaw        = flawf ("Forecaster")                     // flaw function
    protected val e           = new VectorD (y.dim+1)                    // residual/error vector [e_0, e_1, ... e_m]
    protected var yf: MatrixD = null                                     // forecasts for all time points t & horizons to h

    if tt != null then println (s"Forecaster: time parameter vector tt has size $tt.dim")

    /** As seen from class WeightedMovingAverage, the missing signatures are as follows.
     *  For convenience, these are usable as stub implementations.
     */
    def crossValidate(k: Int, rando: Boolean): Array[scalation.mathstat.Statistic] = ???
    def getX: scalation.mathstat.MatrixD = ???

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the maximum lag used by the model (its capacity to look into the past).
     *  Models that use more than one past value to make predictions/forecasts must
     *  override this method, e.g., ARMA (2, 3) should set the cap to max(p, q) = 3.
     */
    def cap: Int = 1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the used response vector y.  Used by derived classes where y may be
     *  transformed, e.g., `ARX`.
     */
    def getY: VectorD = y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the prediction vector yp.
     */
    def getYp: VectorD =
        if yf == null then flaw ("getYp", "can't access, since yf is null, call `predictAll` first")
        yf(?, 1)
    end getYp

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the FORECAST MATRIX yf (initially allocated in `predictAll` method).
     */
    def getYf: MatrixD = yf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the feature/variable names.  Override for models like SARIMAX.
     */
    def getFname: Array [String] = Array ("no-x features")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a time series y_, train the forecasting function y_ = f(lags (y_)) + e,
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
     *  Must override to get Quality of Fit (QoF).
     *  @param x_null  the data/input matrix (ignored, pass null)
     *  @param y_      the actual testing/full response/output vector
     */
    def test (x_null: MatrixD, y_ : VectorD): (VectorD, VectorD) =
        val yp = predictAll (y_)                                       // make all predictions
        assert (y_.dim == yp.dim)                                      // make sure the vector sizes agree
        new Plot (null, y_, yp, s"test: Plot of y_, yp for $modelName vs. t", true)
        (yp, null)                                                     // override to get QoF
    end test

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
     *  and RETURN (1) aligned actual values, (2) its forecasts and (3) QoF vector.
     *  Testing may be in-sample (on the training set) or out-of-sample (on the testing set)
     *  as determined by the parameters passed in.  Note: must call train and forecastAll
     *  before testF.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the testing/full response/output vector
     */
    def testF (h: Int, y_ : VectorD): (VectorD, VectorD, VectorD)      // actual, forecasted, qof

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
        val yy  = y_(h-1 until y_.dim)
        val yfh = yh(h-1 until y_.dim)                                 // align actual and forecasted vectors
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
    /** The standard signature for prediction does not apply to time series.
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
    /** Predict all values corresponding to the given time series vector y_.
     *  @param y_  the actual values to use in making predictions
    def predictAll (y_ : VectorD): VectorD =
        val yp = new VectorD (y_.dim + 1)                            // can't predict first day (set to 0)
        for t <- y_.indices do yp(t+1) = predict (t, y_)
        yp
    end predictAll
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict all values corresponding to the given time series vector y_.
     *  Create FORECAST MATRIX yf and return PREDICTION VECTOR yp as second (1) column
     *  of yf with last value removed.
     *  @see `forecastAll` to forecast beyond horizon h = 1.
     *  @param y_  the actual time series values to use in making predictions
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def predictAll (y_ : VectorD): VectorD =
        yf = new MatrixD (y_.dim + 1, 3)                             // forecasts for all time points t & horizons to h
        for t <- y_.indices do yf(t, 1) = y_(t)                      // first (0) column holds the actual time series values
        for t <- yf.indices do yf(t, 2) = t                          // last (2) column holds time (logical day)

        val yy = WeightedMovingAverage.backcast (y_) +: y_           // prepend by adding one backcasted value
        for t <- yy.indices do yf(t, 1) = predict (t, yy)            // use model to make predictions
//      debug ("predictAll", s"y_.dim = ${y_.dim}, yf.dims = ${yf.dim}")
        yf(?, 1)(0 until y_.dim)                                     // return yp: first horizon only
    end predictAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a vector of size h, of 1 through h-steps ahead forecasts for the model.
     *      forecast the following time points:  t+1, ..., t-1+h.
     *  Note, must create the yf matrix before calling the forecast method.
     *  Intended to work with rolling validation (analog of predict method)
     *  @param t   the time point from which to make forecasts
     *  @param yf  the forecast matrix (time x horizons)
     *  @param y_  the actual values to use in making predictions
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecast (t: Int, yf: MatrixD, y_ : VectorD, h: Int): VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points at horizon h (h-steps ahead).
     *  Assign into FORECAST MATRIX and return the h-steps ahead forecast.
     *  Note, `predictAll` provides predictions for h = 1.
     *  @see `forecastAll` method in `Forecaster` trait.
     *  @param yf  the forecast matrix (time x horizons)
     *  @param y_  the actual values to use in making forecasts
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecastAt (yf: MatrixD, y_ : VectorD, h: Int): VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast intervals for all y_.dim time points at horizon h (h-steps ahead).
     *  Create prediction intervals (two vectors) for the given time points at level p.
     *  Caveat:  assumes errors follow a Normal distribution.  Override this method
     *           to handle other cases.
     *  @param y_   the aligned actual values to use in making forecasts
     *  @param yfh  the forecast vector at horizon h
     *  @param h    the forecasting horizon, number of steps ahead to produce forecasts
     *  @param p    the level (1 - alpha) for the prediction interval
     */
    def forecastAtI (y_ : VectorD, yfh: VectorD, h: Int, p: Double = 0.9): (VectorD, VectorD) =
        debug ("forecastAtI", s"for h = $h: y_.dim = ${y_.dim}, yfh.dim = ${yfh.dim}")
        val sig_h = (y_ - yfh).stdev                                   // standard error of estimate at horizon h
        val width = z_sigma (sig_h, p)                                 // interval half width
        (yfh - width, yfh + width)                                     // return lower and upper bounds
    end forecastAtI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points and all horizons (1 through h-steps ahead).
     *  Record these in the FORECAST MATRIX yf, where
     *      yf(t, k) = k-steps ahead forecast for y_t
     *  Note, column 0, yf(?, 0), is set to y (the actual time series values).
     *     last column, yf(?, h+1), is set to t (the time values, for reference).
     *  Forecast recursively down diagonals in the yf forecast matrix.
     *  The top right and bottom left triangles in yf matrix are not forecastable.
     *  FIX - merge the forecast matrices used by predictAll and forecastAll.
     *  @param y_  the actual values to use in making forecasts
     *  @param h   the maximum forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecastAll (y_ : VectorD, h: Int): MatrixD =
/*
        val yp_ = yf(?, 1)                                             // pull predictions via predictAll's yf column 1
        yf = new MatrixD (y_.dim + h, h + 2)                           // forecasts for all time points t & horizons to h
        debug ("forecastAll", s"y_.dim = ${y_.dim}, yp_.dim = ${yp_.dim}, e.dim = ${e.dim}, yf.dims = ${yf.dims}")

        for t <- y_.indices  do yf(t, 0)   = y_(t)                     // first column (0) holds the actual time series values
        for t <- yp_.indices do yf(t, 1)   = yp_(t)                    // second column (1) holds the predictions (h = 1)
        for t <- yf.indices  do yf(t, h+1) = t                         // last column (h+1) holds time (logical day)
*/
        yf = makeForecastMatrix (y_, yf(?, 1), h)                      // make forecast matrix yf from prediction matrix yf
        for k <- 2 to h do forecastAt (yf, y_, k)                      // forecast k-steps into the future
        println (s"forcastAll: yf = $yf")
        yf                                                             // return matrix of forecasted values
    end forecastAll

   
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Make the full FORECAST MATRIX from the prediction forecast matrix (built by `prodictAll`).
     *  Has has more columns and a few more rows and copies all contents from the prediction
     *  forecast matrix.
     *  @param y_   the actual values to use in making forecasts
     *  @param yp_  the predicted values (h=1) to use in making forecasts
     *  @param h    the maximum forecasting horizon, number of steps ahead to produce forecasts
     */
    def makeForecastMatrix (y_ : VectorD, yp_ : VectorD, h: Int): MatrixD =
        val yf_ = new MatrixD (y_.dim + h, h + 2)                      // forecasts for all time points t & horizons to h
        debug ("makeForecastMatrix", s"forecast matrix: y_.dim = ${y_.dim}, yp_.dim = ${yp_.dim} --> yf_.dims = ${yf_.dims}")

        for t <- y_.indices  do yf_(t, 0)   = y_(t)                    // first column (0) holds the actual time series values
        for t <- yp_.indices do yf_(t, 1)   = yp_(t)                   // second column (1) holds the predictions (h = 1)
        for t <- yf_.indices do yf_(t, h+1) = t                        // last column (h+1) holds time (logical day)
        yf_
    end makeForecastMatrix 

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
/** The `Forecaster` companion object provides methods useful for classes extending
 *  the `Forecaster` trait, i.e., forecasting models with a single input variable.
 */
object Forecaster:

//  private val debug = debugf ("Forecaster", true)                       // debug function
    private val flaw  = flawf ("Forecaster")                              // flaw function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Point out the differences between two vectors/time series.
     *  @param u      the first vector/time series
     *  @param v      the second vector/time series
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check the first two columns (horizon 0 and 1) of the forecast matrix yf.
     *  Assertions will terminate the program when these disagree with [y, yp].
     *  Note, =~ is approximately equals.
     *  @param yf       the forecast matrix (time x horizons)
     *  @param y        the actual time series values
     *  @param yp       the values from the predict method
     *  @param show_yf  whether to show the whole forecast matrix yf
    def checkForecastMatrix (yf: MatrixD, y: VectorD, yp: VectorD, show_yf: Boolean = false): Unit =
        if show_yf then println (s"yf = $yf")
        banner (s"checkForecastMatrix: yf.dims = ${yf.dims}, y.dim = ${y.dim}, yp.dim = ${yp.dim}")
        val m   = y.dim
        val yf0 = yf(?, 0)(0 until m)
        val yf1 = yf(?, 1)(1 until m)                                     // FIX: in ARX -- yp is shifted (0, m-1)
        debug ("checkForecastMatrix", "yf0 vs. y");  differ (yf0, y)
        debug ("checkForecastMatrix", "yf1 vs. yp"); differ (yf1, yp)
        assert (yf0 =~ y)                                                 // zeroth forecast = actual values
        assert (yf1 =~ yp)                                                // first forecast = predicted values
    end checkForecastMatrix
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluate the quality of point and optionally interval forecast for horizon (h = 1 to hh).
     *  @param mod   the forecasting model to be evaluated
     *  @param y     the actual time series values
     *  @param hh    the maximum forecasting horizon (h = 1 to hh)
     *  @param ints  whether to evaluate prediction interval forecasts as well as point forecasts
     */
    def evalForecasts (mod: Forecaster & Fit, y: VectorD, hh: Int, ints: Boolean = false): Unit =
        val ftMat = new MatrixD (hh, Fit.N_QoF)
        banner (s"Evaluate ${mod.modelName}'s QoF for horizons 1 to $hh:")

        for h <- 1 to hh do
            val (yy, yfh, qof) = mod.testF (h, y)                         // h-steps ahead forecast and its QoF
            ftMat(h-1) = qof 
//          println (FitM.fitMap (qof, qoF_names))                        // evaluate h-steps ahead forecasts

            if ints then
                val (low, up) = mod.forecastAtI (yy, yfh, h)              // prediction interval forecasts
                val qof_all   = mod.diagnose_ (yy, yfh, low, up)          // fully evaluate h-steps ahead forecasts
                mod.show_interval_forecasts (yy, yfh, low, up, qof_all, h)
        end for

        println ("fitMap     qof = ")
        println (FitM.showFitMap (ftMat.transpose, QoF.values.map (_.toString)))
    end evalForecasts

end Forecaster

