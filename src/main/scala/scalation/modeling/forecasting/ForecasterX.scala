
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Aug 23 22:54:01 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model Framework: Forecasters with one Endogenous and Multiple Exogenous Variables
 *
 *  @see ruqinren.wordpress.com/2020/02/21/all-the-confusion-about-arima-arimax-transfer-function-dynamic-regression-models/
 *       robjhyndman.com/hyndsight/arimax/
 *       medium.com/@xwang222/forecasting-101-ep07-multivariate-models-9f3a11fbb374
 */

package scalation
package modeling
package forecasting

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ForecasterX` trait provides a common framework for several forecasting 
 *  models that use 1 ENDOGENOUS variable y and 0 or more EXOGENOUS variables xj.
 *  It provides methods for multi-horizon (1 to h) forecasting using the RECURSIVE technique.
 *  Forecasted values are produced only for the endogenous variable y.
 *  Lower case indicates actual values, while upper case is for forecasted values.
 *
 *  Y_t+1 = f(y_t, y_t-1, ... y_t-p+1, x0_t, x0_t-1, ... x0_t-p+1, x1_t, ...)
 *  Y_t+2 = f(Y_t+1, y_t, ... y_t-p+2, x0_t, x0_t-1, ... x0_t-p+1, x1_t, ...)
 *  ...
 *  Y_t+h = f(Y_t+1, y_t, ... y_t-p+2, x0_t, x0_t-1, ... x0_t-p+1, x1_t, ...)
 *
 *  @see Forecaster - when there are no exogenous variables
 *  @param x     the input/predictor matrix built out of lags of y
 *  @param yy    the output/response vector trimmed to match x.dim
 *  @param lags  the lags (p) used for endogenous variable (e.g., 10 => use lags 1 to 10)
 */
trait ForecasterX (x: MatrixD, yy: VectorD, lags: Int):

    private   val debug = debugf ("ForecasterX", true)                   // debug function
    private   val flaw  = flawf ("ForecasterX")                          // flaw function
    protected var yf: MatrixD = null                                     // forecasts for all time points t & horizons to h

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a vector of size h, of 1 through h-steps ahead forecasts for the model.
     *      forecast the following time points:  t+1, ..., t+h.
     *  Note, must create the yf matrix before calling the forecast method.
     *  Intended to work with rolling validation (analog of predict method)
     *  @param t   the time point from which to make forecasts
     *  @param yf  the forecasting matrix for the endogenous variable y (time x horizons)
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecast (t: Int, yf: MatrixD, h: Int): VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points at horizon h (h-steps ahead).
     *  Assign into forecasting matrix and return the h-steps ahead forecast.
     *  @param yf  the forecasting matrix for the endogenous variable y (time x horizons)
     *  @param yx  the matrix of endogenous y and exogenous x values
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecastAt (yf: MatrixD, yx: MatrixD, h: Int): VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast values for all y_.dim time points and all horizons (1 through h-steps ahead).
     *  Record these in the yf matrix, where
     *      yf(t, k) = k-steps ahead forecast for y_t
     *  Note, column 0, yf(?, 0), is set to y (the actual time-series values).
     *  Forecast recursively down diagonals in the yf forecasting matrix.
     *  The top right and bottom left triangles in yf matrix are not forecastable.
     *  @param y_  the actual values to use in making forecasts
     *  @param yx  the matrix of endogenous y and exogenous x values
     *  @param h   the maximum forecasting horizon, number of steps ahead to produce forecasts
     */
    def forecastAll (y_ : VectorD, yx: MatrixD, h: Int): MatrixD =
        debug ("forecastAll", s"y_.dim = ${y_.dim}, yx.dims = ${yx.dims}")
        yf = new MatrixD (y_.dim+h, h+2)                               // forecasts for all time points t & horizons to h
        for t <- y_.indices do yf(t, 0) = y_(t)                        // first column is the actual endogenous y values
        for t <- yf.indices do yf(t, h+1) = t                          // last column is time (logical day)

        for k <- 1 to h do
            if k > 1 then yx.insert (1, lags, yf(?, k-1))              // insert previous forecasts for endogenous variable
            forecastAt (yf, yx, k)                                     // forecast k-steps into the future
        end for
        yf                                                             // return matrix of forecasted values
    end forecastAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test FORECASTS of a forecasting model y_ = f(lags (y_), x) + e
     *  and return its forecasts and QoF vector.  Testing may be in-sample
     *  (on the training set) or out-of-sample (on the testing set) as determined
     *  by the parameters passed in.  Note: must call train and forecastAll before testF.
     *  @param h   the forecasting horizon, number of steps ahead to produce forecasts
     *  @param y_  the testing/full response/output vector
     *  @param yx  the matrix of endogenous y and exogenous x values
     */
    def testF (h: Int, y_ : VectorD, yx: MatrixD): (VectorD, VectorD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set up testing by making h-steps ahead FORECASTS, and then aligning actual
     *  and forecasted values.  Helper method for implementations of testF method.
     *  DROP the first h elements.
     *  @param y_      the testing/full response/output vector
     *  @param yx      the matrix of endogenous y and exogenous x values
     *  @param h       the forecasting horizon, number of steps ahead to produce forecasts
     *  @param doPlot  whether to plot predicted and actual values vs. time t
     */
    protected def testSetupF (y_ : VectorD, yx: MatrixD, h: Int, doPlot: Boolean = true): (VectorD, VectorD) =
        if yf == null || yf.dim2 < h+2 then yf = forecastAll (y_, yx, h)
        val yh  = yf(?, h)                                             // get column h of yf (y-forecasted)
        val yy  = y_(h until y_.dim)
        val yfh = yh(h until y_.dim)                                   // align actual and forecasted vectors
        assert (yy.dim == yfh.dim)                                     // make sure the vector sizes agree
        if doPlot then new Plot (null, yy, yfh, s"Plot of yy, yfh for ForecasterX model (h = $h) vs. t", true)
        (yy, yfh)
    end testSetupF

end ForecasterX

