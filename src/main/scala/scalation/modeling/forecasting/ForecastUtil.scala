
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Feb 13 16:22:21 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Framework: Utilities for Time Series Forecasting
 */

package scalation
package modeling
package forecasting

import scala.math.max

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Given a response vector y, build and return
 *  (1) an input/predictor MATRIX xx and
 *  (2) an output/multi-horizon output/response MATRIX yy.
 *  Used by Multi-Variate (MV) forecast models such as `ARX_MV`.
 *  that use DIRECT multi-horizon forecasting.
 *  The first lag responses can't be predicted due to missing past values.
 *  The last h-1 responses can't be predicted due to missing future values.
 *  Therefore the number of rows in xx and yy is reduced to y.dim + 1 - lags - h.
 *
 *  FIX - try to extend to "val xx = new MatrixD (y.dim - h, lags)"
 *
 *  @param y     the given output/response vector
 *  @param lags  the maximum lag included (inclusive)
 *  @param h     the forecasting horizon (1, 2, ... h)
 */
def buildMatrix4TS (y: VectorD, lags: Int, h: Int): (MatrixD, MatrixD) =
    val xx = new MatrixD (y.dim + 1 - lags - h, lags)
    val yy = new MatrixD (y.dim + 1 - lags - h, h)
    for i <- lags to y.dim - h do
        for j <- xx.indices2 do xx(i-lags, lags - 1 - j) = y(i - 1 - j)
        for j <- yy.indices2 do yy(i-lags, j) = if i + j >= y.dim then -0.0 else y(i + j)
//      for j <- yy.indices2 do yy(i-lags, j) = y(i + j)
    end for
//  println (s"buildMatrix4TS: xx = $xx \n yy = $yy")
    (xx, yy)
end buildMatrix4TS


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Given a response vector y, build and return
 *  (1) an input/predictor MATRIX xx and 
 *  (2) an output/single-horizon output/response VECTOR yy. 
 *  Used by Single-Variate forecast models such as `ARX`.
 *  that use RECURSIVE multi-horizon forecasting.
 *  The first response can't be predicted due to missing past values.
 *  Therefore the number of rows in xx and yy is reduced to y.dim - 1.
 *  @param y     the given output/response vector
 *  @param lags  the maximum lag included (inclusive)
 */  
def buildMatrix4TS (y: VectorD, lags: Int): (MatrixD, VectorD) =
    val xx = new MatrixD (y.dim - 1, lags)
    val yy = new VectorD (y.dim - 1)
    for i <- 1 until y.dim do
        for j <- xx.indices2 do xx(i-1, lags - 1 - j) = y(max(i - 1 - j, 0))
        yy(i-1) = y(i)
    end for
//  println (s"buildMatrix4TS: xx = $xx \n yy = $yy")
    (xx, yy)
end buildMatrix4TS


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Given an exogenous variable vector ex corresponding to an endogenous response
 *  vector y, build and return an input/predictor MATRIX xx.
 *  The first lag responses can't be predicted due to missing past values.
 *  Therefore the number of rows in xx is reduced to ex.dim - lags.
 *  @param ex     the exogenous variable vector
 *  @param lags   the maximum lag included (inclusive) for the endogenous variable
 *  @param elag1  the minimum lag included (inclusive) for the exogenous variable
 *  @param elag2  the maximum lag included (inclusive) for the exogenous variable
 */
def buildMatrix4TS_exo (ex: VectorD, lags: Int, elag1: Int, elag2: Int): MatrixD =
    val flaw = flawf ("top")
    val n = elag2 - elag1
    if n < 1 then flaw ("buildMatrix4TS_exo", "min exo lag must be smaller than max exo lag")
//  if elag2 > lags then flaw ("buildMatrix4TS_exo", "exo lag cannot exceed endogenous lag")

    val xx = new MatrixD (ex.dim - lags, n)
    for i <- lags until ex.dim do
        for j <- xx.indices2 do xx(i-lags, n - 1 - j) = ex(i - elag1 - j)
    end for
//  println (s"buildMatrix4TS_exo: xx = $xx")
    xx
end buildMatrix4TS_exo


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Build a 3D tensor that collects and (lag) expands the endogenous variable and all
 *  exogenous variables into the form 'time x lags x variables' --> INPUT TENSOR.
 *  Lags for endogenous variable:  1, 2, ... lags
 *  Lags for exogenous variables:  el, el+1, ... el-1+lags 
 *  For models like SARIMAX with weekly data, exo variables are not forecasted, so it
 *  is not possible to use exogenous lag 1 to make week-two forecasts.
 *  Also build a matrix of target values for each forecasting horizon --> OUTPUT MATRIX.
 *  The number of rows m = y.dim - el, as forecasts cannot be made unless there is at
 *  least one endogenous and one exogenous lag (past value) available to the model.
 *  Model: yy_hat = f(xx) with loss function, e.g., || yy - yy_hat ||_F
 *  NOTE: for models not taking tensor input, flatten into a matrix.
 *  @param y     the endogenous variable vector over time (e.g., new_deaths)
 *  @param ex    the exogenous variable matrix over time x exo_vars (e.g., icu_patients, hosp_patient)
 *  @param lags  the maximum lag included (inclusive)
 *  @param h     the forecasting horizon (1, 2, ... h)
 *  @param el    the first exogenous lag (may be larger than 1)
 */
def buildTensor4TS (y: VectorD, ex: MatrixD, lags: Int, h: Int = 1)(el: Int = h): (TensorD, MatrixD) =
    val flaw = flawf ("top")
    if y.dim != ex.dim then
        flaw ("buildTensor4TS", s"endo and exo variable sizes do not match: y.dim = ${y.dim} != ex.dim = ${ex.dim}")
    val m = y.dim - el                                                     // number of rows

    val xx = new TensorD (m, lags, 1 + ex.dim2)                            // input tensor from endo and exo vars
    for i <- xx.indices; j <- xx.indices2; k <- xx.indices3 do             // time x lags x variables
        xx(i, j, k) = if k == 0 then y(max(i - el - j, 0))
                      else ex(max(i - el - j, 0), k-1)

    val yy = new MatrixD (m, h)                                            // output matrix from endo vars
    for i <- yy.indices; j <- yy.indices2 do                               // time x horizons
        yy(i, j) = y(max(i - el - j, 0))

    println (s"buildTensor4TS: xx.dims = ${xx.dims}, yy.dims = ${yy.dims}")
    (xx, yy)                                                               // tuple of (input tensor, output matrix)
end buildTensor4TS


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Test the actual response vector vs. forecasted matrix, returning the QoF
 *  for all forecasting horizons 1 to h.
 *  @param mod  the fittable model (one that extends `Fit`)
 *  @param y    the original actual response vector
 *  @param yf   the forecasted response matrix
 *  @param p    the number of variables/lags used in the model
 */
def testForecast (mod: Fit, y: VectorD, yf: MatrixD, p: Int): MatrixD =
    MatrixD (for k <- yf.indices2 yield
        val y_  = y(p + k until y.dim)
        val yf_ = yf(?, k)(0 until y.dim - p - k)
        println (s"y_.dim = ${y_.dim}, yf_.dim = ${yf_.dim}")
        mod.resetDF (p, y.dim - p - (k+1))                               // reset the degrees of freedom
        mod.diagnose (y_, yf_))                                          // return the QoF of the forecasts
end testForecast

