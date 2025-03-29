
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Feb 13 16:22:21 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model Framework: Utilities for Time Series Forecasting
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
 *  Used by Multi-Variate (MV) forecast models such as `RegressionMV4TS`.
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
 *  Used by Single-Variate forecast models such as `Regression4TS`.
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
/** Test the actual response vector vs. forecasted matrix, returning the QoF
 *  for all forecasting horizons 1 to h.
 *  @param mod  the fittable model (one that extends `Fit`)
 *  @param y    the orginal actual response vector
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

