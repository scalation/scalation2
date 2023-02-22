
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Feb 13 16:22:21 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: RegressionMV for Time Series
 */

package scalation
package modeling
package forecasting

import scalation.mathstat._

import neuralnet.RegressionMV

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionMV4TS` object supports regression for Time Series data.
 *  Given a response vector y, and a predictor matrix x is built that consists of
 *  lagged y vectors.   Additional future response vectors are built for training.
 *      y_t = b dot x
 *  where x = [y_{t-1}, y_{t-2}, ... y_{t-lag}].
 */
object RegressionMV4TS:

    private val debug   = debugf ("RegressionMV4TS", true)                // debug function
    private val flaw    = flawf ("RegressionMV4TS")                       // flaw function
    private val MISSING = -0.0                                            // missing value

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `RegressionMV` object from a response vector.  The input/data matrix
     *  x is formed from the lagged y vectors as columns in matix x.
     *  @param y          the original un-expanded output/response vector
     *  @param lag        the maximum lag included (inclusive)
     *  @param h          the forecasting horizon (1, 2, ... h)
     *  @param intercept  whether to add a column of all ones to the matrix (intercept)
     *  @param hparam     the hyper-parameters (use Regression.hp for default)
     */
    def apply (y: VectorD, lag: Int, h: Int, intercept: Boolean = true,
               hparam: HyperParameter = Regression.hp): RegressionMV =
        var (x, yy) = buildMatrix (y, lag, h)                             // column for each lag
        if intercept then x = VectorD.one (yy.dim) +^: x                  // add first column of all ones

        val mod = new RegressionMV (x, yy, null, hparam)
        mod.modelName = s"RegressionMV4TS_$lag"
        mod
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `RegressionMV` object that uses multiple regression to fit a quadratic
     *  surface to Time Series data.
     *  @param y          the original un-expanded output/response vector
     *  @param lag        the maximum lag included (inclusive)
     *  @param h          the forecasting horizon (1, 2, ... h)
     *  @param intercept  whether to add a column of all ones to the matrix (intercept)
     *  @param hparam     the hyper-parameters ((use Regression.hp for default)
     */
    def quadratic (y: VectorD, lag: Int, h: Int, intercept: Boolean = true,
                   hparam: HyperParameter = Regression.hp): RegressionMV =
        var (x, yy) = buildMatrix (y, lag, h)                             // column for each lag
        x = x ++^ x~^2                                                    // add quadratic terms
        if intercept then x = VectorD.one (yy.dim) +^: x                  // add first column of all ones

        val mod = new RegressionMV (x, yy, null, hparam)
        mod.modelName = s"RegressionMV4TS.quadratic$lag"
        mod
    end quadratic

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a response vector y, build and return
     *  (1) an input/predictor matrix xx and
     *  (2) an output/multi-horizon output/response matrix yy.
     *  The first lag responses can't be predicted due to missing past values.
     *  The last h-1 responses can't be predicted due to missing future values.
     *  Therefore the number of rows in xx and yy is reduced to y.dim + 1 - lag - h.
     *  @param y    the given output/response vector
     *  @param lag  the maximum lag included (inclusive)
     *  @param h    the forecasting horizon (1, 2, ... h)
     */
    def buildMatrix (y: VectorD, lag: Int, h: Int = 1): (MatrixD, MatrixD) =
        val xx = new MatrixD (y.dim + 1 - lag - h, lag)
        val yy = new MatrixD (y.dim + 1 - lag - h, h)
        for i <- lag to y.dim - h do
            for j <- xx.indices2 do xx(i-lag, lag - 1 - j) = y(i - 1 - j)
            for j <- yy.indices2 do yy(i-lag, j) = if i + j >= y.dim then -0.0 else y(i + j)
        end for
//      debug ("buildMatrix", s" xx = $xx \n yy = $yy")
        (xx, yy)
    end buildMatrix

end RegressionMV4TS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `regressionMV4TSTest` main function tests the `RegressionMV4TS` class.
 *  This test is used to CHECK that the buildMatrix function is working correctly.
 *  May get NaN for some maximum lags (p) due to multi-collinearity.
 *  > runMain scalation.modeling.forecasting.regressionMV4TSTest
 */
@main def regressionMV4TSTest (): Unit =

    val m = 30
    val y = VectorD.range (1, m)                                       // used to CHECK the buildMatrix function
    val h = 3                                                          // the forecasting horizon

    for p <- 5 to 5 do                                                 // autoregressive hyper-parameter p
        banner (s"Test: RegressionMV4TS with $p lags")
        val mod = RegressionMV4TS (y, p, h)                            // create model for time series data
        mod.trainNtest ()()                                            // train the model on full dataset
        println (mod.summary)

        val yy = mod.getY
        val yp = mod.predict (mod.getX)
        for j <- yp.indices2 do
            new Plot (null, yy(?, j), yp(?, j), s"yy_$j vs. yp_$j for ${mod.modelName} with $p lags", lines = true)
        end for
    end for

end regressionMV4TSTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `regressionMV4TSTest2` main function tests the `RegressionMV4TS` class on real data:
 *  Forecasting lake levels.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.regressionMV4TSTest2
 */
@main def regressionMV4TSTest2 (): Unit =

    import Example_LakeLevels.y
    val m = y.dim
    val h = 3                                                          // the forecasting horizon

    for p <- 1 to 10 do                                                // autoregressive hyper-parameter p
        banner (s"Test: RegressionMV4TS with $p lags")
        val mod = RegressionMV4TS (y, p, h)                            // create model for time series data
        mod.trainNtest ()()                                            // train the model on full dataset
        println (mod.summary)

        banner ("Predictions/Forecasts")                               // direct forecasting technique
        val yy = mod.getY
        val yf = mod.predict (mod.getX)
        for k <- yf.indices2 do
            new Plot (null, yy(?, k), yf(?, k), s"yy_$k vs. yf_$k for ${mod.modelName} with $p lags", lines = true)
        end for
        println (s"yf = $yf")
        println (s"yf.dims = ${yf.dims}")
    end for

end regressionMV4TSTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `regressionMV4TSTest3` main function tests the `RegressionMV4TS` class on real data:
 *  Forecasting lake levels.  Uses quadratic regression.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.regressionMV4TSTest3
 */
@main def regressionMV4TSTest3 (): Unit =

    import Example_LakeLevels.y
    val m = y.dim
    val h = 3                                                          // the forecasting horizon

    for p <- 1 to 10 do                                                // autoregressive hyper-parameter p
        banner (s"Test: RegressionMV4TS with $p lags")
        val mod = RegressionMV4TS.quadratic (y, p, h)                  // create model for time series data
        mod.trainNtest ()()                                            // train the model on full dataset
        println (mod.summary)

        val yy = mod.getY
        val yf = mod.predict (mod.getX)
        for k <- yf.indices2 do
            new Plot (null, yy(?, k), yf(?, k), s"yy_$k vs. yf_$k for ${mod.modelName} with $p lags", lines = true)
        end for
        println (s"yf = $yf")
        println (s"yf.dims = ${yf.dims}")

    end for

end regressionMV4TSTest3

