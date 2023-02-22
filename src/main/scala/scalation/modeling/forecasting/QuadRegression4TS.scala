
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue Feb 22 23:14:31 EST 202
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: Quadratic Regression for Time Series
 */

package scalation
package modeling
package forecasting

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuadRegression4TS` class supports quadratic regression for Time Series data.
 *  Given a response vector y, a predictor matrix x is built that consists of
 *  lagged y vectors,
 *      y_t = b dot x
 *  where x = [y_{t-1}, y_{t-2}, ... y_{t-lag}].
 *  @param x       the input/predictor matrix built out of lags of y
 *  @param yy      the output/response vector trimmed to match x.dim
 *  @param lag     the maximum lag included (inclusive)
 *  @param hparam  the hyper-parameters ((use Regression.hp for default)
 */
class QuadRegression4TS (x: MatrixD, yy: VectorD, lag: Int, fname: Array [String] = null,
                         hparam: HyperParameter = Regression.hp)
      extends Regression (x, yy, fname, hparam):

    private val debug   = debugf ("QuadRegression4TS", true)              // debug function
    private val flaw    = flawf ("QuadRegression4TS")                     // flaw function
    private val MISSING = -0.0                                            // missing value

    modelName = s"QuadRegression4TS$lag"

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast h steps ahead using the recursive method, returning forecasts in
     *  matrix yf with columns: [1-step, 2-steps, ... h-steps].
     *  @param yp  the predicted response vector (horizon 1 forecasts)
     *  @param h   the forecasting horizon
     */
    def forecast (yp: VectorD, h: Int): MatrixD =
        val yf   = new MatrixD (yp.dim, h)                                // matrix to hold forecasts
        yf(?, 0) = yp                                                     // column 0 is predicted values
        for k <- 1 until h do                                             // forecast into future: columns 1 to h-1
            for i <- yf.indices do
                val xi = x(i)
                val yi = yf(i)
                var sum = b(0)
                var l = 0
                for j <- 1 until b.dim-1 by 2 do                          // add terms in an interleaved fashion
                    if j+k+1 < b.dim then
                        sum += b(j) * xi(j+k)                             // linear terms
                        sum += b(j+1) * xi(j+k+1)                         // add quadratic terms
                    else
                        sum += b(j) * yi(l)
                        sum += b(j+1) * yi(l)~^2
                        l += 1
                    end if
                end for
                yf(i, k) = sum                                            // record forecasted value
            end for
        end for
        yf
    end forecast

end QuadRegression4TS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QuadRegression4TS companion object provides a factory function.
 */
object QuadRegression4TS:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `QuadRegression4TS` object that uses multiple regression to fit a quadratic
     *  surface to Time Series data.
     *  @param y       the original un-expanded output/response vector
     *  @param lag     the maximum lag included (inclusive)
     *  @param hparam  the hyper-parameters ((use Regression.hp for default)
     */
    def apply (y: VectorD, lag: Int, 
               hparam: HyperParameter = Regression.hp): QuadRegression4TS =
        var (x, yy) = buildMatrix4TS (y, lag)                             // column for each lag
        val xx = new MatrixD (x.dim, 2*x.dim2+1) 
        xx(?, 0) = VectorD.one (yy.dim)                                   // add first column of all ones
        for j <- x.indices2 do                                            // add terms in an interleaved fashion
            xx(?, 2*j+1) = x(?, j)                                        // linear terms
            xx(?, 2*j+2) = x(?, j)~^2                                     // add quadratic terms
        end for

        println (s"apply: xx = $xx \n yy = $yy")
        new QuadRegression4TS (xx, yy, lag, null, hparam)
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `QuadRegression4TS` object from a response vector.  The input/data matrix
     *  x is formed from the lagged y vectors as columns in matrix x.
     *  In addition, lagged exogenous variables will be added.
     *  @parax ex      the input matrix of exogenous variable
     *  @param y       the original un-expanded output/response vector
     *  @param lag     the maximum lag included (inclusive)
     *  @param hparam  the hyper-parameters (use Regression.hp for default)
     */
    def apply (ex: MatrixD, y: VectorD, lag: Int,
               hparam: HyperParameter): QuadRegression4TS =
        null                                                                // FIX - implement
    end apply

end QuadRegression4TS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `quadRegression4TSTest` main function tests the `QuadRegression4TS` class.
 *  This test is used to CHECK that the buildMatrix4TS function is working correctly.
 *  May get NaN for some maximum lags (p) due to multi-collinearity.
 *  > runMain scalation.modeling.forecasting.quadRegression4TSTest
 */
@main def quadRegression4TSTest (): Unit =

    val m = 30
    val y = VectorD.range (1, m)                                       // used to CHECK the buildMatrix4TS function

    for p <- 1 to 10 do                                                // autoregressive hyper-parameter p
        banner (s"Test: QuadRegression4TS with $p lags")
        val mod = QuadRegression4TS (y, p)                             // create model for time series data
        mod.trainNtest ()()                                            // train the model on full dataset
        println (mod.summary)

        val yp = mod.predict (mod.getX)
        new Plot (null, mod.getY, yp, s"y vs. yp for ${mod.modelName} with $p lags", lines = true)
    end for

end quadRegression4TSTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `quadRegression4TSTest2` main function tests the `QuadRegression4TS` class on real data:
 *  Forecasting lake levels.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.quadRegression4TSTest2
 */
@main def quadRegression4TSTest2 (): Unit =

    import Example_LakeLevels.y
    val m = y.dim
    val h = 3                                                          // the forecasting horizon

    for p <- 1 to 8 do                                                 // autoregressive hyper-parameter p
        banner (s"Test: QuadRegression4TS with $p lags")
        val mod = QuadRegression4TS (y, p)                             // create model for time series data
        mod.trainNtest ()()                                            // train the model on full dataset
        println (mod.summary)                                          // parameter/coefficient statistics

        banner ("Predictions")
        val yy = mod.getY                                              // trimmed actual response vector
        val yp = mod.predict (mod.getX)                                // predicted response vector
        new Plot (null, mod.getY, yp, s"y vs. yp for ${mod.modelName} with $p lags", lines = true)
        println (s"yp = $yp")

        banner ("Forecasts")
        val yf = mod.forecast (yp, h)                                  // forecasted response matrix
        for k <- yf.indices2 do
            new Plot (null, yy, yf(?, k), s"yy vs. yf_$k for ${mod.modelName} with $p lags", lines = true)
        end for
        println (s"yf = $yf")
        println (s"yf.dims = ${yf.dims}")
        assert (yf(?, 0) == yp)                                        // first forecast = predicted values

        banner ("Forecast QoF")
        println (testForecast (mod, y, yf, p))                         // QoF
//      println (Fit.fitMap (mod.testf (k, y)))                        // evaluate k-units ahead forecasts
    end for

end quadRegression4TSTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `quadRegression4TSTest3` main function tests the `QuadRegression4TS` class on real data:
 *  Forecasting COVID-19.
 *  > runMain scalation.modeling.forecasting.quadRregression4TSTest3
 */
@main def quadRegression4TSTest3 (): Unit =

    import ARMA.hp

    val data = MatrixD.load ("covid_19.csv", 1, 1)                     // skip first row (header) and first column
    val yy   = data(?, 4)                                              // column 4 is daily deaths
//  val yy   = data(?, 5)                                              // column 5 is daily deaths smoothed
    val is   = yy.indexWhere (_ >= 2.0)                                // find day of first death with at least 2 deaths
    println (s"is = $is is first day with at least 2 deaths")
    val y    = yy(is until yy.dim)                                     // slice out days before is

/*
    val h = 1                                                          // forecasting horizon
    for p <- 1 to 19 do                                                // number of lags
        val mod = QuadRegression4TS (y, p)                                 // create model for time series data
        mod.trainNtest ()()                                            // train the model on full dataset
        println (mod.summary)                                          // parameter/coefficient statistics

        banner ("Predictions")
        val yy = mod.getY                                              // trimmed actual response vector
        val yp = mod.predict (mod.getX)                                // predicted response vector
        new Plot (null, mod.getY, yp, s"y vs. yp for ${mod.modelName} with $p lags", lines = true)
//      println (s"yp = $yp")
    end for
*/

    val mod = QuadRegression4TS (y, 35)                                // create model for time series data
    mod.trainNtest ()()                                                // train the model on full dataset
    for tech <- SelectionTech.values do
        banner (s"Feature Selection Technique: $tech")
        val (cols, rSq) = mod.stepRegressionAll (cross = false)        // R^2, R^2 bar
//      val (cols, rSq) = mod.selectFeatures (tech)                    // R^2, R^2 bar, R^2 cv
        val k = cols.size
        println (s"k = $k, n = ${mod.getX.dim2}")
        new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "R^2 cv"),
                   s"R^2 vs n for Regression with $tech", lines = true)
        banner ("Feature Importance")
        println (s"$tech: rSq = $rSq")
        val imp = mod.importance (cols.toArray, rSq)
//      for (c, r) <- imp do println (s"col = $c, \t ${ox_fname(c)}, \t importance = $r")
    end for

end quadRegression4TSTest3

