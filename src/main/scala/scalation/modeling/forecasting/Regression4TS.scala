
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue Feb 22 23:14:31 EST 202
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: Regression for Time Series
 */

package scalation
package modeling
package forecasting

import scala.math.max

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression4TS` class supports regression for Time Series data.
 *  Given a response vector y, and a predictor matrix x is built that consists of
 *  lagged y vectors,
 *      y_t = b dot x
 *  where x = [y_{t-1}, y_{t-2}, ... y_{t-lag}].
 *  @param x       the input/predictor matrix built out of lags of y
 *  @param yy      the output/response vector trimmed to match x.dim
 *  @param lag     the maximum lag included (inclusive)
 *  @param hparam  the hyper-parameters ((use Regression.hp for default)
 */
class Regression4TS (x: MatrixD, yy: VectorD, lag: Int, fname: Array [String] = null,
                     hparam: HyperParameter = Regression.hp)
      extends Regression (x, yy, fname, hparam):

    private val debug   = debugf ("Regression4TS", true)                  // debug function
    private val flaw    = flawf ("Regression4TS")                         // flaw function
    private val MISSING = -0.0                                            // missing value

    modelName = s"Regression4TS_$lag"

// FIX - add methods similar to those in Forecaster - may need another trait

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  e.g., (b_0, b_1, b_2) dot (1, z_1, z_2).
     *  Must override when using transformations, e.g., `ExpRegression`.
     *  @param z  the new vector to predict
     */
//  override def predict (z: VectorD): Double = b dot z                   // allows negative values
    override def predict (z: VectorD): Double = max (0.0, b dot z)        // must be at least zero

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of vector y = f(x_, b), e.g., x_ * b for `Regression`.
     *  @param x_  the matrix to use for making predictions, one for each row
     */
    override def predict (x_ : MatrixD): VectorD =
        VectorD (for i <- x_.indices yield predict (x_(i)))
    end predict

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forecast h steps ahead using the recursive method, returning forecasts in
     *  matrix yf with columns: [1-step, 2-steps, ... h-steps].
     *  @param yp  the predicted response vector (horizon 1 forecasts)
     *  @param h   the forecasting horizon
     */
    def forecast (yp: VectorD, h: Int): MatrixD =
        val b_ = b(1 until b.dim)                                         // paramters excluding intercept

        val yf   = new MatrixD (yp.dim, h)                                // matrix to hold forecasts
        yf(?, 0) = yp                                                     // column 0 is predicted values
        for k <- 1 until h do                                             // forecast into future: columns 1 to h-1
            for i <- yf.indices do
                val xy = x(i)(k+1 until x.dim2) ++ yf(i)(0 until k)       // last from x ++ first from yf
//              println (s"xy = $xy")
                yf(i, k) = b(0) + (b_ dot xy)                             // record forecasted value
            end for
        end for
        yf
    end forecast

end Regression4TS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Regression4TS companion object provides a factory function.
 */
object Regression4TS:

    private val flaw = flawf ("Regression4TS")                            // flaw function

    private val TREND = false                                             // include quadratic trend
    private val DAY   = true                                              // include day of the week effect

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Regression4TS` object from a response vector.  The input/data matrix
     *  x is formed from the lagged y vectors as columns in matrix x.
     *  @param y       the original un-expanded output/response vector
     *  @param lag     the maximum lag included (inclusive)
     *  @param hparam  the hyper-parameters (use Regression.hp for default)
     */
    def apply (y: VectorD, lag: Int,
               hparam: HyperParameter = Regression.hp): Regression4TS =
        var (x, yy) = buildMatrix4TS (y, lag)                             // column for each lag
        x = VectorD.one (yy.dim) +^: x                                    // add first column of all ones
        if TREND then
            x = VectorD.range (0, yy.dim) +^: x                           // add trend/time
            x = VectorD.range (0, yy.dim)~^2 +^: x                        // add quadratic trend/time
        end if
        if DAY then
            val day = VectorI (for t <- yy.indices yield t % 7)
            x = day.toDouble +^: x                                        // add DAY of week as ordinal var

//          val dum = Variable.dummyVars (day)
//          x = x ++^ dum                                                 // add DAY of week as dummy vars
        end if

//      println (s"apply: x = $x \n yy = $yy")
        new Regression4TS (x, yy, lag, null, hparam)
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `Regression4TS` object from a response vector.  The input/data matrix
     *  x is formed from the lagged y vectors as columns in matrix x.
     *  In addition, lagged exogenous variables are added.
     *  @param y       the original un-expanded output/response vector
     *  @param lag     the maximum lag included (inclusive)
     *  @parax ex      the input matrix for 1st exogenous variable
     *  @parax ex2     the input matrix for 2nd exogenous variable (optional)
     *  @parax ex3     the input matrix for 3rd exogenous variable (optional)
     *  @param hparam  the hyper-parameters (use Regression.hp for default)
     *  @param elag1   the minimum exo lag included (inclusive)
     *  @param elag2   the maximum exo lag included (inclusive)
     */
    def exo (y: VectorD, lag: Int, ex: VectorD, ex2: VectorD = null, ex3: VectorD = null,
             hparam: HyperParameter = Regression.hp)
            (elag1: Int = max (1, lag / 5),
             elag2: Int = max (1, lag)): Regression4TS =
        var (x, yy) = buildMatrix4TS (y, lag)                             // column for each lag
        x = VectorD.one (yy.dim) +^: x                                    // add first column of all ones
        var xx = buildMatrix4TS_exo (ex, lag, elag1, elag2)
        x = x ++^ xx                                                      // add columns for 1st lagged exo var
        if ex2 != null then
           val xx2 = buildMatrix4TS_exo (ex2, lag, elag1, elag2)
           x = x ++^ xx2                                                  // add columns for 2nd lagged exo var
        end if
        if ex3 != null then
           val xx3 = buildMatrix4TS_exo (ex3, lag, elag1, elag2)
           x = x ++^ xx3                                                  // add columns for 2nd lagged exo var
        end if
        if TREND then
            x = VectorD.range (0, yy.dim) +^: x                           // add trend/time
            x = VectorD.range (0, yy.dim)~^2 +^: x                        // add quadratic trend/time
        end if
        if DAY then
            val day = VectorI (for t <- yy.indices yield t % 7)
            val dum = Variable.dummyVars (day)
            x = x ++^ dum                                                 // add DAY of week as dummy vars
        end if

        println (s"exo: x.dims = ${x.dims} \n yy.dim = ${yy.dim}")
//      println (s"exo: x = $x \n yy = $yy")
        new Regression4TS (x, yy, lag, null, hparam)
    end exo

end Regression4TS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `regression4TSTest` main function tests the `Regression4TS` class.
 *  This test is used to CHECK that the buildMatrix4TS function is working correctly.
 *  May get NaN for some maximum lags (p) due to multi-collinearity.
 *  > runMain scalation.modeling.forecasting.regression4TSTest
 */
@main def regression4TSTest (): Unit =

    val m = 30
    val y = VectorD.range (1, m)                                       // used to CHECK the buildMatrix4TS function

    for p <- 1 to 10 do                                                // autoregressive hyper-parameter p
        banner (s"Test: Regression4TS with $p lags")
        val mod = Regression4TS (y, p)                                 // create model for time series data
        mod.trainNtest ()()                                            // train the model on full dataset
        println (mod.summary)

        val yp = mod.predict (mod.getX)
        new Plot (null, mod.getY, yp, s"y vs. yp for ${mod.modelName} with $p lags", lines = true)
    end for

end regression4TSTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `regression4TSTest2` main function tests the `Regression4TS` class on real data:
 *  Forecasting lake levels.
 *  @see cran.r-project.org/web/packages/fpp/fpp.pdf
 *  > runMain scalation.modeling.forecasting.regression4TSTest2
 */
@main def regression4TSTest2 (): Unit =

    import Example_LakeLevels.y
    val m = y.dim
    val h = 3                                                          // the forecasting horizon

    for p <- 1 to 8 do                                                 // autoregressive hyper-parameter p
        banner (s"Test: Regression4TS with $p lags")
        val mod = Regression4TS (y, p)                                 // create model for time series data
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

end regression4TSTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `regression4TSTest3` main function tests the `Regression4TS` class on real data:
 *  Forecasting COVID-19.
 *  > runMain scalation.modeling.forecasting.regression4TSTest3
 */
@main def regression4TSTest3 (): Unit =

    import scala.collection.mutable.HashMap

    val header = Array ("total_cases",
                        "new_cases",
                        "new_cases_smoothed",
                        "total_deaths",
                        "new_deaths",
                        "new_deaths_smoothed",
                        "reproduction_rate",
                        "icu_patients",
                        "hosp_patients",
                        "weekly_hosp_admissions",
                        "total_tests",
                        "new_tests",
                        "new_tests_smoothed",
                        "positive_rate",
                        "tests_per_case",
                        "total_vaccinations",
                        "people_vaccinated",
                        "people_fully_vaccinated",
                        "new_vaccinations",
                        "new_vaccinations_smoothed",
                        "new_people_vaccinated_smoothed")

    val col = HashMap [String, Int] ()
    for i <- header.indices do col += header(i) -> i

    val data = MatrixD.load ("covid_19.csv", 1, 1)                     // skip first row (header) and first column
    val yy   = data(?, col("new_deaths"))                              // response column 4 is daily deaths
    val xx   = data(?, col("new_cases"))                               // 1st exogenous var
    val xx3  = data(?, col("positive_rate"))                           // 2nd exogenous var
    val xx2  = data(?, col("new_tests"))                               // 3rd exogenous var
    val is   = yy.indexWhere (_ >= 6.0)                                // find day of first death with at least 6 deaths
    println (s"is = $is is first day with at least 6 deaths")

    val y    = yy(is until yy.dim)                                     // slice out days before is for response var
    val ex   = xx(is until yy.dim)                                     // slice out days before is for 1st exogenous var
    val ex2  = xx2(is until yy.dim)                                    // slice out days before is for 2nd exogenous var
    val ex3  = xx3(is until yy.dim)                                    // slice out days before is for 3rd exogenous var

/*
    val h = 1                                                          // forecasting horizon
    for p <- 1 to 19 do                                                // number of lags
        val mod = Regression4TS (y, p)                                 // create model for time series data
        mod.trainNtest ()()                                            // train the model on full dataset
        println (mod.summary)                                          // parameter/coefficient statistics

        banner ("Predictions")
        val yy = mod.getY                                              // trimmed actual response vector
        val yp = mod.predict (mod.getX)                                // predicted response vector
        new Plot (null, mod.getY, yp, s"y vs. yp for ${mod.modelName} with $p lags", lines = true)
//      println (s"yp = $yp")
    end for
*/

    banner ("Test Regression4TS.exo on COVID-19 Data")
//  val mod = Regression4TS (y, 35)                                    // create model for time series data
    val mod = Regression4TS.exo (y, 35, ex, ex2, ex3)()                // create model for time series data
    val (yp, qof) = mod.trainNtest ()()                                // train the model on full dataset
    new Plot (null, mod.getY, yp, s"${mod.modelName}, y vs. yp", lines = true)

    banner (s"Feature Selection Technique: stepRegression")
    val (cols, rSq) = mod.stepRegressionAll (cross = false)            // R^2, R^2 bar
    val k = cols.size
    println (s"k = $k, n = ${mod.getX.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "R^2 cv"),
               s"R^2 vs n for Regression with tech", lines = true)
    banner ("Feature Importance")
    println (s"tech: rSq = $rSq")
    val imp = mod.importance (cols.toArray, rSq)
//  for (c, r) <- imp do println (s"col = $c, \t ${ox_fname(c)}, \t importance = $r")

end regression4TSTest3

