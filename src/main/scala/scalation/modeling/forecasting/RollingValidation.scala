
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun May 29 13:45:56 EDT 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Framework: Rolling Validation for Forecasters
 */

package scalation
package modeling
package forecasting

import scala.math.max

import scalation.mathstat._

import Fit._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RollingValidation` object provides rolling-validation, where a full
 *  dataset is divided into a training set followed by a testing set.
 *  Retraining is done as the algorithm rolls through the testing set making
 *  out-of-sample predictions/forecasts to keep the parameters from becoming stale.
 *  For example, with TR_RATIO = 0.5 and m = 1000 it works as follows:
 *      tr(ain) 0 to 499, te(st) 500 to 999  
 *  Re-training occurs according to the retraining cycle rc, e.g., rc = 10
 *  implies that retraining would occurs after every 10 forecasts or 50 times
 *  for this example.
 */
object RollingValidation:

    private val debug    = debugf ("RollingValidation", true)             // debug function
    private val TR_RATIO = 0.5                                            // ratio training set to full datasets

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the size (number of instances) for a training dataset.
     *  @param m  the size of the full dataset
     */
    def trSize (m: Int): Int = (m * TR_RATIO).toInt

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Align the actual response vector for comparison with the predicted/forecasted
     *  response vector, returning a time vector and sliced response vector.
     *  @param tr_size  the size of the intial training set 
     *  @param y        the actual response for the full dataset (to be sliced)
     */
    def align (tr_size: Int, y: VectorD): (VectorD, VectorD) =
        (VectorD.range (tr_size, y.dim), y(tr_size until y.dim))
    end align

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use rolling-validation to compute test Quality of Fit (QoF) measures
     *  by dividing the dataset into a TESTING SET (tr) and a TRAINING SET (te)
     *  as follows:  [ <-- tr_size --> | <-- te_size --> ]
     *  This version calls predict for one-step ahead out-of-sample forecasts.
     *  @param mod  the forecasting model being used (e.g., `ARIMA`)
     *  @param rc   the retraining cycle (number of forecasts until retraining occurs)
     */
    def rollValidate (mod: Forecaster & Fit, rc: Int): Unit =
        val y       = mod.getY                                            // get (expanded) response/output vector
        val tr_size = trSize (y.dim)                                      // size of initial training set
        val te_size = y.dim - tr_size                                     // size of testing set
        debug ("rollValidate", s"train: tr_size = $tr_size; test: te_size = $te_size, rc = $rc")

        val yp = new VectorD (te_size)                                    // y-predicted over testing set
        for i <- 0 until te_size do                                       // iterate through testing set
            val t = tr_size + i                                           // next time point to forecast
            if i % rc == 0 then mod.train (null, y(0 until t))            // retrain on sliding training set
            yp(i) = mod.predict (t-1, y)                                  // predict the next value
        end for

        val (t, yy) = align (tr_size, y)                                  // align vectors
        val df = max (0, mod.parameter.size - 1)                          // degrees of freedom for model
        mod.resetDF (df, te_size - df)                                    // reset degrees of freedom
        new Plot (t, yy, yp, "Plot yy, yp vs. t", lines = true)
        println (FitM.fitMap (mod.diagnose (yy, yp), QoF.values.map (_.toString)))
    end rollValidate

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Use rolling-validation to compute test Quality of Fit (QoF) measures
     *  by dividing the dataset into a TESTING SET (tr) and a TRAINING SET (te).
     *  as follows:  [ <-- tr_size --> | <-- te_size --> ]
     *  This version calls forecast for h-steps ahead out-of-sample forecasts.
     *  @param mod  the forecasting model being used (e.g., `ARIMA`)
     *  @param rc   the retraining cycle (number of forecasts until retraining occurs)
     *  @param h    the forecasting horizon (h-steps ahead)
     */
    def rollValidate (mod: Forecaster & Fit, rc: Int, h: Int): Unit =
        val y       = mod.getY                                            // get (expanded) response/output vector
        val yf      = mod.forecastAll (y, h)                              // get in-sample forecasting matrix
        val tr_size = trSize (y.dim)                                      // size of initial training set
        val te_size = y.dim - tr_size                                     // size of testing set
        debug ("rollValidate", s"train: tr_size = $tr_size; test: te_size = $te_size, rc = $rc")

        val yp = new VectorD (te_size)                                    // y-predicted over testing set (only for h=1)
        for i <- 0 until te_size do                                       // iterate through testing set
            val t = tr_size + i                                           // next time point to forecast
            if i % rc == 0 then mod.train (null, y(0 until t))            // retrain on sliding training set
            yp(i)  = mod.predict (t-1, y)                                 // predict the next value (only for h=1)
            val yd = mod.forecast (t-1, yf, y, h)                         // forecast the next h-values
                                                                          // yf is updated down its diagonals
            assert (yp(i) =~ yd(0))                                       // make sure h=1 forecasts agree with predictions
        end for                                                           // yf is updated down its diagonals

        val (t, yy) = align (tr_size, y)                                  // align vectors
        val df = max (0, mod.parameter.size - 1)                          // degrees of freedom for model
        mod.resetDF (df, te_size - df)                                    // reset degrees of freedom
        new Plot (t, yy, yp, "Plot yy, yp vs. t", lines = true)

        for k <- 1 to h do
            val yfh = yf(tr_size until y.dim, k)
            new Plot (t, yy, yfh, s"Plot yy, yfh vs. t (h = $k)", lines = true)
            banner (s"rollValidate: for horizon h = $k:")
            println (FitM.fitMap (mod.diagnose (yy, yfh), QoF.values.map (_.toString)))
        end for
    end rollValidate
    
    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test assessment and validation for the given forecasting model:
     *  (1) in-sample assessment on full dataset
     *  (2) out-of-sample validation using rolling validation with predict (one-step)
     *  (3) out-of-sample validation using rolling validation with forecast (h-steps)
     *  @param mod  the forecasting model to test (e.g., `ARIMA`)
     *  @param rc   the retraining cycle (number of forecasting until retraining occurs)
     *  @param h    the forecasting horizon (h-steps ahead)
     */
    def testValidate (mod: Forecaster & Fit, rc: Int, h: Int): Unit =
        banner (s"testValidate: in-sample on full dataset for ${mod.modelName}")
        val (yp, qof) = mod.trainNtest ()()

        banner (s"testValidate: out-of-sample predict rolling validation for ${mod.modelName}")
        RollingValidation.rollValidate (mod, rc)

        banner (s"testValidate: out-of-sample forecast (h=1..$h) rolling validation for ${mod.modelName}")
        RollingValidation.rollValidate (mod, rc, h)
    end testValidate

end RollingValidation

/*
        FIX - Ideas for reducing the size of yf matrix
        val cp = mod.cap                                                  // maximum lag (how far into the past)
        val st = te_size - cp                                             // size of shift from original y
        val yf = new MatrixD (te_size+cp+h, h+2)                          // extend before and after
        val yf = new MatrixD (y.dim+h, h+2)                               // extend before and after
        for t <- 0 until te_size + cp do yf(t, 0) = y(st+t)               // first column is the time-step (e.g., logical day)
        for t <- yf.indices do yf(t, h+1) = te_size + t                   // last column is time (logical day)
        for t <- yf.indices do yf(t, h+1) = t                             // last column is time (logical day)
*/


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `rollingValidationTest` main function is used to test the rollValidate method
 *  in the `RollingValidation` object.
 *  > runMain scalation.modeling.forecasting.rollingValidationTest
 */
@main def rollingValidationTest (): Unit =

    import scalation.random.Normal

    val m = 1200                                                          // number of instances
    val y = new VectorD (m)                                               // response/output vector
    val e = Normal (0, 100)                                               // noise

    y(0) = 50.0
    for i <- 1 until y.dim do y(i) = 0.8 * y(i-1) + e.gen

    val p = 3                                                             // order of the model
    val h = 2                                                             // forecasting horizon, try changing
    val rc = 2                                                            // retrain cycle

    println (s"y.min = ${y.min}, y.max = ${y.max}")

    banner (s"AR($p) full dataset results at forecasting horizon h = $h")

    SARIMAX.hp("p") = p
//  val mod = new AR (y)                                                  // create an AR(p) model
    val mod = new ARMA (y)                                                // create an ARMA(p, 0) model
    val (yp, qof) = mod.trainNtest ()()                                   // train-test model on full dataset

    banner (s"AR($p) one-step ahead rolling validation results")
    RollingValidation.rollValidate (mod, rc)

    banner (s"AR($p) $h-steps rolling validation results")
    RollingValidation.rollValidate (mod, rc, h)

end rollingValidationTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `rollingValidationTest2` main function is used to test the rollValidate method
 *  in the `RollingValidation` object.
 *  > runMain scalation.modeling.forecasting.rollingValidationTest2
 */
@main def rollingValidationTest2 (): Unit =

    import Example_LakeLevels.y

    val p = 3                                                             // order of the model
    val h = 2                                                             // forecasting horizon, try changing
    val rc = 2                                                            // retrain cycle

    println (s"y.min = ${y.min}, y.max = ${y.max}")

    banner (s"AR($p) full dataset results at forecasting horizon h = $h")

    SARIMAX.hp("p") = p
//  val mod = new AR (y)                                                  // create an AR(p) model
    val mod = new ARMA (y)                                                // create an ARMA(p, 0) model
    val (yp, qof) = mod.trainNtest ()()                                   // train-test model on full dataset

    val t = VectorD.range (49 until 97)                                   // note original y must be shifted
    new Plot (t, y(50 until 98), yp(49 until 97), "y, yp vs t 2nd half", lines = true)

    banner (s"AR($p) one-step ahead rolling validation results")
    RollingValidation.rollValidate (mod, rc)

    banner (s"AR($p) $h-steps rolling validation results")
    RollingValidation.rollValidate (mod, rc, h)

end rollingValidationTest2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `rollingValidationTest3` main function is used to test the rollValidate method
 *  in the `RollingValidation` object.
 *  Compares baseline models on in-sample and out-of-sample assessment.
 *  > runMain scalation.modeling.forecasting.rollingValidationTest3
 */
@main def rollingValidationTest3 (): Unit =

    import Example_LakeLevels.y

    val h = 2                                                             // forecasting horizon, try changing
    val rc = 2                                                            // retrain cycle

    RollingValidation.testValidate (new RandomWalk (y), rc, h)

    RollingValidation.testValidate (new NullModel (y), rc, h)

    RollingValidation.testValidate (new TrendModel (y), rc, h)

end rollingValidationTest3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `rollingValidationTest4` main function is used to test the rollValidate method
 *  in the `RollingValidation` object.
 *  Random Walk is used to make structure of the yf matrix clear.
 *  > runMain scalation.modeling.forecasting.rollingValidationTest4
 */
@main def rollingValidationTest4 (): Unit =

    val y = VectorD.range (1, 25)

    val h = 2                                                             // forecasting horizon, try changing
    banner (s"RW full dataset results at forecasting horizon h = $h")           
    val mod = new RandomWalk (y)                                          // create an RW model
    mod.train (null, y)                                                   // train the model on full dataset
    
    val (yp, qof) = mod.test (null, y)                                    // test the model on full dataset
    println (mod.report (qof))                                            // report on Quality of Fit (QoF)
    println (s"yp = $yp")                                                 // print prediction matrix

    val yf = mod.forecastAll (y, h)                                       // produce all forecasts up horizon h
    println (s"yf = $yf")                                                 // print forecast matrix

    val rc = 2                                                            // retrain cycle
    banner ("RW one-step ahead rolling validation results")
    RollingValidation.rollValidate (mod, rc)

    banner (s"RW $h-steps rolling validation results")
    RollingValidation.rollValidate (mod, rc, h)

end rollingValidationTest4

