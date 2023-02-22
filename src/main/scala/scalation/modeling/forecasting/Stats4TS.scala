
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Thu May 26 13:00:49 EDT 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model Framework: Basic Statistics for Time-Series
 */

package scalation
package modeling
package forecasting

import scalation.mathstat.{Plot, VectorD}
import scalation.random._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Stats` case class is used to hold basic statistical information:
 *  mean, variance, auto-covariance, and auto-correlation.
 *  Note gamma0 (biased) does not equal the sample variance (unbiased)
 *  @param y     the response vector (time-series data) for the training/full dataset
 *  @param lags  the maximum number of lags
 */
case class Stats4TS (y: VectorD, lags: Int):

   val mu   = y.mean                                 // sample mean
   val sig2 = y.variance                             // sample variance
   val acv  = new VectorD (lags + 1)                 // auto-covariance vector
   for k <- acv.indices do acv(k) = y acov k         // k-th lag auto-covariance
   val acr  = acv / acv(0)                           // auto-correlation function

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
   /** Convert a `Stats4TS` object to a string.
    */
   override def toString: String =
       s"Stats (m = ${y.dim}, mu = $mu, sig2 = $sig2, acr = $acr)"
   end toString

end Stats4TS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `stats4TSTest` main function tests the `Stats4TS` class on the Lake Levels Dataset.
 *  > runMain scalation.modeling.forecasting.stats4TSTest
 */
@main def stats4TSTest (): Unit =

    import Example_LakeLevels.y

    banner ("Test Stats4TS on Lake Levels Dataset")
    val stats = Stats4TS (y, MAX_LAGS)
    println (stats)
    val zero = new VectorD (stats.acr.dim)
    new Plot (null, stats.acr, zero, "ACF vs. k", true)

end stats4TSTest

