
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Thu May 26 13:00:49 EDT 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Framework: Basic Statistics for Time-Series
 */

package scalation
package mathstat

import scala.math.min

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Stats` case class is used to hold basic statistical information:
 *  mean, variance, auto-covariance, and auto-correlation.
 *  Note gamma0 (biased) does not equal the sample variance (unbiased)
 *  @param y         the response vector (time-series data) for the training/full dataset
 *  @param lags_     the maximum number of lags
 *  @param adjusted  whether to adjust to account for the number of elements in the sum Σ (or use dim-1)
 *                   @see `VectorD.acov`
 */
case class Stats4TS (y: VectorD, lags_ : Int, adjusted: Boolean = true):

    val lags = min (y.dim-1, lags_)                             // lags can't exceed dataset size
    val mu   = y.mean                                           // sample mean
    val sig2 = y.variance                                       // sample variance
    val acv  = new VectorD (lags + 1)                           // auto-covariance vector
    for k <- acv.indices do acv(k) = y.acov (k, mu, adjusted)   // k-th lag auto-covariance
    val acr  = acv / acv(0)                                     // auto-correlation function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert a `Stats4TS` object to a string.
     */
    override def toString: String =
        s"Stats (m = ${y.dim} \n mu = $mu \n sig2 = $sig2 \n acr = $acr)"
    end toString

end Stats4TS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `stats4TSTest` main function tests the `Stats4TS` class on two simple datasets.
 *  > runMain scalation.mathstat.stats4TSTest
 */
@main def stats4TSTest (): Unit =

    val y = VectorD (1, 2, 5, 8, 3, 6, 9, 4, 5, 11,
                     12, 16, 7, 6, 13, 15, 10, 8, 14, 17)

    println (s"y = $y")

    banner ("Test Stats4TS on vector y")
    var stats = Stats4TS (y, MAX_LAGS)
    println (stats)
    var zero = new VectorD (stats.acr.dim)
    new Plot (null, stats.acr, zero, "y: ACF vs. k", true)

    val z = VectorD (1, 2, 4, 7, 9, 8, 6, 5, 3)

    println (s"z = $z")

    banner ("Test Stats4TS on vector z")
    stats = Stats4TS (z, MAX_LAGS)
    println (stats)
    zero = new VectorD (stats.acr.dim)
    new Plot (null, stats.acr, zero, "z: ACF vs. k", true)

end stats4TSTest

