
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John A. Miller
 *  @version 2.0
 *  @date    Fri Oct 27 14:37:32 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Framework: Positional Encoding
 *
 *  @see https://towardsdatascience.com/master-positional-encoding-part-i-63c05d90a0c3
 *  @see https://arxiv.org/pdf/2305.16642.pdf
 *  @see https://www.arxiv-vanity.com/papers/2202.07125/
 *
 *  Alternatives:
 *      1) Learnable Positional Encoding
 *      2) Relative Positional Encoding
 *      3) Timestamp Encoding - treat as features
 */

package scalation
package modeling
package forecasting
package neuralforecasting

import scala.math.{cos, sin}

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PositionalEnc` trait provides methods to convert a time t into an encoded vector.
 *  An encoded vector consists of numbers in [-1.0, 1.0].
 *  It implements Absolute Fixed Vanilla Positional Encoding.
 *  @param m  the length of the time series (number of time points)
 *  @param d  the dimensionality of the positional encoding (except for f0)
 */
trait PositionalEnc (m: Int, d: Int = 16):

    private val u = for j <- 0 until d/2 yield 1E-4~^(2*j/d.toDouble)
    private val w = for j <- 0 until d/2 yield u(j) * d / m.toDouble

    println (s"sinusoidal:      u = $u")
    println (s"length adjusted: w = $w")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert position/time t to an encoded vector (of length one) using Naive Positional
     *  Encoding.  Also assumes t < m.
     *  @param t  the position/time-index to be encoded
     */
    def f0 (t: Int): VectorD =
        val z = t / m.toDouble
        VectorD (2 * z - 1)
    end f0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert position/time t to an encoded vector using Sinusoidal Positional Encoding.
     *  Note: works better for larger values of d as is common for NLP.
     *  @param t  the position/time-index to be encoded
     */
    def f1 (t: Int): VectorD =
        val p_t = new VectorD (d)
        for k <- 0 until d by 2 do
            val w_   = u(k/2)
            p_t(k)   = sin (w_ * t)
            p_t(k+1) = cos (w_ * t)
        p_t
    end f1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert position/time t to an encoded vector using Length-Adjusted Sinusoidal
     *  Positional Encoding.  Suggested for smaller values of d as is common for time series.
     *  @param t  the position/time-index to be encoded
     */
    def f2 (t: Double): VectorD =
        val p_t = new VectorD (d)
        for k <- 0 until d by 2 do
            val w_   = w(k/2)
            p_t(k)   = sin (w_ * t)
            p_t(k+1) = cos (w_ * t)
        p_t
    end f2

end PositionalEnc


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `positionalEncTest` main function is used to test the `PositionalEnc` class.
 *  > runMain scalation.modeling.forecasting.neuralforecasting.positionalEncTest
 */
@main def positionalEncTest (): Unit =

    val m = 20                                                // length of the time series

    object pe extends PositionalEnc (m)

    banner ("Naive Positional Encoding")
    for t <- 0 until m do println (s"t = $t, pe.f0(t) = ${pe.f0(t)}")

    banner ("Sinusoidal Positional Encoding")
    for t <- 0 until m do println (s"t = $t, pe.f1(t) = ${pe.f1(t)}")

    banner ("Length-Adjusted Sinusoidal Positional Encoding")
    for t <- 0 until m do println (s"t = $t, pe.f2(t) = ${pe.f2(t)}")

end positionalEncTest

