
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep  4 13:09:52 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model Framework: Context and Attention for Transformers
 *
 *  @see https://sebastianraschka.com/blog/2023/self-attention-from-scratch.html
 */

package scalation
package modeling
package forecasting

import scala.math.sqrt

import scalation.mathstat._
import scalation.random.RandomMatD

import ActivationFun.f_softmax

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Compute the Context Vector.
 *  @param q_t  the query vector at time t (based on input vector x_t)
 *  @param k    the key matrix
 *  @param v    the value matrix
 */
def context (q_t: VectorD, k: MatrixD, v: MatrixD): VectorD =
    val root_n = sqrt (q_t.dim)
    f_softmax.f_ (k * (q_t / root_n)) *: v
end context


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Compute Self-Attention Weight Matrix.
 *  @param q  the query matrix (q_t over all time)
 *  @param k  the key matrix
 *  @param v  the value matrix
 */
def attention (q: MatrixD, k: MatrixD, v: MatrixD): MatrixD =
    val root_n = sqrt (q.dim2)
    f_softmax.fM (q * (k.transpose / root_n)) * v
end attention


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `attentionTest` function tests the `context` and `attention` top-level functions.
 *  Input matrix taking from:
 *  @see https://sebastianraschka.com/blog/2023/self-attention-from-scratch.html
 *  > runMain scalation.modeling.forecasting.attentionTest
 */
@main def attentionTest (): Unit =

    val x = MatrixD ((6, 16), 0.3374, -0.1778, -0.3035, -0.5880,  0.3486,  0.6603, -0.2196, -0.3792,    // row 0
                              0.7671, -1.1925,  0.6984, -1.4097,  0.1794,  1.8951,  0.4954,  0.2692,

                              0.5146,  0.9938, -0.2587, -1.0826, -0.0444,  1.6236, -2.3229,  1.0878,    // row 1
                              0.6716,  0.6933, -0.9487, -0.0765, -0.1526,  0.1167,  0.4403, -1.4465,

                              0.2553, -0.5496,  1.0042,  0.8272, -0.3948,  0.4892, -0.2168, -1.7472,    // row 2
                             -1.6025, -1.0764,  0.9031, -0.7218, -0.5951, -0.7112,  0.6230, -1.3729,

                             -1.3250,  0.1784, -2.1338,  1.0524, -0.3885, -0.9343, -0.4991, -1.0867,    // row 3
                              0.8805,  1.5542,  0.6266, -0.1755,  0.0983, -0.0935,  0.2662, -0.5850,

                             -0.0770, -1.0205, -0.1690,  0.9178,  1.5810,  1.3010,  1.2753, -0.2010,    // row 4
                              0.4965, -1.5723,  0.9666, -1.1481, -1.1589,  0.3255, -0.6315, -2.8400,

                              0.8768,  1.6221, -1.4779,  1.1331, -1.2203,  1.3139,  1.0533,  0.1388,    // row 5
                              2.2473, -0.8036, -0.2808,  0.7697, -0.6596, -0.7979,  0.1838,  0.2293)

    val m   = x.dim                                                       // number of time points
    val n   = x.dim2                                                      // size of input x_t
    val n_v = x.dim2                                                      // number of variables
    val n_k = 24                                                          // size of each query/key vector (q_t, k_t)
    val n_u = 24                                                          // number of units (size of context vector)
//  val n_u = 28                                                          // number of units (size of context vector) - FIX only runs if n_u = n_k

    val rmg1 = RandomMatD (n_k, n, 1)                                     // random (0, 1) matrix generator 1 for q and k
    val rmg2 = RandomMatD (n_u, n, 1)                                     // random (0, 1) matrix generator 2 for v

    val w_q = rmg1.gen                                                    // query weight matrix
    val w_k = rmg1.gen                                                    // key weight matrix
    val w_v = rmg2.gen                                                    // value weight matrix

    val q = w_q * x.transpose                                             // query matrix
    val k = w_k * x.transpose                                             // key matrix
    val v = w_v * x.transpose                                             // value matrix

    banner ("Dimensions for input x, query q, key k, value v")
    println (s"x.dims = ${x.dims}")
    println (s"q.dims = ${q.dims}")
    println (s"k.dims = ${k.dims}")
    println (s"v.dims = ${v.dims}")

    banner ("Attention Matrix")
    val att = attention (q, k, v)
    println (s"att.dims = ${att.dims}")
    println (s"att      = $att")

    banner ("Context Vectors Collected into Matrix")
    val cxt = new MatrixD (att.dim, att.dim2)
    println (s"cxt.dims = ${cxt.dims}")
    for i <- q.indices do cxt(i) = context (q(i), k, v)
    println (s"cxt      = $cxt")

    assert (cxt =~ att)
  
end attentionTest

