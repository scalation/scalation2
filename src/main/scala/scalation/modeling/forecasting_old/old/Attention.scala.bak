
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep  4 13:09:52 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model Framework: Context and Attention for Transformers
 *
 *  @see https://sebastianraschka.com/blog/2023/self-attention-from-scratch.html
 *  @see https://arxiv.org/pdf/1706.03762.pdf (main paper)
 */

package scalation
package modeling
package forecasting

import scala.math.sqrt

import scalation.mathstat._
import scalation.random.{RandomMatD, RandomTenD}

import ActivationFun.f_softmax

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Attention` trait provides methods for computing context vectors, single-head
 *  attention matrices and multi-head attention matrices.
 *  @param n_var  the size of the input vector x_t (number of variables)
 *  @param n_mod  the size of the output (dimensionality of the model, d_model)
 *  @param heads  the number of attention heads
 */
trait Attention (n_var: Int, n_mod: Int = 512, heads: Int = 8):

    val n_k = n_mod / heads                                              // size per head (dimensionality d_k, d_v)
    val rmg = RandomMatD (n_k, n_var, 1)                                 // random (0, 1) matrix generator for q, k, v

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Query, Key, Value matrices from the given input and weight matrices.
     *  @param x    the input matrix
     *  @param w_q  the weight matrix for query Q
     *  @param w_v  the weight matrix for key K
     *  @param w_v  the weight matrix for value V
     */
    def queryKeyValue (x: MatrixD, w_q: MatrixD, w_k: MatrixD, w_v: MatrixD): (MatrixD, MatrixD, MatrixD) =
        val x_t = x.transpose
        (w_q * x_t, w_k * x_t, w_v * x_t)
    end queryKeyValue

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute a Context Vector from the given query at time t (q_t), key (K) and value (V).
     *  @param q_t  the query vector at time t (based on input vector x_t)
     *  @param k    the key matrix K
     *  @param v    the value matrix V
     */
    def context (q_t: VectorD, k: MatrixD, v: MatrixD): VectorD =
        val root_n = sqrt (q_t.dim)
        f_softmax.f_ (k * (q_t / root_n)) *: v
    end context

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute a Self-Attention Weight Matrix from the given query (Q), key (K) and value (V).
     *  @param q  the query matrix Q (q_t over all time)
     *  @param k  the key matrix K
     *  @param v  the value matrix V
     */
    def attention (q: MatrixD, k: MatrixD, v: MatrixD): MatrixD =
        val root_n = sqrt (q.dim2)
        f_softmax.fM (q * (k.transpose / root_n)) * v
    end attention

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute a Multi-Head, Self-Attention Weight Matrix by taking attention for each head
     *  and concatenating them; finally multiplying by the overall weight matrix w_o.
     *  The operator ++^ concatenates matrices column-wise.
     *  @param q    the query matrix Q (q_t over all time)
     *  @param k    the key matrix K
     *  @param v    the value matrix V
     *  @param w_q  the weight tensor for query Q (w_q(i) matrix for i-th head)
     *  @param w_v  the weight tensor for key K (w_k(i) matrix for i-th head)
     *  @param w_v  the weight tensor for value V (w_v(i) matrix for i-th head)
     *  @param w_o  the overall weight matrix to be applied to concatenated attention
     */
    def attentionMH (q: MatrixD, k: MatrixD, v: MatrixD,
                     w_q: TensorD, w_k: TensorD, w_v: TensorD,
                     w_o: MatrixD): MatrixD =
        var att = attention (q * w_q(0), k * w_k(0), v * w_v(0))
        for i <- 1 until heads do att = att ++^ attention (q * w_q(i), k * w_k(i), v * w_v(i))
        println (s"att.dims = ${att.dims}, w_o.dims = ${w_o.dims}")
        att * w_o
    end attentionMH

end Attention


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Attention` object contains sample a input matrix from
 *  @see https://sebastianraschka.com/blog/2023/self-attention-from-scratch.html
 *  The example is from 6 words with 16 dimensional encoding.
 */
object Attention:

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

end Attention

import Attention._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `attentionTest` function tests the `context` and `attention` top-level functions.
 *  > runMain scalation.modeling.forecasting.attentionTest
 */
@main def attentionTest (): Unit =

    val n_var = x.dim2                                                    // number of variables in input vector x_t
    val n_mod = 24                                                        // size of each query/key vector (q_t, k_t, v_t)
    val heads = 1                                                         // number of attention heads
    object att extends Attention (n_var, n_mod, heads)

    val (q, k, v) = att.queryKeyValue (x, att.rmg.gen, att.rmg.gen, att.rmg.gen)

    banner ("Dimensions for input x, query q, key k, value v")
    println (s"x.dims = ${x.dims}")
    println (s"q.dims = ${q.dims}")
    println (s"k.dims = ${k.dims}")
    println (s"v.dims = ${v.dims}")

    banner ("Attention Matrix")
    val aw = att.attention (q, k, v)
    println (s"aw.dims = ${aw.dims}")
    println (s"aw      = $aw")

    banner ("Context Vectors Collected into Matrix")
    val cxt = new MatrixD (aw.dim, aw.dim2)
    println (s"cxt.dims = ${cxt.dims}")
    for i <- q.indices do cxt(i) = att.context (q(i), k, v)
    println (s"cxt      = $cxt")

    assert (cxt =~ aw)
  
end attentionTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `attentionTest2` function tests the `attentionMH` top-level function.
 *  Test Multi-Head, Self-Head Attention.
 *  > runMain scalation.modeling.forecasting.attentionTest2
 */
@main def attentionTest2 (): Unit =

    val n_var = x.dim2                                                    // number of variables in input vector x_t
    val n_mod = 72                                                        // size of each query/key vector (q_t, k_t, v_t)
    val heads = 3                                                         // number of attention heads
    object att extends Attention (n_var, n_mod, heads)

    val (q, k, v) = att.queryKeyValue (x, att.rmg.gen, att.rmg.gen, att.rmg.gen)

    banner ("Dimensions for input x, query q, key k, value v")
    println (s"x.dims = ${x.dims}")
    println (s"q.dims = ${q.dims}")
    println (s"k.dims = ${k.dims}")
    println (s"v.dims = ${v.dims}")

    // Multi-Head (MH)

    val rtg = RandomTenD (heads, n_mod, att.n_k, 1)                       // random (0, 1) tensor generator for q, k, v
    val rmg = RandomMatD (n_mod, n_mod, 1)                                // random (0, 1) matrix generator for for w_o

    val wt_q = rtg.gen                                                    // MH query weight tensor:   heads x n_mod x n_k
    val wt_k = rtg.gen                                                    // MH key weight tensor:     heads x n_mod x n_k
    val wt_v = rtg.gen                                                    // MH value weight tensor;   heads x n_mod x n_k
    val w_o  = rmg.gen                                                    // MH overall weight matrix: n_mod x n_mod 

    banner ("Dimensions for query wt_q, key wt_k, value wt_v, overall w_o")
    println (s"wt_q.dims = ${wt_q.dims}")
    println (s"wt_k.dims = ${wt_k.dims}")
    println (s"wt_v.dims = ${wt_v.dims}")
    println (s"w_o.dims  = ${w_o.dims}")

    banner ("Multi-Head Attention Matrix")
    val aw = att.attentionMH (q, k, v, wt_q, wt_k, wt_v, w_o)
    println (s"aw.dims = ${aw.dims}")
    println (s"aw      = $aw")

end attentionTest2

