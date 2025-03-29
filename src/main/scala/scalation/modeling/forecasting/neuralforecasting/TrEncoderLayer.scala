
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John A. Miller, Yousef Fekri Dabanloo
 *  @version 2.0
 *  @date    Fri Oct 13 22:21:37 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Framework: Transformer Encoder Layer
 *
 *  @see sebastianraschka.com/blog/2023/self-attention-from-scratch.html
 *  @see arxiv.org/pdf/1706.03762.pdf (main paper)
 */

package scalation
package modeling
package forecasting
package neuralforecasting

import scalation.mathstat._
import scalation.random.{RandomMatD, RandomTenD}

import ActivationFun._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TrEncoderLayer` class consists of a Multi-Head Self-Attention and a Feed-Forward
 *  Neural Network (FFNN) sub-layers.
 *  @see pytorch.org/docs/stable/generated/torch.nn.TransformerEncoderLayer.html#torch.nn.TransformerEncoderLayer
 *  @param n_var       the size of the input vector x_t (number of variables)
 *  @param n_mod       the size of the output (dimensionality of the model, d_model)
 *  @param heads       the number of attention heads
 *  @param n_v         the size of the value vectors
 *  @param n_z         the size of the hidden layer in the Feed-Forward Neural Network
 *  @param f           the activation function family (used by alinear1)
 *  @param p_drop      the probability of setting an element to zero in a dropout layer
 *  @param norm_eps    a small values used in normalization to avoid divide by zero
 *  @param norm_first  whether layer normalization should be done first (see apply method)
 */
class TrEncoderLayer (n_var: Int, n_mod: Int = 512, heads: Int = 8,
                      n_v: Int = -1, n_z: Int = 2024, f: AFF = f_reLU,
                      p_drop: Double = 0.5, norm_eps: Double = 1E-5, norm_first: Boolean = false)
      extends Attention (n_var, n_mod, heads, n_v):

    private val w_q = rmg.gen                                             // weight matrix for query q
    private val w_k = rmg.gen                                             // weight matrix for key k
    private val w_v = rmg.gen                                             // weight matrix for value v

    val rtg   = RandomTenD (heads, n_mod, n_k, 1)                         // random (0, 1) tensor generator for q, k
    val rtg_v = RandomTenD (heads, n_mod, n_val, 1)                       // random (0, 1) tensor generator for v
    val rmg_o = RandomMatD (heads*n_val, n_mod, 1)                        // random (0, 1) matrix generator for for w_o

    private val wt_q = rtg.gen                                            // MH query weight tensor:   heads x n_mod x n_k
    private val wt_k = rtg.gen                                            // MH key weight tensor:     heads x n_mod x n_k
    private val wt_v = rtg_v.gen                                          // MH value weight tensor;   heads x n_mod x n_val
    private val w_o  = rmg_o.gen                                          // MH overall weight matrix: n_mod x n_mod

    private val dropout_sa = DropoutLayer (p_drop)                        // dropout layer (sa_block)

    private val alinear1   = DenseLayer (n_mod, n_z, f)                   // activated linear layer (ff_block)
    private val dropout1   = DropoutLayer (p_drop)                        // dropout layer (ff_block)
    private val linear2    = DenseLayer (n_z, n_mod)                      // linear layer (ff_block)
    private val dropout2   = DropoutLayer (p_drop)                        // dropout layer (ff_block)

    private val norm1      = LayerNorm (true, norm_eps)                   // normalization layer (apply)
    private val norm2      = LayerNorm (true, norm_eps)                   // normalization layer (apply)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass:  Compute this encoder layer's result z by using Multi-Head Self-Attention
     *  followed by a Feed-Forward Neural Network.
     *  @param x  the input matrix
     */
    def apply (x: MatrixD): MatrixD =
        banner ("1. Multi-Head Self-Attention: query q, key k, value v")
        banner ("2. Fee-Forward Neural Network")

        var z: MatrixD = null
        if norm_first then
            z = x + sa_block (norm1 (x))
            z = z + ff_block (norm2 (z))
        else
            z = norm1 (x + sa_block (x))
            z = norm2 (z + ff_block (z))
        end if
        z
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Multi-Head Self-Attention result.
     *  @param x  the input matrix
     */
    def sa_block (x: MatrixD): MatrixD =
        val (q, k, v) = queryKeyValue (x, w_q, w_k, w_v)
        dropout_sa (attentionMH (q, k, v, wt_q, wt_k, wt_v, w_o))
    end sa_block

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Feed-forward Neural Network result.
     *  @param x  the input matrix
     */
    def ff_block (x: MatrixD): MatrixD =
        dropout2 (linear2 (dropout1 (alinear1 (x))))
    end ff_block

end TrEncoderLayer

