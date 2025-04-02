
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John A. Miller
 *  @version 2.0
 *  @date    Fri Oct 13 22:21:37 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Framework: Normalization Layer
 *
 *  @see proceedings.neurips.cc/paper/2019/file/1e8a19426224ca89e83cef47f1e7f53b-Paper.pdf
 */

package scalation
package modeling
package forecasting
package neuralforecasting

import scalation.mathstat.{MatrixD, VectorD}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LayerNorm` class will, in computing the output, normalize by subtracting the
 *  mean and dividing by the standard deviation.
 *  @see pytorch.org/docs/stable/generated/torch.nn.LayerNorm.html#torch.nn.LayerNorm
 *  @param atransform  whether to apply an affine transformation to standard normalization
 *  @param eps         the small value to prevent division by zero
 */
case class LayerNorm (atransform: Boolean = true, eps: Double = 1E-5):

    private var w = 1.0                                                   // learnable weight 
    private var b = 0.0                                                   // learnable bias

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the weight and bias.
     *  @param w_  the new weight
     *  @param b_  the new bias
     */
    def reset (w_ : Double, b_ : Double): Unit = { w = w_; b = b_ }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: calculate the output of this layer.
     *  @param x  the m by nx input matrix (full or batch)
     */
    def apply (x: MatrixD): MatrixD =
        val y = (x - x.mean) / (x.stdev + eps)
        if atransform then y * w + b else y
    end apply

end LayerNorm


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RMSNorm` class will, in computing the output, normalize by dividing by the
 *  Root Mean Square (RMS).
 */
case class RMSNorm ():

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: calculate the output of this layer given the values sent to
     *  all its neurons:  u = W x  where W is a weight matrix
     *  @param u  the input vector to the given layer
     */
    def apply (u: VectorD): VectorD =  u / u.rms

end RMSNorm

