
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John A. Miller
 *  @version 2.0
 *  @date    Fri Oct 13 22:21:37 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model Framework: Normalization Layer
 */

package scalation
package modeling
package forecasting

import scalation.mathstat.MatrixD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NormLayer` class will, in computing the output, normalize by subtracting the
 *  mean and dividing by the standard deviation.
 *  @see pytorch.org/docs/stable/generated/torch.nn.LayerNorm.html#torch.nn.LayerNorm
 *  @param atransform  whether to apply an affine transformation to standard normalization
 *  @param eps         the small value to prevent division by zero
 */
case class NormLayer (atransform: Boolean = true, eps: Double = 1E-5):

    private var w = 1.0                                                   // learnable weight 
    private var b = 0.0                                                   // learnable bias

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: calculate the output of this layer.
     *  @param x  the m by nx input matrix (full or batch)
     */
    def apply (x: MatrixD): MatrixD =
        val y = (x - x.mean) / (x.stdev + eps)
        if atransform then y * w + b else y
    end apply

end NormLayer

