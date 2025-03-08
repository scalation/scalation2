
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John A. Miller
 *  @version 2.0
 *  @date    Fri Oct 13 22:21:37 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Framework: Dropout Layer
 */

package scalation
package modeling
package forecasting
package neuralforecasting

import scalation.mathstat.MatrixD
import scalation.random.Bernoulli

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DropoutLayer` class will, in computing the output, set each element to zero with
 *  probability p; otherwise, multiply it by a scale factor.
 *  @see pytorch.org/docs/stable/generated/torch.nn.Dropout.html#torch.nn.Dropout
 *  @param p  the probability of setting an element to zero
 */
case class DropoutLayer (p: Double = 0.5):

    private val coin   = Bernoulli (p)                                      // Bernoulli RVG
    private val factor = 1.0 / (1.0 - p)                                    // scale factor

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: calculate the output of this layer.
     *  @param x  the m by nx input matrix (full or batch)
     */
    def apply (x: MatrixD): MatrixD =
        val y = new MatrixD (x.dim, x.dim2)
        for i <- x.indices; j <- x.indices2 do
            y(i, j) = if coin.igen == 1 then 0.0 else factor * x(i, j) 
        y
    end apply

end DropoutLayer

