
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John A. Miller
 *  @version 2.0
 *  @date    Fri Oct 13 22:21:37 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Framework: (Optionally Activated) Linear Transformation Layer
 */

package scalation
package modeling
package forecasting
package neuralforecasting

import scalation.mathstat.MatrixD

import Initializer._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DenseLayer` class applies an (optionally activated) linear transformation to
 *  the input matrix X.
 *     Yp = f(X W + b)
 *  When f is null, it acts as a Linear Layer.
 *  @see pytorch.org/docs/stable/generated/torch.nn.Linear.html#torch.nn.Linear
 *  @param n_x  the second dimension of the input matrix (m by n_x)
 *  @param n_y  the second dimension of the output matrix (m by n_y)
 *  @param f    the activation function family for layers 1->2 (input to output)
 */
case class DenseLayer (n_x: Int, n_y: Int, f: AFF = null):

    private val w = weightMat (n_x, n_y)                             // the weight matrix
    private val b = weightVec (n_y)                                  // the bias vector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward pass: calculate the output of this layer.
     *  @param x  the m by nx input matrix (full or batch)
     */
    def apply (x: MatrixD): MatrixD =
        if f != null then f.fM (x * w + b)
        else x * w + b
    end apply

end DenseLayer

