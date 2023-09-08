
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue Aug 29 13:54:14 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: Recurrent Neural Network (RNN) for Multivariate Time Series
 */

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Standard RNN (GRU/LSTM) Formats for Multivariate Time Series Data:
 *  Matlab:
 *
 *  Keras: 3D format expected by GRU/LSTM is [samples, timesteps, features].
 *         => indexing [timestamp t, lags k, variable j]
 *  PyTorch:
 */

package scalation
package modeling
package forecasting

import scalation.mathstat.{MatrixD, Plot, VectorD}
import scalation.random.{NormalMat, NormalVec_c}

import ActivationFun.{softmax_, tanh_}
import MatrixD.outer

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RNN` class implements Recurrent Neural Network (RNN) via Back Propagation Through
 *  Time (BPTT).  At each time point x_t, there is a vector representing several variables
 *  or the encoding of a word.  Intended to work for guessing the next work in a sentence
 *  or for multi-horizon forecasting.
 *  Time series: (x_t: t = 0, 1, ..., n_seq-1) where n_seq is the number of time points/words
 *  @param x       the input sequence/time series
 *  @param y       the output sequence/time series
 *  @param fname   the feature/variable names
 *  @param n_mem   the size for hidden state (h) (dimensionality of memory)
 */
class RNN (x: MatrixD, y: MatrixD, fname: Array [String] = null, n_mem: Int = 8):   // 4

    private val CLASSIF    = false                                     // whether to apply classification (e.g., guess next word) or forecast a value 
    private val max_epochs = 20                                        // maximum number of iterations
    private val eta        = 0.0005                                    // the learning rate (use 0.25 for rNNTest)

    private val n_seq = x.dim                                          // e.g., 20, number of words in a sentence (including start and end symbol)
    private val n_var = x.dim2                                         // e.g., 64, number of variables or distinct words (vocabulary size)
                                                                       // since we will only use one sentence for training,
                                                                       // this is also the total steps during training.

    private val _1 = VectorD.one (n_mem)                               // vector of all ones for e.g., 1 - z

    // initialize parameters (weights and biases)
    private val rmg1 = NormalMat (n_mem, n_var, 0.0, 0.01)             // random (Normal) matrix generators
    private val rmg2 = NormalMat (n_mem, n_mem, 0.0, 0.01)
    private val rmg3 = NormalMat (n_var, n_mem, 0.0, 0.01)
    private val rvg1 = NormalVec_c (n_mem, 0.0, 0.01)                  // random (Normal) vector generators
    private val rvg3 = NormalVec_c (n_var, 0.0, 0.01)

    private var U    = rmg1.gen                                        // parameters for computing the hidden state
    private var W    = rmg2.gen
    private var b_h  = rvg1.gen

    private val hg = Gate (n_seq, n_mem, n_var)                        // hidden state gate-like structure

    // decoder for generating output
    private var V    = rmg3.gen                                        // decoder weight matrix
    private var b_y  = rvg3.gen                                        // decoder bias vector

    private var h_m1 = rvg1.gen                                        // hidden state @ t = -1 (m1 means minus 1)
    private val h    = new MatrixD (n_seq, n_mem)                      // hidden state h
    private val yp   = new MatrixD (n_seq, n_var)                      // predicted output
    private val L    = new VectorD (n_seq)                             // store loss function values

    // the partial derivative of weights and biases
    private var dh_m1 = new VectorD (h_m1.dim)
    private var db_y: VectorD = null
    private var dV    = new MatrixD (V.dim, V.dim2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the RNN using simple gradient descent.
     */
    def train (): Unit =
        for it <- 1 to max_epochs do
            forward ()            // forward propagate: get intermediate and output results

            println (s"train: for epoch $it: loss function L = $L")
            banner (s"train: for epoch $it: total loss function L.sum = ${L.sum}")

            backward ()           // back propagate: calculate gradients (partial derivatives)

            update_params ()      // update parameters (weights and biases)
        end for
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the RNN predictions.
     */
    def test (): Unit =
        new Plot (null, y(?, 0), yp(?, 0), "Plot of y vs yp for RNN", lines = true)
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Forward propagate calculates yp, loss and intermediate variables for each step.
     */
    def forward (): Unit =
        for t <- 0 until n_seq do
            val h_pre = if t == 0 then h_m1 else h(t-1)                // get previous hidden state
            h(t) = tanh_ (U * x(t) + W * h_pre + b_h)                  // compute hidden state
            if CLASSIF then
                yp(t) = softmax_ (V * h(t) + b_y)                      // activation: softmax for classification
                L(t)  = (-y(t) * log_ (yp(t))).sum                     // cross-entropy loss function
            else
                yp(t) = V * h(t) + b_y                                 // activation: id for forecasting
                L(t)  = (y(t) - yp(t)).normSq	                       // sse loss function
            end if
        end for
    end forward

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Backward propagate to calculate gradients using chain rules in O(n_seq) time.
     *  FIX - add option of using sse loss function and fix affected partial derivatives
     */
    def backward (): Unit =

        import ActivationFun.tanhD

        // start back-propagation with the final/feed-forward (ff) layer (uses id for activation)

        val e = yp - y                                                 // negative error matrix
        db_y  = e.sumVr                                                // vector of row sums
        for t <- 0 until n_seq do dV += outer (e(t), h(t))             // outer vector product
        val dh_ff = e * V                                              // partial w.r.t. h: n_seq by n_mem matrix
        var dh = new VectorD (dh_ff.dim2)                              // hold partial for hidden state (dh) @ time t
        var dIn: VectorD = null

        // calculate the derivative contribution of each step and add them up

        for t <- n_seq-1 to 1 by -1 do                                 // move back in time to t = 1
            dh += dh_ff(t)                                             // update partial for hidden state (dh) @ time t
            dIn = dh * tanhD (hg(t))                                   // input to tanh for hidden state
            hg += (dIn, x(t), h(t-1))                                  // update partials for hidden state gate @ time t
            dh = W.Ƭ * dIn                                             // Ƭ => matrix transpose
        end for

        // end case @ time t = 0 -> use h_m1 for hidden state

        dh += dh_ff(0)                                                 // update partial for hidden state (dh) @ t = 0
        dIn = dh * tanhD (hg(0))
        hg += (dIn, x(0), h_m1)                                        // update partials for hidden state gate @ t = 0
        dh_m1 = W.Ƭ * dIn
    end backward

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Based on the calculated partial derivatives, update the parameters (weights
     *  and biases).
     */
    def update_params (): Unit =
        // hidden state (h)
        U   -= hg.dU * eta
        W   -= hg.dW * eta
        b_h -= hg.db * eta

        // output layer
        V   -= dV * eta
        b_y -= db_y * eta
    end update_params

end RNN


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RNN` companion object provides factory methods.
 */
object RNN:

    import ActivationFun._

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `RNN` with automatic rescaling from a data matrix and response matrix.
     *  @param x       the input/data matrix
     *  @param y       the output/response matrix
     *  @param fname   the feature/variable names
     *  @param n_mem   the size of the hidden state (dimensionality of memory)
     */
    def rescale (x: MatrixD, y: MatrixD, fname: Array [String] = null, n_mem: Int = 4): RNN =
        val x_s = rescaleX (x, f_sigmoid)
        val y_s = rescaleY (y, f_sigmoid)._1

//      println (s" scaled: x = $x_s \n scaled y = $y_s")
        new RNN (x_s, y_s, fname, n_mem)
    end rescale

end RNN


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `rNNTest` main function tests the `RNN` class on randomly generated
 *  sequence data meant to represent encoded words 
 *  > runMain scalation.modeling.forecasting.rNNTest
 */
@main def rNNTest (): Unit =

    val n_seq = 8
    val n_var = 5

    val (x_t, y_t) = genSequenceData (n_seq, n_var)

    println (s"x_t = $x_t")
    println (s"y_t = $y_t")

    banner ("Create a Recurrent Neural Network (RNN)")
    val mod = new RNN (x_t, y_t)
    mod.train ()
    mod.test ()

end rNNTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `rNNTest2` main function tests the `RNN` class on sequence data read as words
 *  in a file that encoded and pass into `RNN`
 *  > runMain scalation.modeling.forecasting.rNNTest2
 */
@main def rNNTest2 (): Unit =

    println ("read words from a text file")

//  FIX - find example text

end rNNTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `rNNTest3` main function tests the `RNN` class on sequence/time series data
 *  corresponding to the lake level dataset using multiple lags.
 *  > runMain scalation.modeling.forecasting.rNNTest3
 */
@main def rNNTest3 (): Unit =

    import Example_LakeLevels.y
    val lags = 2                                                      // number of lags to include
    val hh   = 2                                                      // forecasting horizon - FIX - currently lags == hh

    val y_s = scaleV (extreme (y), (-2.0, 2.0))(y)                    // rescale y to active domain of sigmoid, tanh

    val (x, yy) = buildMatrix4TS (y_s, lags, hh)                      // column for each lag

    println (s"x.dims = ${x.dims}, yy.dims = ${yy.dims}")

    banner ("Create a Recurrent Neural Network Unit (RNN)")
    val mod = new RNN (x, yy)                                         // call constructor
    mod.train ()
    mod.test ()

end rNNTest3

