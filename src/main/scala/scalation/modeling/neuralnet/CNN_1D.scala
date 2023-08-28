
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Oct 28 20:43:47 EDT 2020
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: 1D Convolutional Neural Network (CNN)
 */

package scalation
package modeling
package neuralnet

import scalation.mathstat._

import ActivationFun._
import Initializer._
import Optimizer._

import CoFilter_1D.conv

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CNN_1D` class implements a Convolutionsl Network model.
 *  The model is trained using a data matrix x and response matrix y.
 *  @param x       the input/data matrix with instances stored in rows
 *  @param y       the output/response matrix, where y_i = response for row i of matrix x
 *  @param fname_  the feature/variable names (defaults to null)
 *  @param nf      the number of filters for this convolutional layer
 *  @param nc      the width of the filters (size of cofilters)
 *  @param hparam  the hyper-parameters for the model/network
 *  @param f       the activation function family for layers 1->2 (input to hidden)
 *  @param f1      the activation function family for layers 2->3 (hidden to output)
 *  @param itran   the inverse transformation function returns responses to original scale
 */
class CNN_1D (x: MatrixD, y: MatrixD, fname_ : Array [String] = null,
              nf: Int = 1, nc: Int = 3,
              hparam: HyperParameter = Optimizer.hp,
              f: AFF = f_reLU, f1: AFF = f_reLU,
              val itran: FunctionM2M = null)
      extends PredictorMV (x, y, fname_, hparam)
         with Fit (dfm = x.dim2 - 1, df = x.dim - x.dim2):

    private val debug = debugf ("CNN_1D", true)                           // debug function
    private val eta       = hp("eta").toDouble                            // learning rate
    private val bSize     = hp("bSize").toInt                             // batch size
    private val maxEpochs = hp("maxEpochs").toInt                         // maximum number of training epochs/iterations
    private val (n, ny)   = (x.dim2, y.dim2)
    private val nz = n - nc + 1

    private var c = weightVec (nc)                                        // parameters (weights & biases) in to hid
    private val b: NetParam = NetParam (weightMat (nz, ny), new VectorD (ny))   // parameters (weights & biases) hid to out

    modelName = s"CNN_1D_${f.name}_${f1.name}"

    println (s"Create a CNN_1D with $n input, $nf filters and $ny output nodes")

    private val filt = Array.fill (nf)(new CoFilter_1D (nc))              // array of filters

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the i-th input vector with the f-th filter.
     *  @param i  the index of the i-th row of the matrix
     *  @param f  the index of the f-th filter
     */
    def filter (i: Int, f: Int): VectorD =
        val xi = x(i)
        val ft = filt(f)
        val xf = new VectorD (xi.dim - nc + 1)
//      for j <- xf.indices fo xf(j) = ft.dot (xi, j)
        xf
    end filter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update filter fs parameters.
     *  @param f     the index for the filter
     *  @param vec2  the new paramters for the filter's vector
     */
    def updateFilterParams (f: Int, vec2: VectorD): Unit = filt(f).update (vec2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameters c and b.
     */
    override def parameters: NetParams = Array (NetParam (MatrixD.fromVector (c)), b)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data x_ and y_, fit the parametera a and b.
     *  This is a simple algorithm that iterates over several epochs using gradient descent.
     *  It does not use batching nor a sufficient stopping rule.
     *  In practice, use the train or train2 methods that use better optimizers.
     *  @param x_  the training/full data/input matrix
     *  @param y_  the training/full response/output matrix
     */
    def train (x_ : MatrixD = x, y_ : MatrixD = y): Unit =
        println (s"train: eta = $eta")
        var sse0 = Double.MaxValue                                        // hold prior value of sse

        var (go, epoch) = (true, 1)
        cfor (go && epoch <= maxEpochs, epoch += 1) {  
            var z  = f.fM (conv (c, x_))                                  // Z  = f(conv (c, X))
            var yp = f1.fM (b * z)                                        // Yp = f(ZB)
            val ε  = yp - y                                               // negative error E  = Yp - Y
            val δ1 = f1.dM (yp) ⊙ ε                                       // delta matrix for y
            val δ0 = f.dM (z) ⊙ (δ1 * b.w.Ƭ)                              // delta matrix for z`
            CNN_1D.updateParam (x_, z, δ0, δ1, eta, c, b)

            val yp_ = f1.fM (f.fM (b * conv (c, x)))                      // updated predictions
            val sse = (y_ - yp_).normFSq                                  // recompute sum of squared errors
            debug ("train", s"sse for $epoch th epoch: sse = $sse")
            if sse > sse0 then go = false                                 // return early if moving up
            sse0 = sse                                                    // save prior sse
        } // cfor
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data x_ and y_, fit the parameters c and b.
     *  Iterate over several epochs, where each epoch divides the training set into
     *  batches.  Each batch is used to update the weights.       
     *  FIX - to be implemented
     *  @param x_  the training/full data/input matrix
     *  @param y_  the training/full response/output matrix
     */
    override def train2 (x_ : MatrixD = x, y_ : MatrixD = y): Unit =
        val epochs = 0 // optimize3 (x_, y_, c, b, eta, bSize, maxEpochs, f, f1)    // optimize parameters c, b
        println (s"ending epoch = $epochs")
//      estat.tally (epochs._2)
    end train2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test a predictive model y_ = f(x_) + e and return its QoF vector.
     *  Testing may be be in-sample (on the training set) or out-of-sample
     *  (on the testing set) as determined by the parameters passed in.
     *  Note: must call train before test.
     *  @param x_  the testing/full data/input matrix (defaults to full x)
     *  @param y_  the testing/full response/output matrix (defaults to full y)
     */
    def test (x_ : MatrixD = x, y_ : MatrixD = y): (MatrixD, MatrixD) =
        val yp = predict (x_)                                            // make predictions
        val yy = if itran == null then y_ else itran (y_)                // undo scaling, if used
        e = yy - yp                                                      // RECORD the residuals/errors (@see `Predictor`)
        val qof = MatrixD (for k <- yy.indices2 yield diagnose (yy(?, k), yp(?, k))).transpose
        (yp, qof)                                                        // return predictions and QoF vector
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new input vector z, predict the output/response vector f(z).
     *  @param z  the new input vector
     */
    def predict (z: VectorD): VectorD = f1.f_ (b dot f.f_ (conv (c, z)))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an input matrix z, predict the output/response matrix f(z).
     *  @param z  the input matrix
     */
    override def predict (z: MatrixD = x): MatrixD = f1.fM (b * f.fM (conv (c, z)))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a sub-model that is restricted to the given columns of the data matrix.
     *  @param x_cols  the columns that the new model is restricted to
     */
    def buildModel (x_cols: MatrixD): CNN_1D =
        new CNN_1D (x_cols, y, null, nf, nc, hparam, f, f1, itran)
    end buildModel

end CNN_1D


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CNN_1D` companion object provides factory methods for creating 1D
 *  convolutional neural networks.
 */
object CNN_1D:

    def apply (xy: MatrixD): CNN_1D = ???

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the parameters:  the weights in the convolutional filter c and
     *  the weights biases in the fully-connected layer b.
     *  @param x_  the training/full data/input matrix
     *  @param z   the training/full response/output matrix
     *  @param δ0  the convolutional layer delta
     *  @param δ1  the fully-connectd layer delta
     *  @param c   the convolution filter vector
     *  @param b   the fully-connectd layer parameters
     */
    def updateParam (x_ : MatrixD, z: MatrixD, δ0: MatrixD, δ1: MatrixD, eta: Double, c: VectorD, b: NetParam) =
        for j <- c.indices do
            var sum = 0.0
            for i <- x_.indices; h <- z.indices2 do sum += x_(i, h+j) * δ0(i, h)
            c(j) -= (sum / x_.dim) * eta                                 // update c weights in conv filter 
        end for
        b -= (z.Ƭ * δ1 * eta, δ1.mean * eta)                              // update b weights & biases
    end updateParam

end CNN_1D


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `cNN_1DTest` main function is used to test the `CNN_1D` class.
 *  Test using the simple example from section 11.10 of ScalaTion textbook.
 *  Perform four training steps.
 *  > runMain scalation.modeling.neuralnet.cNN_1DTest
 */
@main def cNN_1DTest (): Unit =

    val x = MatrixD ((2, 5), 1, 2, 3, 4, 5,
                             6, 7, 8, 9, 10)
    val y = MatrixD ((2, 2),  6,  9,
                             16, 24)
    val c = VectorD (0.5, 1, 0.5)
    val b = NetParam (MatrixD ((3, 2), 0.1, 0.2,
                                       0.3, 0.4,
                                       0.5, 0.6))
    val eta = 0.001
    hp("eta") = eta
    val f  = f_reLU                                                       // first activation function
    val f1 = f_reLU                                                       // second activation function

    println (s"input x = $x")                                             // input/data matrix
    println (s"input y = $y")                                             // output/response matrix
    println (s"eta     = ${hp("eta")}")

    for epoch <- 1 to 4 do
        banner (s"Start of epoch $epoch")
        println (s"filter  c = $c")                                       // values for cofilter
        println (s"weights b = $b")                                       // values for fully-connected layer

        val z  = f.fM (conv (c, x))                                       // Z  = f(conv (c, X))
        val yp = f1.fM (z *: b)                                           // Yp = f(ZB)
        val ε  = yp - y                                                   // negative error E  = Yp - Y
        val δ1 = f1.dM (yp) ⊙ ε                                           // delta matrix for y
        val δ0 = f.dM (z) ⊙ (δ1 * b.w.Ƭ)                                  // delta matrix for z

        println (s"feature map z  = $z")
        println (s"response    yp = $yp")
        println (s"- error     ε  = $ε")
        println (s"delta 1     δ1 = $δ1")
        println (s"delta 0     V0 = $δ0")

        CNN_1D.updateParam (x, z, δ0, δ1, eta, c, b)
        val yp_ = f1.fM (f.fM (conv (c, x)) *: b)                         // updated predictions
        val sse = (y - yp_).normFSq
        println (s"sse for $epoch th epoch: sse = $sse")
    end for

end cNN_1DTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `cNN_1DTest2` main function is used to test the `CNN_1D` class
 *  using the AutoMPG dataset.
 *  > runMain scalation.modeling.neuralnet.cNN_1DTest2
 */
@main def cNN_1DTest2 (): Unit =

    import Example_AutoMPG._
    banner ("CNN_1D vs. Regession - ExampleAutoMPG")

    banner ("Regression")
    val reg = Regression (oxy)()
    reg.trainNtest ()()

    banner ("CNN_1D")
    hp("eta") = 0.01
    val cnn   = new CNN_1D (x, MatrixD.fromVector (y))
    cnn.trainNtest ()()

end cNN_1DTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `cNN_1DTest3` main function is used to test the `CNN_1D` class
 *  for the convolutional operator.
 *  > runMain scalation.modeling.neuralnet.cNN_1DTest3
 */
@main def cNN_1DTest3 (): Unit =

    val c = VectorD (1, 2)
    val x = VectorD (1, 2, 3, 4)

    val y = new VectorD (c.dim + x.dim - 1)
    for k <- y.indices do
        var sum = 0.0
        for j <- c.indices do
            val i = k - j
            if 0 <= i && i < x.dim then sum += c(j) * x(k - j)
        end for
        y(k) = sum
    end for
    println (s"y = $y")

end cNN_1DTest3

