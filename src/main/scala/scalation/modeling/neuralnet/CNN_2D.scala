
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Nov  3 16:00:47 EST 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: 2D Convolutional Neural Network (CNN)
 *
 */

// U N D E R   D E V E L O P M E N T

// FIX - needs much work

package scalation
package modeling
package neuralnet

import scalation.mathstat._

import ActivationFun._
import Initializer._
import Optimizer._

import CoFilter_2D.conv

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CNN_2D` class implements a Convolutionsl Network model.
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
class CNN_2D (x: TensorD, y: MatrixD, fname_ : Array [String] = null,
              nf: Int = 1, nc: Int = 3, hparam: HyperParameter = Optimizer.hp,
              f: AFF = f_reLU, f1: AFF = f_reLU,
              val itran: FunctionM2M = null)
      extends Model                                         // FIX: need a trait like `PredictorMV` at the tensor level
         with Fit (dfm = x.dim2 - 1, df = x.dim - x.dim2):

    private val debug     = debugf ("CNN_2D", true)                       // debug function
    private val flaw      = flawf ("CNN_2D")                              // flaw function
    private val eta       = hp("eta").toDouble                            // learning rate
//  private val bSize     = hp("bSize").toInt                             // batch size
    private val maxEpochs = hp("maxEpochs").toInt                         // maximum number of training epochs/iterations
    private val (n, ny)   = (x.dim2, y.dim2)
    private val nz = n - nc + 1
    private var e: MatrixD = null

    if nz < 2 then flaw ("init", s"the size of the hidden layer nz = $nz is too small") 

    private val c = weightMat (nc, nc)                                    // parameters (weights & biases) in to hid
    private val b: NetParam = NetParam (weightMat (nz, ny), new VectorD (ny))   // parameters (weights & biases) hid to out

    private val fT = tensorize (f.f_)                                     // activation function at tensor level
    private val dT = tensorize (f.d)                                      // activation derivative at tensor level

    modelName = s"CNN_2D_${f.name}_${f1.name}"

    println (s"Create a CNN_2D with $n input, $nf filters and $ny output nodes")

    private val filt = Array.fill (nf)(new CoFilter_2D (nc))              // array of filters

    debug ("init", s"fname_ = $fname_")

    /** As seen from class CNN_2D, the missing signatures are as follows.
     *  For convenience, these are usable as stub implementations.
     *  FIX - put in new trait
     */
    def crossValidate(k: Int, rando: Boolean): Array[scalation.mathstat.Statistic] = ???
    def getFname: Array[String] = ???
    def getX: scalation.mathstat.MatrixD = ???
    def getY: scalation.mathstat.VectorD = ???
    def hparameter: scalation.HyperParameter = ???
    def parameter: scalation.mathstat.VectorD | scalation.mathstat.MatrixD = ???
    def predict(z: scalation.mathstat.VectorD): Double | scalation.mathstat.VectorD = ???
    def test (x_ : scalation.mathstat.MatrixD, y_ : scalation.mathstat.VectorD): (
        scalation.mathstat.VectorD, scalation.mathstat.VectorD) = ???
    def train(x_ : scalation.mathstat.MatrixD, y_ : scalation.mathstat.VectorD): Unit = ???

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the i-th input vector with the f-th filter.
     *  @param i  the index of the i-th row of the matrix
     *  @param f  the index of the f-th filter
     */
    def filter (i: Int, f: Int): MatrixD =
        val xi = x(i)
//      val ft = filt(f)
        val xf = new MatrixD (xi.dim - nc + 1, xi.dim - nc + 1)
//      for j <- xf.indices fo xf(j) = ft.dot (xi, j)
        xf
    end filter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update filter f's parameters.
     *  @param f     the index for the filter
     *  @param mat2  the new paramters for the filter's vector
     */
    def updateFilterParams (f: Int, mat2: MatrixD): Unit = filt(f).update (mat2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the parameters c and b.
     */
    def parameters: NetParams = Array (NetParam (c), b)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data x_ and y_, fit the parametera c and b.
     *  This is a simple algorithm that iterates over several epochs using gradient descent.
     *  It does not use batching nor a sufficient stopping rule.
     *  In practice, use the train2 method that uses a better optimizer.
     *  @param x_  the training/full data/input matrix
     *  @param y_  the training/full response/output matrix
     */
    def train (x_ : TensorD = x, y_ : MatrixD = y): Unit =
        println (s"train: eta = $eta")
        var sse0 = Double.MaxValue                                        // hold prior value of sse

        var (go, epoch) = (true, 1)
        cfor (go && epoch <= maxEpochs, epoch += 1) {  
            val φ  = fT (conv (c, x_))                                    // φ  = f(conv (c, X)) at tensor level
            val z  = φ.flatten                                            // flatten feature map φ
            val yp = f1.fM (b * z)                                        // Yp = f1(ZB)
            val ε  = yp - y                                               // negative error E  = Yp - Y
            val δ1 = f1.dM (yp) ⊙ ε                                       // delta matrix for y
            val δ0 = dT (φ).flatten ⊙ (δ1 * b.w.𝐓)                        // delta matrix for φ (transpose (𝐓))
            CNN_2D.updateParam (x_, z, δ0, δ1, eta, c, b)

            val sse = (y_ - yp).normFSq                                   // loss = sum of squared errors
            debug ("train", s"sse for $epoch th epoch: sse = $sse")
            if sse >= sse0 then go = false                                // return early if moving up
            sse0 = sse                                                    // save prior sse
        } // cfor
    end train

//          val yp_ = f1.fM (f.fM (b * conv (c, x_)))                     // updated predictions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data x_ and y_, fit the parameters c and b.
     *  Iterate over several epochs, where each epoch divides the training set into
     *  batches.  Each batch is used to update the weights.       
     *  FIX - to be implemented
     *  @param x_  the training/full data/input matrix
     *  @param y_  the training/full response/output matrix
     */
    def train2 (x_ : TensorD = x, y_ : MatrixD = y): Unit =
        val epochs = 0 // optimize3 (x_, y_, c, b, eta, bSize, maxEpochs, f, f1)    // FIX: optimize parameters c, b
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
    def test (x_ : TensorD = x, y_ : MatrixD = y): (MatrixD, MatrixD) =
        val yp = predict (x_)                                             // make predictions
        val yy = if itran == null then y_ else itran (y_)                 // undo scaling, if used
        e = yy - yp                                                       // RECORD the residuals/errors (@see `Predictor`)
        val qof = MatrixD (for k <- yy.indices2 yield diagnose (yy(?, k), yp(?, k))).𝐓   // transpose (𝐓)
        (yp, qof)                                                         // return predictions and QoF vector
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given a new input vector z, predict the output/response vector f(z).
     *  @param z  the new input vector
     */
    def predict (z: MatrixD): VectorD = f1.f_ (b dot (f.fM (conv (c, z))).flatten)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an input matrix z, predict the output/response matrix f(z).
     *  @param z  the input matrix
     */
    def predict (z: TensorD = x): MatrixD = f1.fM (b * fT (conv (c, z)).flatten)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a sub-model that is restricted to the given columns of the data matrix.
     *  @param x_cols  the columns that the new model is restricted to
     */
    def buildModel (x_cols: TensorD): CNN_2D =
        new CNN_2D (x_cols, y, null, nf, nc, hparam, f, f1, itran)
    end buildModel

end CNN_2D


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CNN_2D` companion object provides factory methods for creating 2D
 *  convolutional neural networks.
 */
object CNN_2D extends Scaling:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `CNN_2D` with automatic rescaling from a data matrix and response matrix.
     *  @param x       the input/data matrix with instances stored in rows
     *  @param y       the output/response matrix, where y_i = response for row i of matrix x
     *  @param fname   the feature/variable names (defaults to null)
     *  @param nf      the number of filters for this convolutional layer
     *  @param nc      the width of the filters (size of cofilters)
     *  @param hparam  the hyper-parameters for the model/network
     *  @param f       the activation function family for layers 1->2 (input to hidden)
     *  @param f1      the activation function family for layers 2->3 (hidden to output)
     *  @param itran   the inverse transformation function returns responses to original scale
     */
    def rescale (x: TensorD, y: MatrixD, fname: Array [String] = null,
                 nf: Int = 1, nc: Int = 3, hparam: HyperParameter = Optimizer.hp,
                 f: AFF = f_reLU, f1: AFF = f_reLU): CNN_2D =
        var itran: FunctionM2M = null                                        // inverse transform -> original scale

        val x_s = if scale then rescaleX (x, f)
                  else x
        val y_s = if f1.bounds != null then { val y_i = rescaleY (y, f1); itran = y_i._2; y_i._1 }
                  else y
//      val y_s = { val y_i = rescaleY (y, f_sigmoid); itran = y_i._2; y_i._1 }

        println (s" scaled: x = $x_s \n scaled y = $y_s")
        new CNN_2D (x_s, y_s, fname, nf, nc, hparam, f, f1, itran)
    end rescale

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the parameters:  the weights in the convolutional filter c and
     *  the weights biases in the fully-connected layer b.
     *  @param x_  the training/full data/input matrix
     *  @param z   the training/full response/output matrix
     *  @param δ0  the convolutional layer delta
     *  @param δ1  the fully-connectd layer delta
     *  @param c   the convolution filter matrix
     *  @param b   the fully-connectd layer parameters
     */
    def updateParam (x_ : TensorD, z: MatrixD, δ0: MatrixD, δ1: MatrixD, η: Double, c: MatrixD, b: NetParam) =
        for j <- c.indices do
            var sum = 0.0
            sum += 0                                                      // remove after FIX
//          for i <- x_.indices; h <- z.indices2 do sum += x_(i, h+j) * δ0(i, h)  // FIX: c now a matrix, x_ a tensor
            c(j) -= (sum / x_.dim) * η                                    // update c weights in conv filter 
        end for
        b -= (z.𝐓 * δ1 * η, δ1.mean * η)                                  // update b weights & biases (transpose 𝐓)
    end updateParam

end CNN_2D


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `cNN_2DTest` main function is used to test the `CNN_2D` class.
 *  Test using the simple example from section 11.10 of ScalaTion textbook.
 *  Perform four training steps.
 *  > runMain scalation.modeling.neuralnet.cNN_2DTest
 *
@main def cNN_2DTest (): Unit =

    val x = MatrixD ((2, 5), 1, 2, 3, 4, 5,
                             6, 7, 8, 9, 10)
    val y = MatrixD ((2, 2),  6,  9,
                             16, 24)
    val c = VectorD (0.5, 1, 0.5)
    val b = NetParam (MatrixD ((3, 2), 0.1, 0.2,
                                       0.3, 0.4,
                                       0.5, 0.6))

    val sst0 = (y(?, 0) - y(?, 0).mean).normSq                            // sum of squares total for y_:0
    val sst1 = (y(?, 1) - y(?, 1).mean).normSq                            // sum of squares total for y_:1
    println (s"sst0 = $sst0")
    println (s"sst1 = $sst1")

    val η = 0.001                                                         // learning rate

    val f  = f_reLU                                                       // first activation function
    val f1 = f_reLU                                                       // second activation function

    println (s"input x = $x")                                             // input/data matrix
    println (s"input y = $y")                                             // output/response matrix
    println (s"η       = $η")

    for epoch <- 1 to 4 do
        banner (s"Start of epoch $epoch")
        println (s"filter  c = $c")                                       // values for cofilter
        println (s"weights b = $b")                                       // values for fully-connected layer

        val φ  = f.fM (conv (c, x))                                       // φ  = f(conv (c, X))
        val yp = f1.fM (φ *: b)                                           // Yp = f1(φB)  -- use *: as b is NetParam
        val ε  = yp - y                                                   // negative error E  = Yp - Y
        val δ1 = f1.dM (yp) ⊙ ε                                           // delta matrix for y
        val δ0 = f.dM (φ) ⊙ (δ1 * b.w.𝐓)                                  // delta matrix for φ (transpose (𝐓))

        println (s"feature map φ  = $φ")
        println (s"response    yp = $yp")
        println (s"- error     ε  = $ε")
        println (s"delta 1     δ1 = $δ1")
        println (s"delta 0     V0 = $δ0")

        CNN_2D.updateParam (x, φ, δ0, δ1, η, c, b)
        val sse = ε.normFSq
        println (s"sse for $epoch th epoch: sse = $sse")

        val sse0 = ε(?, 0).normSq                                         // sum of squared errors for column 0
        val sse1 = ε(?, 1).normSq                                         // sum of squared errors for column 1
        banner ("metrics")
        println (s"sse0  = $sse0")
        println (s"sse1  = $sse1")
        println (s"R^2_0 = ${1 - sse0/sst0}")
        println (s"R^2_1 = ${1 - sse1/sst1}")
    end for

//      val yp_ = f1.fM (f.fM (conv (c, x)) *: b)                         // updated predictions

end cNN_2DTest
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `cNN_2DTest` main function is used to test the `CNN_2D` class.
 *  Test using the simple example from section 11.10 of ScalaTion textbook.
 *  Perform four training steps.
 *  > runMain scalation.modeling.neuralnet.cNN_2DTest2
 */
@main def cNN_2DTest2 (): Unit =

    val x = TensorD ((5, 5, 2), 0, 0, 2, 1, 0,
                                0, 0, 0, 1, 2,
                                1, 2, 2, 0, 2,
                                2, 0, 0, 0, 1,
                                2, 2, 2, 0, 1,

                                1, 0, 2, 1, 0,
                                1, 0, 0, 1, 2,
                                1, 2, 2, 0, 2,
                                2, 0, 0, 0, 1,
                                2, 2, 2, 0, 1)

    val y = MatrixD ((2, 2),  6,  9,
                             16, 24)

/*
    val c = MatrixD ((2, 2), 1, 1,
                             0, 1)
    val b = NetParam (MatrixD ((3, 2), 0.1, 0.2,
                                       0.3, 0.4,
                                       0.5, 0.6))

*/
    val sst0 = (y(?, 0) - y(?, 0).mean).normSq                            // sum of squares total for y_:0
    val sst1 = (y(?, 1) - y(?, 1).mean).normSq                            // sum of squares total for y_:1
    println (s"sst0 = $sst0")
    println (s"sst1 = $sst1")

    val η = 0.001                                                         // learning rate

    println (s"input x = $x")                                             // input/data matrix
    println (s"input y = $y")                                             // output/response matrix
    println (s"η       = $η")

    banner ("CNN_2D")
    hp("eta") = η
    val cnn   = new CNN_2D (x, y)
    cnn.train ()
    cnn.test ()

end cNN_2DTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `cNN_2DTest3` main function is used to test the `CNN_2D` class
 *  using the Reduced MNIST dataset with 10,000 images in the training set
 *  and 2000 images in the test set.
 *  > runMain scalation.modeling.neuralnet.cNN_2DTest3
 */
@main def cNN_2DTest3 (): Unit =

    println ("TBD")

end cNN_2DTest3

