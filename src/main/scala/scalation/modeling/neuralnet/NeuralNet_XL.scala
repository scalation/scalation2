
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Mar 16 15:13:38 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: Neural Network with 3 Layers (input, hidden and output layers)
 *
 *  @see     hebb.mit.edu/courses/9.641/2002/lectures/lecture03.pdf
 */

package scalation
package modeling
package neuralnet

import scala.math.max
import scala.runtime.ScalaRunTime.stringOf

import scalation.mathstat._

import ActivationFun._
import Initializer._
import Optimizer._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_XL` class supports multi-output, X-layer (input, hidden* and output)
 *  Neural-Networks.  It can be used for both classification and prediction,
 *  depending on the activation functions used.  Given several input vectors and output
 *  vectors (training data), fit the parameters [b] connecting the layers,
 *  so that for a new input vector v, the net can predict the output value, e.g.,
 *      yp = f3 (c * f2 (b * f (a * v)))
 *  where f, f2 and f3 are the activation functions and the parameter a, b and b
 *  are the parameters between input-hidden1, hidden1-hidden2 and hidden2-output layers.
 *  Unlike `NeuralNet_2L` which adds input x0 = 1 to account for the intercept/bias,
 *  `NeuralNet_XL` explicitly adds bias.
 *  Defaults to two hidden layers.
 *  This implementation is partially adapted from Michael Nielsen's Python implementation found in
 *  @see  github.com/mnielsen/neural-networks-and-deep-learning/blob/master/src/network2.py
 *  @see  github.com/MichalDanielDobrzanski/DeepLearningPython35/blob/master/network2.py
 *------------------------------------------------------------------------------
 *  @param x       the m-by-n input/data matrix (training data consisting of m input vectors)
 *  @param y       the m-by-ny output/response matrix (training data consisting of m output vectors)
 *  @param fname_  the feature/variable names (if null, use x_j's)
 *  @param nz      the number of nodes in each hidden layer, e.g., Array (9, 8) => 2 hidden of sizes 9 and 8
 *                 (null => use default formula)
 *  @param hparam  the hyper-parameters for the model/network
 *  @param f       the array of activation function families between every pair of layers
 *  @param itran   the inverse transformation function returns response matrix to original scale
 */
class NeuralNet_XL (x: MatrixD, y: MatrixD, fname_ : Array [String] = null,
                    private var nz: Array [Int] = null, hparam: HyperParameter = Optimizer.hp,
                    f: Array [AFF] = Array (f_sigmoid, f_sigmoid, f_id),
                    val itran: FunctionM2M = null)
      extends PredictorMV (x, y, fname_, hparam)
         with Fit (dfm = x.dim2, df = x.dim - x.dim2):                    // under-estimate of degrees of freedom

    private val debug     = debugf ("NeuralNet_XL", false)                // debug function
    private val flaw      = flawf ("NeuralNet_XL")                        // flaw function
    private val eta       = hp("eta").toDouble                            // learning rate
    private val bSize     = hp("bSize").toInt                             // batch size
    private val maxEpochs = hp("maxEpochs").toInt                         // maximum number of training epochs/iterations
    private val lambda    = hp ("lambda").toDouble                        // regularization hyper-parameter
    private val nl        = f.length                                      // number of ACTIVE layers (i.e., with activation function)
    private val layers    = 0 until nl                                    // range for active layers
//          val opti      = new Optimizer_SGD ()                          // parameter optimizer SGD
            val opti      = new Optimizer_SGDM ()                         // parameter optimizer SGDM

    // Guidelines for setting the number of nodes in hidden layer:
    if nz == null then nz = compute_nz                                    // default number of nodes for each hidden layers

    if nz.length + 1 != nl then
        flaw ("init", "count mismatch among number of layers and activation functions")
    end if

    if nl < 2 then flaw ("init", s"must have at least two ACTIVE layers, but nl = $nl")

    private val sizes = x.dim2 +: nz :+ y.dim2                            // sizes (# nodes) of all layers
                bb = Array.ofDim [NetParam] (nl)                          // parameters for each active layer

    for l <- layers do
        bb(l) = new NetParam (weightMat (sizes(l), sizes(l+1)),           // parameters weights &
                              weightVec (sizes(l+1)))                     // biases per active layer
    end for

    modelName = s"NeuralNet_XL_${stringOf (f.map (_.name))}"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute default values for the number nodes in each hidden layer, based on
     *  the number of nodes in the input layer.
     *  Rule: e.g., n = 15 => [ 31, 15, 10, 7 ] 
     */
    def compute_nz: Array [Int] =
        val nz1 = 2 * x.dim2 + 1
        (for l <- 1 until f.length yield max (1, nz1 / l)).toArray
    end compute_nz

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data x_ and y_, fit the parameters bb.
     *  Minimize the error in the prediction by adjusting the parameters bb.
     *  @param x_  the training/full data/input matrix
     *  @param y_  the training/full response/output matrix
     */
    def train (x_ : MatrixD = x, y_ : MatrixD = y): Unit =
        val epochs = opti.optimize (x_, y_, bb, eta, f)                   // optimize parameters bb
        println (s"ending epoch = $epochs")
        estat.tally (epochs._2)
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data x_ and y_, fit the parameters bb.
     *  Minimize the error in the prediction by adjusting the parameters bb.
     *  This version preforms an interval search for the best eta value.
     *  @param x_  the training/full data/input matrix
     *  @param y_  the training/full response/output matrix
     */
    override def train2 (x_ : MatrixD = x, y_ : MatrixD = y): Unit =
        val etaI = (0.25 * eta, 4.0 * eta)                                     // quarter to four times eta
        val epochs = opti.auto_optimize (x_, y_, bb, etaI, f, opti.optimize)   // optimize parameters bb
        println (s"ending epoch = $epochs")
        estat.tally (epochs._2)
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
    /** Given a new input vector v, predict the output/response vector f(v).
     *  @param v  the new input vector
     */
    def predict (v: VectorD): VectorD =
        var yp = v
        for l <- layers do yp = f(l).f_ (bb(l) dot yp)                    // scaled prediction
        if itran == null then yp else itran (MatrixD (yp))(0)             // back to original scale
    end predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an input matrix v, predict the output/response matrix f(v).
     *  @param v  the input matrix
     */
    override def predict (v: MatrixD = x): MatrixD =
        var yp = v
        for l <- layers do yp = f(l).fM (bb(l) * yp)                      // scaled predictions
        if itran == null then yp else itran (yp)                          // back to original scale
    end predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a sub-model that is restricted to the given columns of the data matrix.
     *  @param x_cols  the columns that the new model is restricted to
     */
    def buildModel (x_cols: MatrixD): NeuralNet_XL =
        new NeuralNet_XL (x_cols, y, null, null, hparam, f, itran)
    end buildModel

end NeuralNet_XL


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_XL` companion object provides factory functions for buidling multi-layer
 *  neural nets.
 */
object NeuralNet_XL extends Scaling:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `NeuralNet_XL` with automatic resclaing from a combined data matrix.
     *  @param xy      the combined input and output matrix
     *  @param fname   the feature/variable names
     *  @param nz      the number of nodes in each hidden layer, e.g., Array (5, 10) means 2 hidden with sizes 5 and 10
     *  @param hparam  the hyper-parameters
     *  @param f       the array of activation function families between every pair of layers
     *  @param col     the first designated response column (defaults to the last column)
     */
    def apply (xy: MatrixD, fname: Array [String] = null,
               nz: Array [Int] = null, hparam: HyperParameter = Optimizer.hp,
               f: Array [AFF] = Array (f_sigmoid, f_sigmoid, f_id))
               (col: Int = xy.dim2 - 1): NeuralNet_XL =
        var itran: FunctionM2M = null                                        // inverse transform -> original scale
        val (x, y) = (xy(?, 0 until col), xy(?, col until xy.dim2)) 

        val x_s = if scale then rescaleX (x, f(0))
                  else x
        val y_s = if f.last.bounds != null then { val y_i = rescaleY (y, f.last); itran = y_i._2; y_i._1 }
                  else y

//      println (s" scaled: x = $x_s \n scaled y = $y_s")
        new NeuralNet_XL (x_s, y_s, fname, nz, hparam, f, itran)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `NeuralNet_XL` with automatic rescaling from a data matrix and response vector.
     *  @param x       the input/data matrix
     *  @param y       the output/response matrix
     *  @param fname   the feature/variable names
     *  @param nz      the number of nodes in hidden layer (-1 => use default formula)
     *  @param hparam  the hyper-parameters
     *  @param f       the array of activation function families between every pair of layers
     */
    def rescale (x: MatrixD, y: MatrixD, fname: Array [String] = null,
                 nz: Array [Int] = null, hparam: HyperParameter = Optimizer.hp,
                 f: Array [AFF] = Array (f_sigmoid, f_sigmoid, f_id)): NeuralNet_XL =
        var itran: FunctionM2M = null                                        // inverse transform -> original scale

        val x_s = if scale then rescaleX (x, f(0))
                  else x
        val y_s = if f.last.bounds != null then { val y_i = rescaleY (y, f.last); itran = y_i._2; y_i._1 }
                  else y

//      println (s" scaled: x = $x_s \n scaled y = $y_s")
        new NeuralNet_XL (x_s, y_s, fname, nz, hparam, f, itran)
    end rescale

end NeuralNet_XL


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `neuralNet_XLTest` main function is used to test the `NeuralNet_XL` class.
 *  > runMain scalation.modeling.neuralnet.neuralNet_XLTest
 */
@main def neuralNet_XLTest (): Unit =

    val x = MatrixD ((5, 3), 1.0, 0.35, 0.9,                     // training data - input matrix (m=5 vectors)
                             1.0, 0.20, 0.7,
                             1.0, 0.30, 0.8,
                             1.0, 0.25, 0.75,
                             1.0, 0.40, 0.95)
    val y = MatrixD ((5, 2), 0.5, 0.4,                           // training data - output matrix (m=5 vectors)
                             0.3, 0.3,
                             0.2, 0.35,
                             0.3, 0.32,
                             0.6, 0.5)

    println (s"input  matrix x = $x")
    println (s"output matrix y = $y")

    val mod = new NeuralNet_XL (x, y)                            // create NeuralNet_XL model 
//  mod.trainNtest ()()                                          // train and test the model
    mod.trainNtest2 ()()                                         // train and test the model - with auto-tuning
//  println (mod.summary ())                                     // parameter/coefficient statistics - FIX - implement?

    banner ("neuralNet_XLTest: Compare with Linear Regression - first column of y")
    val y0  = y(?, 0)                                            // use first column of response matrix y
    val rg0 = new Regression (x, y0)                             // create a Regression model
    rg0.trainNtest ()()                                          // train and test the model
    println (rg0.summary ())                                     // parameter/coefficient statistics

    banner ("neuralNet_XLTest: Compare with Linear Regression - second column of y")
    val y1  = y(?, 1)                                            // use second column of response matrix y
    val rg1 = new Regression (x, y1)                             // create a Regression model
    rg1.trainNtest ()()                                          // train and test the model
    println (rg1.summary ())                                     // parameter/coefficient statistics

end neuralNet_XLTest

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `neuralNet_XLTest2` main function tests the `NeuralNet_XL` class using the
 *  Concrete dataset.
 *  > runMain scalation.modeling.neuralnet.neuralNet_XLTest2
 */
@main def neuralNet_XLTest2 (): Unit =

    import Example_Concrete._

//  println (s"ox = $ox")
//  println (s"y  = $y")
    println (s"ox_fname = ${stringOf (ox_fname)}")

    banner ("Concrete NeuralNet_XL")
//  val mod = new NeuralNet_XL (ox, y, ox_fname)                 // create model with intercept (else pass x)
    val mod = NeuralNet_XL.rescale (ox, y, ox_fname)             // create model with intercept (else pass x) - rescales
//  mod.trainNtest ()()                                          // train and test the model
    mod.trainNtest2 ()()                                         // train and test the model - with auto-tuning
//  println (mod.summary ())                                     // parameter/coefficient statistics

    banner ("Concrete Validation Test")
    println (FitM.showFitMap (mod.validate ()(), QoF.values.map (_.toString)))

/*
    banner ("Concrete Cross-Validation Test")
    val stats = mod.crossValidate ()
    Fit.showQofStatTable (stats)
*/

end neuralNet_XLTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `neuralNet_XLTest3` main function tests the `NeuralNet_XL` class using the
 *  AutoMPG dataset.
 *  > runMain scalation.modeling.neuralnet.neuralNet_XLTest3
 */
@main def neuralNet_XLTest3 (): Unit =

    import Example_AutoMPG.{x, y, x_fname}                       // don't include intercept, uses biases instead

    val yy = MatrixD (y).transpose

//  println (s"x  = $x")
//  println (s"yy = $yy")
    println (s"x_fname = ${stringOf (x_fname)}")

    banner ("AutoMPG NeuralNet_XL")
//  val mod = new NeuralNet_XL (x, yy, x_fname)                  // create model with intercept (else pass x)
    val mod = NeuralNet_XL.rescale (x, yy, x_fname)              // create model with intercept (else pass x) - rescales
//  mod.trainNtest ()()                                          // train and test the model
    mod.trainNtest2 ()()                                         // train and test the model - with auto-tuning
//  println (mod.summary ())                                     // parameter/coefficient statistics

    banner ("AutoMPG Validation Test")
    println (FitM.showFitMap (mod.validate ()(), QoF.values.map (_.toString)))

/*
    banner ("AutoMPG Cross-Validation Test")
    val stats = mod.crossValidate ()
    Fit.showQofStatTable (stats)
*/

end neuralNet_XLTest3



//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `neuralNet_XLTest4` main function tests the `NeuralNet_XL` class using the AutoMPG
 *  dataset.  Assumes no missing values.  It tests forward selection.
 *  > runMain scalation.modeling.neuralnet.neuralNet_XLTest4
 */
@main def neuralNet_XLTest4 (): Unit =

    import Example_AutoMPG.{x, y, x_fname}                       // don't include intercept, uses biases instead

//  println (s"x  = $x")
//  println (s"y  = $y")
    println (s"x_fname = ${stringOf (x_fname)}")

    val yy = MatrixD (y).transpose                               // vector to matrix with 1 column

    banner ("AutoMPG NeuralNet_XL")
//  val mod = new NeuralNet_XL (x, yy, x_fname)                  // create model with intercept (else pass x)
    val mod = NeuralNet_XL.rescale (x, yy, x_fname)              // create model with intercept (else pass x) - rescales
//  mod.trainNtest ()()                                          // train and test the model
    mod.trainNtest2 ()()                                         // train and test the model - with auto-tuning
//  println (mod.summary ())                                     // parameter/coefficient statistics

    banner ("Feature Selection Technique: Forward")
    val (cols, rSq) = mod.forwardSelAll ()                       // R^2, R^2 bar, R^2 cv
//  val (cols, rSq) = mod.backwardElimAll ()                     // R^2, R^2 bar, R^2 cv
    val k = cols.size
    println (s"k = $k, n = ${x.dim2}")
    new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "R^2 cv"),
               s"R^2 vs n for ${mod.modelName}", lines = true)
    println (s"rSq = $rSq")

end neuralNet_XLTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `neuralNet_XLTest5` main function tests the `NeuralNet_XL` class using the AutoMPG
 *  dataset.  Assumes no missing values.  It tests forward, backward and stepwise selection.
 *  > runMain scalation.modeling.neuralnet.neuralNet_XLTest5
 */
@main def neuralNet_XLTest5 (): Unit =

    import Example_AutoMPG.{x, y, x_fname}                       // don't include intercept, uses biases instead

//  println (s"x = $x")
//  println (s"y  = $y")

    val yy = MatrixD (y).transpose                               // vector to matrix with 1 column

    banner ("AutoMPG NeuralNet_XL")
//  val mod = new NeuralNet_XL (x, yy, x_fname)                  // create model with intercept (else pass x)
    val mod = NeuralNet_XL.rescale (x, yy, x_fname)              // create model with intercept (else pass x) - rescales
//  mod.trainNtest ()()                                          // train and test the model
    mod.trainNtest2 ()()                                         // train and test the model - with auto-tuning
//  println (mod.summary ())                                     // parameter/coefficient statistics

    banner ("Cross-Validation")
    FitM.showQofStatTable (mod.crossValidate ())

    println (s"x_fname = ${stringOf (x_fname)}")

    for tech <- SelectionTech.values do
        banner (s"Feature Selection Technique: $tech")
        val (cols, rSq) = mod.selectFeatures (tech)              // R^2, R^2 bar, R^2 cv
        val k = cols.size
        println (s"k = $k, n = ${x.dim2}")
        new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "R^2 cv"),
                   s"R^2 vs n for ${mod.modelName} with $tech", lines = true)
        println (s"$tech: rSq = $rSq")
    end for

end neuralNet_XLTest5


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `neuralNet_XLTest6` main function tests the `NeuralNet_XL` class using the
 *  AutoMPG dataset.  It tries all activation functions.
 *  > runMain scalation.modeling.neuralnet.neuralNet_XLTest6
 */
@main def neuralNet_XLTest6 (): Unit =

    import Example_AutoMPG.{x, y, x_fname}                       // don't include intercept, uses biases instead

    val yy = MatrixD (y).transpose

//  println (s"x  = $x")
//  println (s"yy = $yy")
    println (s"x_fname = ${stringOf (x_fname)}")

    Optimizer.hp ("eta") = 0.005                                 // some activation functions need smaller eta
    for f <- f_aff; f2 <- f_aff do                               // try all activation functions for first two layers
        banner (s"AutoMPG NeuralNet_XL with ${f.name}")
        val mod = NeuralNet_XL.rescale (x, yy, x_fname,
                             f = Array (f, f2, f_id))            // create model with intercept (else pass x) - rescales
        mod.trainNtest2 ()()                                     // train and test the model - with auto-tuning

        banner ("AutoMPG Validation Test")
        println (FitM.showFitMap (mod.validate ()(), QoF.values.map (_.toString)))
    end for

end neuralNet_XLTest6


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `neuralNet_XLTest7` main function is used to test the `NeuralNet_XL` class.
  *  It tests a simple case that does not require a file to be read.
  *  @see translate.google.com/translate?hl=en&sl=zh-CN&u=https:
  *       //www.hrwhisper.me/machine-learning-decision-tree/&prev=search
  *  > runMain scalation.modeling.neuralnet.neuralNet_XLTest7
  */
@main def neuralNet_XLTest7 (): Unit =

    import ActivationFun.f_tanh

    val x  = MatrixD ((10, 1), 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val y  = VectorD (5.56, 5.70, 5.91, 6.40, 6.80, 7.05, 8.90, 8.70, 9.00, 9.05)
    val ox = VectorD.one (x.dim) +^: x
    val fname = Array ("x")

    banner (s"Regression with intercept")
    val reg = new Regression (ox, y)
    reg.trainNtest ()()                                               // train and test the model

    banner (s"Perceptron sigmoid")
    val nn = Perceptron.rescale (ox, y)
    nn.trainNtest ()()                                                // train and test the model

    banner (s"Perceptron tanh")
    val nn2 = Perceptron.rescale (ox, y, f = f_tanh)
    nn2.trainNtest ()()                                               // train and test the model

    val ym = MatrixD (y).transpose
    Optimizer.hp ("eta") = 0.85                                       // Preceptron and NeuralNet_2L use different optimizers,
                                                                      // so different learning rates (eta) are needed.
    banner (s"NeuralNet_2L sigmoid")
    val nn3 = NeuralNet_2L.rescale (ox, ym)
    nn3.trainNtest ()()                                               // train and test the model

    banner (s"NeuralNet_2L tanh")
    val nn4 = NeuralNet_2L.rescale (ox, ym, f = f_tanh)
    nn4.trainNtest ()()                                               // train and test the model

    banner (s"NeuralNet_3L sigmoid-id")
    val nn5 = NeuralNet_3L.rescale (ox, ym)
    nn5.trainNtest ()()                                               // train and test the model

    banner (s"NeuralNet_3L tanh-tanh")
    val nn6 = NeuralNet_3L.rescale (ox, ym, f = f_tanh, f1 = f_tanh)
    nn6.trainNtest ()()                                               // train and test the model

    banner (s"NeuralNet_XL sigmoid-sigmoid-id")
    val nn7 = NeuralNet_XL.rescale (ox, ym)
    nn7.trainNtest ()()                                               // train and test the model

    banner (s"NeuralNet_XL tanh-tanh-tanh")
    val nn8 = NeuralNet_XL.rescale (ox, ym, f = Array (f_tanh, f_tanh, f_tanh))
    nn8.trainNtest ()()                                               // train and test the model

end neuralNet_XLTest7

