
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

import scala.runtime.ScalaRunTime.stringOf

import scalation.mathstat._

import ActivationFun._
import Initializer._
import Optimizer._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_3L` class supports multi-output, 3-layer (input, hidden and output)
 *  Neural-Networks.  It can be used for both classification and prediction,
 *  depending on the activation functions used.  Given several input vectors and output
 *  vectors (training data), fit the parameters a and b connecting the layers,
 *  so that for a new input vector v, the net can predict the output value, i.e.,
 *      yp = f1 (b * f (a * v))
 *  where f and f1 are the activation functions and the parameter a and b
 *  are the parameters between input-hidden and hidden-output layers.
 *  Unlike `NeuralNet_2L` which adds input x0 = 1 to account for the intercept/bias,
 *  `NeuralNet_3L` explicitly adds bias.
 *  @param x       the m-by-n input/data matrix (training data consisting of m input vectors)
 *  @param y       the m-by-ny output/response matrix (training data consisting of m output vectors)
 *  @param fname_  the feature/variable names (if null, use x_j's)
 *  @param nz      the number of nodes in hidden layer (-1 => use default formula)
 *  @param hparam  the hyper-parameters for the model/network
 *  @param f       the activation function family for layers 1->2 (input to output)
 *  @param f1      the activation function family for layers 2->3 (hidden to output)
 *  @param itran   the inverse transformation function returns response matrix to original scale
 */
class NeuralNet_3L (x: MatrixD, y: MatrixD, fname_ : Array [String] = null,
                    private var nz: Int = -1, hparam: HyperParameter = Optimizer.hp,
                    f: AFF = f_sigmoid, f1: AFF = f_id,
                    val itran: FunctionM2M = null)
      extends PredictorMV (x, y, fname_, hparam)
         with Fit (dfm = x.dim2, df = x.dim - x.dim2):                    // under-estimate of degrees of freedom

    private val debug     = debugf ("NeuralNet_3L", false)                // debug function
    private val eta       = hp("eta").toDouble                            // learning rate
    private val bSize     = hp("bSize").toInt                             // batch size
    private val maxEpochs = hp("maxEpochs").toInt                         // maximum number of training epochs/iterations
//          val opti      = new Optimizer_SGD ()                          // parameter optimizer SGD
            val opti      = new Optimizer_SGDM ()                         // parameter optimizer SGDM

    // Guidelines for setting the number of nodes in hidden layer:
    if nz < 1 then nz = 2 * x.dim2 + 1                                    // [1] default number of nodes for hidden layer
//  if nz < 1 then nz = 2 * x.dim2 + y.dim2                               // [2] default number of nodes for hidden layer

    private val (n, ny) = (x.dim2, y.dim2)
    private val a  = new NetParam (weightMat (n, nz), new VectorD (nz))   // parameters (weights & biases) in to hid
                b  = new NetParam (weightMat (nz, ny), new VectorD (ny))  // parameters (weights & biases) hid to out
                bb = Array (a, b.asInstanceOf [NetParam])                 // inside array

    modelName = s"NeuralNet_3L_${f.name}_${f1.name}"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given training data x_ and y_, fit the parameters bb.
     *  Minimize the error in the prediction by adjusting the parameters bb.
     *  @param x_  the training/full data/input matrix
     *  @param y_  the training/full response/output matrix
     */
    def train (x_ : MatrixD = x, y_ : MatrixD = y): Unit =
        val epochs = opti.optimize3 (x_, y_, bb, eta, Array (f, f1))      // optimize parameters bb
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
        val etaI = (0.25 * eta, 4.0 * eta)                                                 // quarter to four times eta
        val epochs = opti.auto_optimize (x_, y_, bb, etaI, Array (f, f1), opti.optimize3)  // optimize parameters bb
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
        val yp = f1.f_ (b dot f.f_ (a dot v))                             // scaled prediction
        if itran == null then yp else itran (MatrixD (yp))(0)             // back to original scale
    end predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given an input matrix v, predict the output/response matrix f(v).
     *  @param v  the input matrix
     */
    override def predict (v: MatrixD = x): MatrixD =
        val yp = f1.fM (b * (f.fM (a * v)))                               // scaled predictions
        if itran == null then yp else itran (yp)                          // back to original scale
    end predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a sub-model that is restricted to the given columns of the data matrix.
     *  @param x_cols  the columns that the new model is restricted to
     */
    def buildModel (x_cols: MatrixD): NeuralNet_3L =
        new NeuralNet_3L (x_cols, y, null, -1, hparam, f, f1, itran)
    end buildModel

end NeuralNet_3L


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `NeuralNet_3L` companion object provides factory functions for buidling three-layer
 *  neural nets.
 */
object NeuralNet_3L extends Scaling:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `NeuralNet_3L` with automatic resclaing from a combined data matrix.
     *  @param xy      the combined input and output matrix
     *  @param fname   the feature/variable names
     *  @param nz      the number of nodes in hidden layer (-1 => use default formula)
     *  @param hparam  the hyper-parameters
     *  @param f       the activation function family for layers 1->2 (input to output)
     *  @param f1      the activation function family for layers 2->3 (hidden to output)
     *  @param col     the first designated response column (defaults to the last column)
     */
    def apply (xy: MatrixD, fname: Array [String] = null,
               nz: Int = -1, hparam: HyperParameter = Optimizer.hp,
               f: AFF = f_sigmoid, f1: AFF = f_id)
               (col: Int = xy.dim2 - 1): NeuralNet_3L =
        var itran: FunctionM2M = null                                        // inverse transform -> original scale
        val (x, y) = (xy(?, 0 until col), xy(?, col until xy.dim2)) 

        val x_s = if scale then rescaleX (x, f)
                  else x
        val y_s = if f1.bounds != null then { val y_i = rescaleY (y, f1); itran = y_i._2; y_i._1 }
                  else y

//      println (s" scaled: x = $x_s \n scaled y = $y_s")
        new NeuralNet_3L (x_s, y_s, fname, nz, hparam, f, f1, itran)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `NeuralNet_3L` with automatic rescaling from a data matrix and response vector.
     *  @param x       the input/data matrix
     *  @param y       the output/response matrix
     *  @param fname   the feature/variable names
     *  @param nz      the number of nodes in hidden layer (-1 => use default formula)
     *  @param hparam  the hyper-parameters
     *  @param f       the activation function family for layers 1->2 (input to output)
     *  @param f1      the activation function family for layers 2->3 (hidden to output)
     */
    def rescale (x: MatrixD, y: MatrixD, fname: Array [String] = null,
                 nz: Int = -1, hparam: HyperParameter = Optimizer.hp,
                 f: AFF = f_sigmoid, f1: AFF = f_id): NeuralNet_3L =
        var itran: FunctionM2M = null                                        // inverse transform -> original scale

        val x_s = if scale then rescaleX (x, f)
                  else x
        val y_s = if f1.bounds != null then { val y_i = rescaleY (y, f1); itran = y_i._2; y_i._1 }
                  else y

        println (s" scaled: x = $x_s \n scaled y = $y_s")
        new NeuralNet_3L (x_s, y_s, fname, nz, hparam, f, f1, itran)
    end rescale

end NeuralNet_3L


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `neuralNet_3LTest` main function is used to test the `NeuralNet_3L` class.
 *  > runMain scalation.modeling.neuralnet.neuralNet_3LTest
 */
@main def neuralNet_3LTest (): Unit =

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

    val mod = new NeuralNet_3L (x, y)                            // create NeuralNet_3L model 
//  mod.trainNtest ()()                                          // train and test the model
    mod.trainNtest2 ()()                                         // train and test the model - with auto-tuning
//  println (mod.summary ())                                     // parameter/coefficient statistics - FIX - implement?

    banner ("neuralNet_3LTest: Compare with Linear Regression - first column of y")
    val y0  = y(?, 0)                                            // use first column of response matrix y
    val rg0 = new Regression (x, y0)                             // create a Regression model
    rg0.trainNtest ()()                                          // train and test the model
    println (rg0.summary ())                                     // parameter/coefficient statistics

    banner ("neuralNet_3LTest: Compare with Linear Regression - second column of y")
    val y1  = y(?, 1)                                            // use second column of response matrix y
    val rg1 = new Regression (x, y1)                             // create a Regression model
    rg1.trainNtest ()()                                          // train and test the model
    println (rg1.summary ())                                     // parameter/coefficient statistics

end neuralNet_3LTest 

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `neuralNet_3LTest2` main function tests the `NeuralNet_3L` class using the
 *  Concrete dataset.
 *  > runMain scalation.modeling.neuralnet.neuralNet_3LTest2
 */
@main def neuralNet_3LTest2 (): Unit =

    import Example_Concrete._

//  println (s"ox = $ox")
//  println (s"y  = $y")
    println (s"ox_fname = ${stringOf (ox_fname)}")

    banner ("Concrete NeuralNet_3L")
//  val mod = new NeuralNet_3L (ox, y, ox_fname)                 // create model with intercept (else pass x)
    val mod = NeuralNet_3L.rescale (ox, y, ox_fname)             // create model with intercept (else pass x) - rescales
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

end neuralNet_3LTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `neuralNet_3LTest3` main function tests the `NeuralNet_3L` class using the
 *  AutoMPG dataset.
 *  > runMain scalation.modeling.neuralnet.neuralNet_3LTest3
 */
@main def neuralNet_3LTest3 (): Unit =

    import Example_AutoMPG.{x, y, x_fname}                       // don't include intercept, uses biases instead

    val yy = MatrixD (y).transpose

//  println (s"x  = $x")
//  println (s"yy = $yy")
    println (s"x_fname = ${stringOf (x_fname)}")

    banner ("AutoMPG NeuralNet_3L")
//  val mod = new NeuralNet_3L (x, yy, x_fname)                  // create model with intercept (else pass x)
    val mod = NeuralNet_3L.rescale (x, yy, x_fname)              // create model with intercept (else pass x) - rescales
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

end neuralNet_3LTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `neuralNet_3LTest4` main function tests the `NeuralNet_3L` class using the AutoMPG
 *  dataset.  Assumes no missing values.  It tests forward selection.
 *  > runMain scalation.modeling.neuralnet.neuralNet_3LTest4
 */
@main def neuralNet_3LTest4 (): Unit =

    import Example_AutoMPG.{x, y, x_fname}                       // don't include intercept, uses biases instead

//  println (s"x  = $x")
//  println (s"yy = $yy")
    println (s"x_fname = ${stringOf (x_fname)}")

    val yy = MatrixD (y).transpose                               // vector to matrix with 1 column

    banner ("AutoMPG NeuralNet_3L")
//  val mod = new NeuralNet_3L (x, yy, x_fname)                  // create model with intercept (else pass x)
    val mod = NeuralNet_3L.rescale (x, yy, x_fname)              // create model with intercept (else pass x) - rescales
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

end neuralNet_3LTest4


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `neuralNet_3LTest5` main function tests the `NeuralNet_3L` class using the AutoMPG
 *  dataset.  Assumes no missing values.  It tests forward, backward and stepwise selection.
 *  > runMain scalation.modeling.neuralnet.neuralNet_3LTest5
 */
@main def neuralNet_3LTest5 (): Unit =

    import Example_AutoMPG.{x, y, x_fname}                       // don't include intercept, uses biases instead

//  println (s"x  = $x")
//  println (s"yy = $yy")

    val yy = MatrixD (y).transpose                               // vector to matrix with 1 column

    banner ("AutoMPG NeuralNet_3L")
//  val mod = new NeuralNet_3L (x, yy, x_fname)                  // create model with intercept (else pass x)
    val mod = NeuralNet_3L.rescale (x, yy, x_fname)              // create model with intercept (else pass x) - rescales
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

end neuralNet_3LTest5


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `neuralNet_3LTest6` main function tests the `NeuralNet_3L` class using the
 *  AutoMPG dataset.  It tries all activation functions.
 *  > runMain scalation.modeling.neuralnet.neuralNet_3LTest6
 */
@main def neuralNet_3LTest6 (): Unit =

    import Example_AutoMPG.{x, y, x_fname}                       // don't include intercept, uses biases instead

    val yy = MatrixD (y).transpose

//  println (s"x  = $x")
//  println (s"yy = $yy")
    println (s"x_fname = ${stringOf (x_fname)}")

    Optimizer.hp ("eta") = 0.002                                 // some activation functions need smaller eta
    for f <- f_aff do                                            // try all activation functions for first layer
        banner (s"AutoMPG NeuralNet_3L with ${f.name}")
        val mod = NeuralNet_3L.rescale (x, yy, x_fname, f = f)   // create model with intercept (else pass x) - rescales
        mod.trainNtest2 ()()                                     // train and test the model - with auto-tuning

        banner ("AutoMPG Validation Test")
        println (FitM.showFitMap (mod.validate ()(), QoF.values.map (_.toString)))
    end for

end neuralNet_3LTest6


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `neuralNet_3LTest7` main function is used to test the `NeuralNet_3L` class.
  *  It tests a simple case that does not require a file to be read.
  *  @see translate.google.com/translate?hl=en&sl=zh-CN&u=https:
  *       //www.hrwhisper.me/machine-learning-decision-tree/&prev=search
  *  > runMain scalation.modeling.neuralnet.neuralNet_3LTest7
  */
@main def neuralNet_3LTest7 (): Unit =

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
    val nn2 = Perceptron.rescale (ox, y, f = ActivationFun.f_tanh)
    nn2.trainNtest ()()                                               // train and test the model

    val ym = MatrixD (y).transpose
    Optimizer.hp ("eta") = 0.85                                       // Preceptron and NeuralNet_2L use different optimizers,
                                                                      // so different learning rates (eta) are needed.
    banner (s"NeuralNet_2L sigmoid")
    val nn3 = NeuralNet_2L.rescale (ox, ym)
    nn3.trainNtest ()()                                               // train and test the model

    banner (s"NeuralNet_2L tanh")
    val nn4 = NeuralNet_2L.rescale (ox, ym, f = ActivationFun.f_tanh)
    nn4.trainNtest ()()                                               // train and test the model

    banner (s"NeuralNet_3L sigmoid-id")
    val nn5 = NeuralNet_3L.rescale (ox, ym)
    nn5.trainNtest ()()                                               // train and test the model

    banner (s"NeuralNet_3L tanh-tanh")
    val nn6 = NeuralNet_3L.rescale (ox, ym, f = ActivationFun.f_tanh, f1 = ActivationFun.f_tanh)
    nn6.trainNtest ()()                                               // train and test the model

end neuralNet_3LTest7

