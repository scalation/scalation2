
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Dong Yu Yu, John Miller
 *  @version 2.0
 *  @date    Fri Jan  5 16:54:27 EST 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: Regression Tree Random Forest (RF) (subsampling only)
 */

package scalation
package modeling

import scala.runtime.ScalaRunTime.stringOf

import scalation.mathstat._

import modeling.{RegressionTree  => REG_TREE}                                // swap for Regression Tree
//import modeling.{RegressionTreeMT => REG_TREE}                             // swap for Model Tree

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RegressionTreeRF` class uses several randomly built reegression trees for prediction.
 *  It randomly selects sub-samples of 'bRatio * x.dim1' size from the data x and y to
 *  build nTrees regression trees.  The predict method uses the average over all trees.
 *  Note: this version does not select sub-features to build the trees.
 *  @param x       the input/data matrix (instances by features)
 *  @param y       the ouput/response vector (instances)
 *  @param fname_  the names of the variables/features (defaults to null)
 *  @param hparam  the hyper-parameters to the random forest (defaults to RegressionTree.hp)
 */
class RegressionTreeRF (x: MatrixD, y: VectorD, fname_ : Array [String] = null,
                        hparam: HyperParameter = RegressionTree.hp)
      extends Predictor (x, y, fname_, hparam)
         with Fit (dfm = x.dim2 - 1, df = x.dim - x.dim2):                    // call resetDF once tree is built

    private val debug      = debugf ("RegressionTreeRF", true)                // debug function
    private val flaw       = flawf ("RegressionTreeRF")                       // flaw function
    private val depth      = hparam("maxDepth").toInt                         // the max depth for the base regression trees
    private val nTrees     = hparam ("nTrees").toInt                          // number of trees
    private val bRatio     = hparam ("bRatio").toDouble                       // bagging ratio 
    private val sampleSize = (bRatio * x.dim).toInt                           // size of matrix sub-samples
    private val forest     = Array.ofDim [REG_TREE] (nTrees)                  // forest of regression trees

    if nTrees <= 0 then                flaw ("init", "RF number of tree must be at least one")
    if bRatio <= 0 || bRatio >= 1 then flaw ("init", "RF bagging ratio restricted to (0, 1)")

    modelName = s"RegressionTreeRF ($depth, $nTrees)"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the regression tree RF by selecting thresholds for the features/variables
     *  in matrix x_.
     *  Build the trees of the forest by selecting a subSample for each tree.
     *  @param x_  the training/full data/input matrix
     *  @param y_  the training/full response/output vector
     */
    def train (x_ : MatrixD, y_ : VectorD): Unit =
        for k <- 0 until nTrees do
            val (xx, yy, imap) = subSample (x_, y_, sampleSize, k)            // select rows of data matrix
            debug ("train", s"for tree$k, imap = ${stringOf (imap)}")

            forest(k) = new REG_TREE (xx, yy, fname, hparam)                  // means/regression in leaves
            forest(k).train (xx, yy)
            debug ("train", s"for tree$k === \n ${forest(k).printTree ()}")
        end for
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test a predictive model y_ = f(x_) + e and return its QoF vector.
     *  Testing may be be in-sample (on the training set) or out-of-sample
     *  (on the testing set) as determined by the parameters passed in.
     *  Note: must call train before test.
     *  @param x_  the testing/full data/input matrix (defaults to full x)
     *  @param y_  the testing/full response/output vector (defaults to full y)
     */
    def test (x_ : MatrixD = x, y_ : VectorD = y): (VectorD, VectorD) =
        val yp  = predict (x_)                                               // make predictions
        val df1 = forest.foldLeft (0)(_ + _.numLeaves)                       // degrees of freedom model = number of leaves
        val df2 = y_.dim - df1                                               // degrees of freedom error
        resetDF ((df1, df2))
        (yp, diagnose (y_, yp))                                              // return predictions and QoF vector
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the response value given vector z by averaging the predictions over
     *  all the randomized trees.
     *  @param z  the vector to be predicted
     */
    override def predict (z: VectorD): Double =
        var sum = 0.0
        for k <- 0 until nTrees do sum += forest(k).predict (z)              // sum up for each tree
        sum / nTrees                                                         // take the average
    end predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a sub-model that is restricted to the given columns of the data matrix.
     *  @param x_cols  the columns that the new model is restricted to
     */
    override def buildModel (x_cols: MatrixD): RegressionTreeGB =
        new RegressionTreeGB (x_cols, y, null, hparam)
    end buildModel

end RegressionTreeRF


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `regressionTreeRFTest` main function is used to test the `RegressionTreeRF` class.
  *  @see translate.google.com/translate?hl=en&sl=zh-CN&u=https:
  *       //www.hrwhisper.me/machine-learning-decision-tree/&prev=search
  *  > runMain scalation.modeling.regressionTreeRFTest
  */
@main def regressionTreeRFTest (): Unit =

    val x  = MatrixD ((10, 1), 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val y  = VectorD (5.56, 5.70, 5.91, 6.40, 6.80, 7.05, 8.90, 8.70, 9.00, 9.05)
    val ox = VectorD.one (x.dim) +^: x
    val fname = Array ("x")

    banner (s"Regression no intercept")
    val reg = new Regression (x, y)
    reg.trainNtest ()()                                               // train and test the model

    banner (s"Regression with intercept")
    val reg2 = new Regression (ox, y)
    reg2.trainNtest ()()                                              // train and test the model

    banner (s"Quadratic Regression")
    val reg3 = SymbolicRegression.quadratic (x, y, fname)
    reg3.trainNtest ()()                                              // train and test the model

    banner (s"Perceptron sigmoid")
    val nn = Perceptron.rescale (reg3.getX, y)
    nn.trainNtest ()()                                                // train and test the model

    banner (s"Perceptron tanh")
    val nn2 = Perceptron.rescale (reg3.getX, y, f = ActivationFun.f_tanh)
    nn2.trainNtest ()()                                               // train and test the model

    for d <- 1 to 2 do
        banner (s"Regression Tree RF with maxDepth = $d")
        RegressionTree.hp.updateReturn ("maxDepth", d)
        val mod = new RegressionTreeRF (x, y, fname)
        mod.trainNtest ()()                                           // train and test the model
//      mod.printTree ()
    end for

end regressionTreeRFTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `regressionTreeRFTest2` main function tests the `RegressionTreeRF` class using the
 *  AutoMPG dataset.  Assumes no missing values.  It tests multiple depths.
 *  > runMain scalation.modeling.regressionTreeRFTest2
 */
@main def regressionTreeRFTest2 (): Unit =

    import Example_AutoMPG._

//  println (s"x = $o")
//  println (s"y = $y")

    val dmax    = 6                                                     // range of depths 1 to dmax
    val qual = new MatrixD (dmax, 3)

    for d <- 1 to dmax do
        banner ("AutoMPG Regression Tree RF with depth d = $d")
        RegressionTree.hp("maxDepth") = d
        RegressionTree.hp("nTrees")   = 7
        val mod = new RegressionTreeRF (x, y, x_fname)                  // create model with intercept (else pass x)
        val qof = mod.trainNtest ()()._2                                // train and test the model
//      mod.printTree ()                                                // print the regression tree
//      println (mod.summary ())                                        // parameter/coefficient statistics

        banner (s"AutoMPG Regression Tree RF with d = $d Validation")
        val qof2 = mod.validate ()()                                    // out-of-sampling testing
        val iq = QoF.rSq.ordinal                                        // index for rSq
        qual (d-1) = VectorD (qof(iq), qof(iq+1), qof2(iq))             // R^2, R^2 bar, R^2 os
    end for

    new PlotM (VectorD.range (1, dmax+1), qual.transpose, Array ("R^2", "R^2 bar", "R^2 os"),
               "RegressionTreeRF in-sample, out-of-sample QoF vs. depth", lines = true)
    println (s"RegressionTreeRF: qual = $qual")

end regressionTreeRFTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `regressionTreeRFTest3` main function tests the `RegressionTreeRF` class using the
 *  AutoMPG dataset.  Assumes no missing values.  It tests forward, backward and stepwise
 *  selection.
 *  > runMain scalation.modeling.regressionTreeRFTest3
 */
@main def regressionTreeRFTest3 (): Unit =

    import Example_AutoMPG._

    val d = 5                                                           // depth of tree

//  println (s"x = $x")
//  println (s"y = $y")

    banner (s"AutoMPG Regression Tree RF with d = $d")
    RegressionTree.hp("maxDepth") = d
    val mod = new RegressionTreeRF (x, y, x_fname)                      // create model with intercept (else pass x)
    mod.trainNtest ()()                                                 // train and test the model
//  mod.printTree ()                                                    // print the regression tree

    for tech <- SelectionTech.values do
        banner (s"Feature Selection Technique: $tech")
        val (cols, rSq) = mod.selectFeatures (tech)                     // R^2, R^2 bar, R^2 cv
        val k = cols.size
        println (s"k = $k, n = ${x.dim2}")
        new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "R^2 cv"),
                   s"R^2 vs n for Regression Tree RF with $tech", lines = true)
        println (s"$tech: rSq = $rSq")
    end for

end regressionTreeRFTest3

