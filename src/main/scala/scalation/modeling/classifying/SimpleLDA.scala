
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue Feb 27 15:16:23 EST 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: Simple Linear Discriminant Analysis (LDA) Classifier
 */

package scalation
package modeling
package classifying

import scala.math.log

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SimpleLDA` class implements a Linear Discriminant Analysis (LDA) classifier.
 *  It places a value into a group according to its maximal discriminant function.
 *  @see en.wikipedia.org/wiki/Linear_discriminant_analysis
 *  @param x       the input/design matrix with only one column
 *  @param y       the response/classification vector, y_i in {0, 1}
 *  @param fname_  the name for the feature/variable
 *  @param k       the number of possible values for y (0, 1, ... k-1)
 *  @param cname_  the names for all classes
 *  @param hparam  the hyper-parameters
 */
class SimpleLDA (x: MatrixD, y: VectorI, fname_ : Array [String] = Array ("x1"),
                 k: Int = 2, cname_ : Array [String] = Array ("No", "Yes"),
                 hparam: HyperParameter = Classifier.hp)
      extends Classifier (x, y, fname_, k, cname_, hparam)
         with FitC (k):

    private val debug = debugf ("SimpleLDA", true)                       // debug function

    private var mu: VectorD = null                                       // group means
    private var sig2 = 0.0                                               // pooled variance
    private var term1: VectorD = null                                    // first term in classify
    private var term2: VectorD = null                                    // second term in classify

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the classifer by computing pcovar, ipcovar and prior
     *  that are needed to compute the discriminant functions f.
     *  These are computed in the predictI method.
     *  @param x_  the training/full data/input matrix (defaults to full x)
     *  @param y_  the training/full response/output vector (defaults to full y)
     */
    override def train (x_ : MatrixD = x, y_ : VectorI = y): Unit =
        val xc = for c <- 0 until k yield                                // groups for x
            VectorD (for i <- y_.indices if y(i) == c yield x_(0, i))    // group c
        p_y = VectorD (xc.map (_.dim / y.dim.toDouble))                  // probability y = c
        mu  = VectorD (xc.map (_.mean))                                  // group means
        var sum = 0.0
        for c <- 0 until k do sum += (xc(c) - mu(c)).normSq
        sig2  = sum / (m - k).toDouble                                   // pooled variance
        term1 = mu / sig2
        term2 = mu~^2 / (2.0 * sig2) - p_y.map (log (_))

        debug ("train", s"p_y \t = $p_y \n mu \t = $mu \n sig2 \t = $sig2")
        debug ("train", s"term1 \t = $term1 \n term2 \t = $term2")
    end train

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the predictive model y_ = f(x_) + e and return its predictions and QoF vector.
     *  Testing may be in-sample (on the full dataset) or out-of-sample (on the testing set)
     *  as determined by the parameters passed in.
     *  Note: must call train before test.
     *  @param x_  the testing/full data/input matrix (defaults to full x)
     *  @param y_  the testing/full response/output vector (defaults to full y)
     */
    def test (x_ : MatrixD = x, y_ : VectorI = y): (VectorI, VectorD) =
        val yp  = predictI (x_)                                          // predicted classes
        val qof = diagnose (y_.toDouble, yp.toDouble)                    // diagnose from actual and predicted
        debug ("test", s" yp = $yp \n qof = $qof")
        (yp, qof)
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict/classify vector z by computing its discriminant function delta for
     *  each group and return the group index with the maximun value for delta.
     *  @param z  the new vector to predict
     */
    override def predictI (z: VectorD): Int =
        val delta = term1 * z(0) - term2
        delta.argmax ()
    end predictI

    override def predictI (z: VectorI): Int = predictI (z.toDouble)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a QoF summary for a model with diagnostics for predictor x_1,
     *  and the overall Quality of Fit (QoF).
     *  @param x_      the testing/full data/input matrix
     *  @param fname_  the array of feature/variable names
     *  @param b_      the parameters/coefficients for the model
     *  @param vifs    the Variance Inflation Factors (VIFs)
     */
    override def summary (x_ : MatrixD = null, fname_ : Array [String] = null,
                          b_ : VectorD = parameter, vifs: VectorD = null): String =
        super.summary (x_, fname_, b_, vifs)                             // summary from `Fit`
    end summary

end SimpleLDA


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleLDATest` main function tests the `SimpleLDA` class.
 *  @see people.revoledu.com/kardi/tutorial/LDA/Numerical%20Example.html
 *  > runMain scalation.modeling.classifying.SimpleLDATest
 */
@main def simpleLDATest (): Unit =

    // features/variable: 
    // x1: curvature
    //                 x1
    val x = VectorD (2.95, 2.53, 3.57, 3.16, 2.58, 2.16, 3.27)
    val y = VectorI (   0,    0,    0,    0,    1,    1,    1)

    val k     = 2                                                        // number of classes
    val fname = Array ("curvature")                                      // feature name
    val cname = Array ("pass", "fail")                                   // class names

    val xx = MatrixD (x).transpose                                       // put vector into matrix

    val lda = new SimpleLDA (xx, y, fname, k, cname)                     // create SimpleLDA classifier
    lda.trainNtest ()                                                    // train and test the classifier
    println (lda.summary ())

    banner ("classify")
    val z  = VectorD (2.81)
    println (s"classify ($z) = ${lda.classify (z)}")

end simpleLDATest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `simpleLDATest2` main function tests the `SimpleLDA` class.
 *  > runMain scalation.modeling.classifying.simpleLDATest2
 */
@main def simpleLDATest2 (): Unit =

    import scalation.random.Normal

    val normal1 = Normal (98.6,  1.0)                                    // variate generator 1
    val normal2 = Normal (101.0, 1.0)                                    // variate generator 2

    val x = new MatrixD (200, 1)                                         // temperature vector in column 0
    val y = new VectorI (x.dim)                                          // actual class vector
    for (i <- 0 until 100)   { x(0, i) = normal1.gen; y(i) = 0 }
    for (i <- 100 until 200) { x(0, i) = normal2.gen; y(i) = 1 }

    val k     = 2                                                        // number of classes
    val fname = Array ("temperature")                                    // feature name
    val cname = Array ("well", "has-flu")                                // class names

    val lda  = new SimpleLDA (x, y, fname, k, cname)                     // create SimpleLDA classifier
    lda.trainNtest ()()                                                  // train and test the classifier
    println (lda.summary ())

end simpleLDATest2

