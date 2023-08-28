
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 2.0
 *  @date    Sat Aug  8 20:26:34 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model Framework:  Trait for Bayesian Classifiers
 */

package scalation
package modeling
package classifying

import scalation.mathstat._
import scalation.log2

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BayesClassifier` trait provides methods for Bayesian Classifiers, including
 *  calculations of joint probabilities and Conditional Mutual Information (CMI).
 *  Make sure the variable values start at zero, otherwise call the `shift2zero` method.
 *  If the value counts (vc) are unknown, the `vc_fromData` method may be called.
 *      Classifier.shift2zero (x)                // make sure values for all features start at zero
 *      val vc = Classifier.vc_fromData (x)      // set value counts from data
 *  @see `bayesClassifierTest` for calculating cmi and `bayesClassifierTest2` for cmiMatrix
 *  @param k  the number of classes (defaults to binary (2-way) classification
 */
trait BayesClassifier (k: Int = 2):

    private val EPS = 1E-9                                              // a small value to prevent "/ 0"

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the joint probability of x and y and return it as a matrix.
     *  @param x    the integer-valued data vectors stored as columns of a matrix
     *  @param vcx  the value count for x (number of distinct values for x)
     *  @param y    the class vector, where y(i) = class
     */
    def jProbXY (x: VectorI, vcx: Int, y: VectorI): MatrixD =
        (Probability.freq (x, vcx, y, k) + EPS) / x.dim.toDouble
    end jProbXY

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the joint probability of x, z and y and return it as a tensor.
     *  @param x     the first integer-valued data vector
     *  @param z     the second integer-valued data vector
     *  @param vcxz  the vector of value counts (number of distinct values for x, z)
     *  @param y     the class vector, where y(i) = class
     */
    def jProbXZY (x: VectorI, z: VectorI, vcxz: VectorI, y: VectorI): RTensorD =
        (RTensorD.freq (x, z, vcxz, y, k) + EPS) / x.dim.toDouble
    end jProbXZY

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the Conditional Mutual Information (CMI) of data vectors x and z,
     *  given response/classification vector y, i.e., I(x; z | y).
     *  @see en.wikipedia.org/wiki/Conditional_mutual_information
     *  @param x     the first integer-valued data vector
     *  @param z     the second integer-valued data vector
     *  @param vcxz  the vector of value counts (number of distinct values for x, z)
     *  @param y     the class vector, where y(i) = class
     */
    def cmi (x: VectorI, z: VectorI, vcxz: VectorI, y: VectorI): Double =
        var cmi_ = 0.0

        val p_y   = y.freq (k)._2                                       // class/prior probability of y
        val p_xy  = jProbXY (x, vcxz(0), y)                             // joint probability of x and y
        val p_zy  = jProbXY (z, vcxz(1), y)                             // joint probability of z and y
        val p_xzy = jProbXZY (x, z, vcxz, y)                            // joint probability of x, z and y

        for c <- 0 until k do                                           // check each class, where k = p_y.size
            val py = p_y(c)
            for xj <- 0 until vcxz(0) do
                val pxy = p_xy(xj, c)
                for zl <- 0 until vcxz(1) do
                    val pxzy = p_xzy(xj, zl, c)
                    cmi_    += pxzy * log2 ((py * pxzy) / (pxy * p_zy(zl, c)))
        end for
        cmi_
    end cmi

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the Conditional Mutual Information (CMI) matrix for data matrix x
     *  given response/classification vector y, i.e., I(xj; xl | y) for all pairs
     *  of features/columns xj and xl in matrix x.
     *  @see en.wikipedia.org/wiki/Conditional_mutual_information
     *  @param x   the integer-valued data vectors stored as columns of a matrix
     *  @param vc  the vector of value counts (number of distinct values per feature)
     *  @param y   the class vector, where y(i) = class for row i of the matrix x, x(i)
     */
    def cmiMatrix (x: MatrixD, vc: VectorI, y: VectorI): MatrixD =
        val n     = x.dim2                                              // number of features
        val cmiMx = new MatrixD (n, n)                                  // CMI matrix

        for j <- x.indices2; l <- j + 1 until n do
            cmiMx(j, l) = cmi (x(?, j).toInt, x(?, l).toInt, VectorI (vc(j), vc(l)), y)
        end for
        cmiMx
    end cmiMatrix

end BayesClassifier


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bayesClassifierTest` main function is used to test the `BayesClassifier`
 *  Calculate the CMI I(x; z | y) and it should be 0.15834454180428106.
 *  @see stackoverflow.com/questions/55402338/finding-conditional-mutual-information-from-3-discrete-variable
 *  > runMain scalation.modeling.classifying.bayesClassifierTest
 */
@main def bayesClassifierTest (): Unit =

    object bc extends BayesClassifier ()                                // create a Bayes Classifier object

    val x = VectorI (0, 1, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0)
    val z = VectorI (0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 1, 0)
    val y = VectorI (1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 0, 1) 

    banner ("Example Conditional Mutual Information (CMI) Calculation")
    val cmi_xzy = bc.cmi (x, z, VectorI (2, 2), y)
    println (s"CMI I(x; z | y) cmi_xzy = $cmi_xzy")

end bayesClassifierTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bayesClassifierTest2` main function is used to test the `BayesClassifier` class
 *  using the Play Tennis Example.  Calculate the CMI Matrix I(xj; xl | y) for j < l
 *  > runMain scalation.modeling.classifying.bayesClassifierTest2
 */
@main def bayesClassifierTest2 (): Unit =

    import Example_PlayTennis.{x, y, k}

    println (s"x = $x")
    println (s"y = $y")

    object bc extends BayesClassifier (k)                               // create a Bayes Classifier object

    Classifier.shift2zero (x)                                           // make sure values for all features start at zero
    val vc = Classifier.vc_fromData (x)                                 // set value counts from data

    banner (s"Probability of Response y")
    val p_y = y.freq (k)._2
    println (s"p_y = $p_y")                                             // class/prior probability of y

    for j <- x.indices2 do
        banner (s"Joint Probability Feature x$j and Response y")
        val p_xy = bc.jProbXY (x(?, j).toInt, vc(j), y)
        println (s"p_xy = $p_xy")                                       // joint probability of xj and y
    end for

    for j <- x.indices2; l <- j + 1 until x.dim2 do
        banner (s"Joint Probability Features x$j, x$l and Response y")
        val p_xzy = bc.jProbXZY (x(?, j).toInt, x(?, l).toInt, VectorI (vc(j), vc(l)), y)
        println (s"p_xzy = $p_xzy")                                     // joint probability of xj, xl and y
    end for

    banner ("Conditional Mutual Information")
    val cmi = bc.cmiMatrix (x, vc, y)                                   // Conditional Mutual Information (CMI)
    println (s"cmi = $cmi")

    val cmiAns = MatrixD ((4, 4), 0.00000, 0.419593, 0.222815, 0.311752,
                                  0.00000, 0.00000,  0.419593, 0.168895,
                                  0.00000, 0.00000,  0.00000,  0.0610538,
                                  0.00000, 0.00000,  0.00000,  0.00000)
    println (s"cmiAns = $cmiAns")

end bayesClassifierTest2

