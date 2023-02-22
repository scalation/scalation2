
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Mar 11 15:12:46 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model Support: Quality of Fit (QoF) and Confusion Matrix
 */

package scalation
package modeling
package classifying

import scala.collection.mutable.{LinkedHashMap, Map}
import scala.Double.NaN
import scala.runtime.ScalaRunTime.stringOf

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QoFC` enum defines the Quality of Fit (QoF) measures for classifiers.
 */
enum QoFC (name: String):

    case rSq    extends QoFC ("rSq")                        // index  0 - R-sqaured
    case p_rSq  extends QoFC ("p_rSq")                      // index  1 - pseudo R-squared (Efron's or McFadden's)
    case sst    extends QoFC ("sst")                        // index  2 - sum of squares total (ssr + sse)
    case sse    extends QoFC ("sse")                        // index  3 - sum of squares for error
    case mse0   extends QoFC ("mse0")                       // index  4 - raw mean squared error
    case rmse   extends QoFC ("rmse")                       // index  5 - root mean squared error
    case mae    extends QoFC ("mae")                        // index  6 - mean absolute error

    case kappa  extends QoFC ("kappa")                      // index  7 - Cohen's kappa
    case acc    extends QoFC ("acc")                        // index  8 - accuracy

    case p_m    extends QoFC ("p_m")                        // index  9 - mean micro-precision
    case r_m    extends QoFC ("r_m")                        // index 10 - mean micro-recall
    case s_m    extends QoFC ("s_m")                        // index 11 - mean micro-specificity
    case f1_m   extends QoFC ("f1_m")                       // index 12 - mean micro-F1-measure

    case p      extends QoFC ("p")                          // index 13 - precision (for k = 2)
    case r      extends QoFC ("r")                          // index 14 - recall/sensitivity (for k = 2)
    case s      extends QoFC ("s")                          // index 15 - specificity (for k = 2)
    case f1     extends QoFC ("f1")                         // index 16 - F1-measure (for k = 2)

end QoFC

import QoFC._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FitC` companion object records the indicies and labels for the
 *  base Quality of Fit (QoF) measures for the classification techniques.
 */
object FitC:

    val MIN_FOLDS = 3                                          // minimum number of folds for cross-validation

    // indices for Vecror Quality of Fit (QoF) micro-measures

    val index_p_v   =  0                                       // index  0 - micro-precision vector
    val index_r_v   =  1                                       // index  1 - micro-recall vector
    val index_s_v   =  2                                       // index  2 - micro-specificity vector
    val index_f1_v  =  3                                       // index  3 - micro-F1-measure vector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the help string that describes the Quality of Fit (QoF) measures provided
     *  by the `FitC` trait.  The QoF measures are divided into four groups:
     *  general, ordinary, micro (per class) vectors and means of the micro vectors.
     *  Ordinary are values of the last element in the micro vectors and can be
     *  interpreted as, say the precision for the last class value/label, e.g.,
     *  y = hasCancer in {no, yes}, is the prcision of the yes prediction
     *  and is most meaningful when the number of class values/labels (k) is 2.
     *  @see en.wikipedia.org/wiki/Precision_and_recall
     *  @see en.wikipedia.org/wiki/Cohen%27s_kappa
     */
    def help: String =
        """
help: Quality of Fit (QoF) measures:
    rSq   =  R-squared, the Coefficient of Determination
    p_rSq =  pseudo R-squared (Efron's or McFadden's)
    sst   =  Sum of Squares Total (ssr + sse)
    sse   =  Sum of Squares for Error
    mse0  =  raw Mean Square Error
    rmse  =  Root Mean Square error
    mae   =  Mean Absolute error

    kappa =  Cohen's kappa, adjusted accuracy that accounts for agreement by chance
    acc   =  accuracy, the fraction of predictions that are correct 

    p     =  precision, the fraction classified as true that are actually true
    r     =  recall/sensitivity, the fraction of the actually true that are classified as true
    s     =  specificity, the fraction of the actually false that are classified as false
    f1    =  F1-measure, harmonic mean of precision and recall

    p_m   =  mean of the micro-precision vector
    r_m   =  mean of the micro-recall vector
    s_m   =  mean of the micro-specificity vector
    f1_m  =  mean of the micro-F1-measure vector

    p_v   =  micro-precision vector, precision for every class
    r_v   =  micro-recall vector, recall for every class
    s_v   =  micro-specificity vector, specificity for every class
    f1_v  =  micro-F1-measure vector, F1-measure for every class
        """
    end help

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the Vector Quality of Fit (QoF) micro-measures.
     */
    def fitLabel_v: Seq [String] = Seq ("p_v",                 // index  0 - micro-precision vector
                                        "r_v",                 // index  1 - micro-recall vector
                                        "s_v",                 // index  2 - micro-specificity vector
                                        "f1_v")                // index  3 - micro-F1-measur vector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test and report the confusion matrix and associate QoF measures.
     *  @param fc  the `FitC` object
     *  @param y_  the actual class values
     *  @param yp  the predicted class values
     *  @param k   the number of class labels {0, 1, ... , k-1}
     */
    def test (fc: FitC, y_ : VectorI, yp: VectorI, k: Int = 2): Unit =
        banner ("Actual Class Values/Labels")
        println (s"y_ = $y_")                                  // actual class values

        banner ("Predicted Class Values/Labels")
        println (s"yp = $yp")                                  // predicted class values

//      fc.confusion (y_, yp)                                  // confusion matrix and derivatives
        fc.diagnose (y_, yp)                                   // full diagnosis

        banner ("Quality of Fit (QoF) measures")
        println (fc.summary (null, null, null))
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Collect qof results for a model and return them in a vector.
     *  @param fit     the fit vector with regard to the training set
     *  @param cv_fit  the fit array of statistics for cross-validation (upon test sets)
     */
    def qofVector (fit: VectorD, cv_fit: Array [Statistic]): VectorD =
        val cv = if cv_fit == null then -0.0                   // cv not computed
                 else cv_fit(rSq.ordinal).mean                 // mean for R^2 cv
        VectorD (100 * fit(rSq.ordinal),                       // R^2 percentage
                 100 * fit(f1.ordinal),                        // F1 percentage
                 100 * cv)                                     // R^2 cv percentage
    end qofVector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a table to store statistics for QoF measures, where each row corresponds
     *  to the statistics on a particular QoF measure, e.g., acc
     */
    def qofStatTable: Array [Statistic] =
        val fLabel = QoFC.values                                     // labels for QoF measures
        val stats  = Array.ofDim [Statistic] (fLabel.length)         // for collecting stats on QoF measures
        for i <- stats.indices do stats(i) = new Statistic (QoFC.values(i).toString)
        stats
    end qofStatTable

end FitC


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FitC` trait provides methods for determining the confusion matrix as well as
 *  derived Quality of Fit (QoF) measures such as pseudo R-squared, sst, sse,
 *  accuracy, precsion, recall, specificity and Cohen's kappa coefficient.
 *  @see `modeling.Fit`
 *  Must call the confusion method before calling the other methods.
 *  @param y  the vector of actual class values/labels
 *  @param k  the number distinct class values/labels
 */
trait FitC (y: VectorI, k: Int = 2)
      extends FitM:

    private val debug = debugf ("FitC", false)                       // debug function
    private val flaw  = flawf ("FitC")                               // flaw function

//  from FitM: m, sse, ssr, sst, mse0, rmse, mae, rSq

    private var p_rSq = -1.0                                         // pseudo R-squared (Efron's or McFadden's)

    private val cmat  = new MatrixI (k, k)                           // confusion matrix
    private var tcmat = new MatrixI (k, k)                           // total cummulative confusion matrix
    private val rsum  = new VectorI (k)                              // vector of row sums of cmat
    private val csum  = new VectorI (k)                              // vector of column sums of cmat

    private val pv    = new VectorD (cmat.dim)                       // micro-precision vector
    private val rv    = new VectorD (cmat.dim)                       // micro-recall vector
    private val sv    = new VectorD (cmat.dim)                       // micro-specificity vector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Clear the total cummulative confusion matrix.
     */
    def clearConfusion (): Unit = tcmat.setAll (0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a copy of the total cummulative confusion matrix tcmat and clear tcmat.
     */
    def total_cmat (): MatrixI = { val t = tcmat.copy; tcmat.setAll (0); t }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Diagnose the health of the model by computing the Quality of Fit (QoF) measures,
     *  from the error/residual vector and the predicted & actual responses.
     *  For some models the instances may be weighted.
     *  @param y_  the actual response/output vector to use (test/full)
     *  @param yp  the predicted response/output vector (test/full)
     *  @param w   the weights on the instances (defaults to null)
     */
    override def diagnose (y_ : VectorD, yp: VectorD, w: VectorD = null): VectorD =
        super.diagnose (y_, yp)                                      // compute basic QoF from `FitM`
        p_rSq = pseudo_rSq                                           // compute pseudo R^2
        confusion (y_.toInt, yp.toInt)                               // create the confusion matrix
        fit                                                          // return QoF vector
    end diagnose

    def diagnose (y_ : VectorI, yp: VectorI): VectorD =
        super.diagnose (y_.toDouble, yp.toDouble)                    // compute basic QoF from `FitM`
        p_rSq = pseudo_rSq                                           // compute pseudo R^2
        confusion (y_, yp)                                           // create the confusion matrix
        fit                                                          // return QoF vector
    end diagnose

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare the actual class y vector versus the predicted class yp vector,
     *  returning the confusion matrix cmat, which for k = 2 is
     *       yp  0   1
     *        ----------
     *  y  0  | tn  fp |
     *     1  | fn  tp |
     *        ----------
     *  Note: ScalaTion's confusion matrix is Actual × Predicted, but to swap the position of
     *  actual y (rows) with predicted yp (columns) simply use cmat.transpose, the transpose of cmat.
     *  @see www.dataschool.io/simple-guide-to-confusion-matrix-terminology
     *  @param y_  the actual class values/labels for full (y) or test (y_e) dataset
     *  @param yp  the precicted class values/labels
     */
    def confusion (y_ : VectorI, yp: VectorI): MatrixI =
        cmat.setAll (0)                                              // clear the confusion matrix
        for i <- y_.indices    do cmat(y_(i), yp(i)) += 1            // increment counts
        for i <- cmat.indices  do rsum(i) = cmat(i).sum.toInt        // compute row sums
        for j <- cmat.indices2 do csum(j) = cmat(?, j).sum.toInt     // compute column sums
        tcmat += cmat
        p_r_s ()                                                     // precision, recall and specificity
        cmat
    end confusion

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Contract the actual class y_ vector versus the predicted class yp vector.
     *  @param yp  the predicted class values/labels
     *  @param y_  the actual class values/labels for full (y) or test (y_e) dataset
     */
    def contrast (yp: VectorI, y_ : VectorI = y): Unit =
        println (s"actual    y_ = $y_")
        println (s"predicted yp = $yp")
    end contrast
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the micro-precision, micro-recall and micro-specificity vectors
     *  which have elements for each class i in {0, 1, ... k-1}.
     *  Precision is the fraction classified as true that are actually true.
     *  Recall (sensitivity) is the fraction of the actually true that are classified as true.
     *  Specificity is the fraction of the actually false that are classified as false.
     *  Note, for k = 2, ordinary precision p, recall r and specificity s will
     *  correspond to the last elements in the pv, rv and sv micro vectors.
     */
    def p_r_s (): Unit =
        for i <- cmat.indices do
            val tp: Double = cmat(i, i)                              // true  positives for class i: y = i, yp = i 
            val fp: Double = csum(i) - tp                            // false positives for class i: y ≠ i, yp = i
            val fn: Double = rsum(i) - tp                            // false negatives for class i: y = i, yp ≠ i
            val tn: Double = m - (tp + fp + fn)                      // true  negatives for class i: y ≠ i, yp ≠ i

            pv(i) = tp / (tp + fp)                                   // micro-precision for class i
            rv(i) = tp / (tp + fn)                                   // micro-recall for class i
            sv(i) = tn / (tn + fp)                                   // micro-specificity for class i
        end for
        debug ("p_r_s", s" pv = $pv,\n rv = $rv,\n sv = $sv")
    end p_r_s

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Efron's pseudo R-squared value.  Override to McFadden's, etc.
     *  @param p1  the first parameter
     *  @param p2  the second parameter
     */
    def pseudo_rSq: Double =  1.0 - sse / sst

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the confusion matrix for k = 2 as a tuple (tn, fp, fn, tp).
     *  @param con  the confusion matrix (defaults to cmat)
     */
    def tn_fp_fn_tp (con: MatrixI = cmat): (Double, Double, Double, Double) =
        if k == 2 then
            (con(0, 0) /* tn */, con(0, 1) /* fp */,
             con(1, 0) /* fn */, con(1, 1) /* tp */)
        else (NaN, NaN, NaN, NaN)
    end tn_fp_fn_tp

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the accuracy of the classification, i.e., the fraction of correct
     *  classifications.  Note, the correct classifications tp_i are in the main
     *  diagonal of the confusion matrix.
     */
    def accuracy: Double = cmat.trace / cmat.sum.toDouble

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the mean ignoring NaN (Not-a-Number).
     *  @param x  the vector whose mean is sought
     */
    private def mean (x: VectorD): Double =
        var sum = 0.0
        var k   = 0
        for i <- x.indices if ! x(i).isNaN do { sum += x(i); k += 1 }
        sum / k
    end mean

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the F1-measure, i.e., the harmonic mean of the precision and recall.
     *  @param p  the precision
     *  @param r  the recall
     */
    def f1_measure (p: Double, r: Double): Double = (p * r * 2.0) / (p + r)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the micro-F1-measure vector, i.e., the harmonic mean of the precision and recall.
     */
    def f1v: VectorD = (pv * rv * 2.0) / (pv + rv)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute Cohen's kappa coefficient that measures agreement between
     *  actual y and predicted yp classifications.
     *  @see en.wikipedia.org/wiki/Cohen%27s_kappa
     */
    def kappa: Double =
        val freq_y  = new VectorI (k)
        val freq_yp = new VectorI (k)
        for i <- y.indices do
            freq_y(y(i))  += 1
            freq_yp(y(i)) += 1
        end for
        val pe = (freq_y dot freq_yp) / (y.dim * y.dim).toDouble
        val po = accuracy
        (po - pe) / (1.0 - pe)
    end kappa

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the Quality of Fit (QoF) measures corresponding to the labels given
     *  above in the fitLabel method.
     */
    def fit: VectorD =
        val (p, r, s) = (pv.last, rv.last, sv.last)                       // ordinary precision, recall and specificity

        VectorD (rSq, p_rSq, sst, sse, mse0, rmse, mae, kappa, accuracy,  // common QoF measures
                 mean (pv), mean (rv), mean (sv), mean (f1v),             // means of precision, recall, specificity and F1
                 p, r, s, f1_measure (p, r))                              // most meaningful when k = 2
    end fit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the Quality of Fit (QoF) vector micor-measures, i.e., measures for
     *  each class. 
     */
    def fitMicroMap: Map [String, VectorD] =
        val lab = fitLabel_v
        LinkedHashMap (lab(0) -> pv,                                      // mirco-precision vector
                       lab(1) -> rv,                                      // mirco-recall vector
                       lab(2) -> sv,                                      // mirco-specificity vector
                       lab(3) -> f1v)                                     // mirco-F1 vector
    end fitMicroMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the help string that describes the Quality of Fit (QoF) measures
     *  provided by the `FitC` class.  Override to correspond to fitLabel.
     */
    def help: String = FitC.help

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the labels for the Quality of Fit (QoF) measures. Override to
     *  add additional QoF measures.
     */
    def fitLabel_v: Seq [String] = FitC.fitLabel_v

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a summary report with diagnostics and the overall Quality of Fit (QoF).
     *  @param x_     the testing/full data/input matrix
     *  @param fname  the array of feature/variable names
     *  @param b      the parameters/pmf for the model
     *  @param vifs   the Variance Inflation Factors (VIFs)
     */
    def summary (x_ : MatrixD, fname: Array [String], b: VectorD, vifs: VectorD = null): String =
        val fit1 = FitM.fitMap (fit, QoFC.values.map (_.toString))
        val fit2 = fitMicroMap

        var sb = new StringBuilder ("-" * 58 + "\nSUMMARY")
        sb.append ("\n" + "-" * 58)
        sb.append ("\nfname = " + stringOf (fname))
        sb.append ("\n" + "-" * 58)
        sb.append ("\nparameter = " + b)
        sb.append ("\n" + "-" * 58)
        sb.append ("\nConfusion Matrix = " + cmat)
        sb.append ("\n" + "-" * 58)
        sb.append ("\nScalar QoF Measures")
        sb.append ("\n" + "-" * 58)
        for (k, v) <- fit1 do sb.append (s"\n\t $k \t= $v")
        sb.append ("\n" + "-" * 58)
        sb.append ("\nVector QoF Micro-Measures")
        sb.append ("\n" + "-" * 58)
        for (k, v) <- fit2 do sb.append (s"\n\t $k \t= $v")
        sb.append ("\n" + "-" * 58)
        sb.mkString
    end summary

end FitC


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TestFit` class can be used for comparing two vectors on the basis of QoF.
 *  @param y  the response vector
 *  @param k  the number of classes
 */
class TestFitC (y: VectorI, k: Int) extends FitC (y, k)


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `fitCTest` object is used to test the `FitC` trait.
 *  > runMain scalation.modeling.classifying.fitCTest
 */
@main def fitCTest (): Unit =

    val y  = VectorI (0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2)   // actual
    val yp = VectorI (0, 0, 0, 1, 2, 0, 0, 1, 1, 2, 0, 1, 1, 1, 2)   // predicted
    val k  = 3                                                       // three classes

    FitC.test (new TestFitC (y, k), y, yp, k)

end fitCTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `fitCTest2` object is used to test the `FitC` trait.
 *  @see www.quora.com/How-do-I-compute-precision-and-recall-values-for-a-dataset
 *  > runMain scalation.modeling.classifying.fitCTest2
 */
@main def fitCTest2 (): Unit =

    val y  = VectorI (1, 1, 1, 1, 1, 0, 0, 0, 0, 0)                   // actual
    val yp = VectorI (1, 0, 0, 1, 1, 1, 0, 0, 1, 1)                   // predicted
    val k  = 2                                                        // two classes

    FitC.test (new TestFitC (y, k), y, yp, k)

end fitCTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `fitCTest3` object is used to test the `FitC` class.
 *  @see towardsdatascience.com/multi-class-metrics-made-simple-part-i-precision-and-recall-9250280bddc2
 *  Note: ScalaTion's confusion matrix is the transpose of the one on the Website
 *  > runMain scalation.modeling.classifying.fitCTest3
 */
@main def fitCTest3 (): Unit =

    val k  = 3                                                        // three classes: cat, fish, hen
//                              y yp
    val yyp = MatrixI ((25, 2), 0, 0,                                 // 6 actual cats and their predictions
                                0, 0,
                                0, 0,
                                0, 0,
                                0, 1,
                                0, 2,

                                1, 0,                                  // 10 actual fish and their predictions
                                1, 0,
                                1, 0,
                                1, 0,
                                1, 0,
                                1, 0,
                                1, 1,
                                1, 1,
                                1, 2,
                                1, 2,
    
                                2, 0,                                  // 9 actual hens and their predictions
                                2, 0,
                                2, 0,
                                2, 2,
                                2, 2,
                                2, 2,
                                2, 2,
                                2, 2,
                                2, 2)

    println (FitC.help)
    val (y, yp) = (yyp(?, 0).toInt, yyp(?, 1).toInt)
    FitC.test (new TestFitC (y, k), y, yp, k)

end fitCTest3

