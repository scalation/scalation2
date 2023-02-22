
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Thu Mar 22 22:31:32 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model Support: Quality of Fit (QoF) suitable for all models
 *
 *  @see facweb.cs.depaul.edu/sjost/csc423/documents/f-test-reg.htm
 *  @see avesbiodiv.mncn.csic.es/estadistica/ejemploaic.pdf
 *  @see en.wikipedia.org/wiki/Bayesian_information_criterion
 *  @see www.forecastpro.com/Trends/forecasting101August2011.html
 */

package scalation
package modeling

import scala.collection.mutable.{LinkedHashMap, Map}
import scala.math.sqrt

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FitM` class provides methods to determine basic Quality of Fit 'QoF' measures
 *  suitable for all Models.
 */
trait FitM: 

    protected var m      = -1                                // number of instances (# data points)

    protected var sse    = -1.0                              // sum of squares for error (SSE or RSS)
    protected var ssr    = -1.0                              // sum of squares regression/model (SSR)
    protected var sst    = -1.0                              // sum of squares total (SST = SSR + SSE)

    protected var mse0   = -1.0                              // raw/MLE mean squared error (MSE0)
    protected var rmse   = -1.0                              // root mean squared error (RMSE)
    protected var mae    = -1.0                              // mean absolute error (MAE or MAD)
    protected var rSq    = -1.0                              // coefficient of determination R^2 using mean
    protected var rSq0   = -1.0                              // coefficient of determination R^2 using 0

    private val flaw  = flawf ("FitM")                       // flaw function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sum of the squares for error (sse).  Must call diagnose first.
     */
    def sse_ : Double = sse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the coefficient of determination (R^2).  Must call diagnose first.
     */
    def rSq_ : Double  = rSq                                // using mean 
    def rSq0_ : Double = rSq0                               // using 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Diagnose the health of the model by computing the Quality of Fit (QoF) measures,
     *  from the error/residual vector and the predicted & actual responses.
     *  For some models the instances may be weighted.
     *  @see `Regression_WLS`
     *  Must be overridden.
     *  @param y   the actual response/output vector to use (test/full)
     *  @param yp  the predicted response/output vector (test/full)
     *  @param w   the weights on the instances (defaults to null)
     */
    def diagnose (y: VectorD, yp: VectorD, w: VectorD = null): VectorD =
        m = y.dim                                           // size of response vector (test/full)
        if m < 2       then flaw ("diagnose", s"requires at least 2 responses to evaluate m = $m")
        if yp.dim != m then flaw ("diagnose", s"yp.dim = ${yp.dim} != y.dim = $m")

        val mu = y.mean                                     // mean of y (may be zero)
        val e  = y - yp                                     // residual/error vector
        sse    = e.normSq                                   // sum of squares for error
        if w == null then
            sst = (y - mu).normSq                           // sum of squares total (ssr + sse)
            ssr = sst - sse                                 // sum of squares regression/model
//          println (s"ssr = $ssr")
        else
            ssr = (w * (yp - (w * yp / w.sum).sum)~^2).sum  // regression sum of squares
            sst = ssr + sse
        end if

        mse0   = sse / m                                    // raw/MLE mean squared error
        rmse   = sqrt (mse0)                                // root mean squared error (RMSE)
        mae    = e.norm1 / m                                // mean absolute error
        rSq    = 1 - sse / sst                              // R^2 using mean
        rSq0   = 1 - sse / y.normSq                         // R^2 using 0
        e                                                   // returns error, overrides return QoF
    end diagnose

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the Quality of Fit (QoF) measures corresponding to the labels given.
     *  Note, if sse > sst, the model introduces errors and the rSq may be negative,
     *  otherwise, R^2 (rSq) ranges from 0 (weak) to 1 (strong).
     *  Override to add more quality of fit measures.
     */
    def fit: VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the help string that describes the Quality of Fit (QoF) measures
     *  provided by the `Fit` class.  Override to correspond to 'fitLabel'.
     */
    def help: String

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a QoF summary for a model with diagnostics for each predictor 'x_j'
     *  and the overall Quality of Fit (QoF).
     *  @param x_     the testing/full data/input matrix
     *  @param fname  the array of feature/variable names
     *  @param b      the parameters/coefficients for the model
     *  @param vifs   the Variance Inflation Factors (VIFs)
     */
    def summary (x_ : MatrixD, fname: Array [String], b: VectorD, vifs: VectorD = null): String

end FitM


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FitM` object provides functions for making fit maps for QoF measures.
 */
object FitM:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a map of quality of fit measures (use of `LinkedHashMap` makes it ordered).
     *  @param  ftVec  the vector of QoF values
     *  @param  ftLab  the array of QoF labels
     */
    def fitMap (ftVec: VectorD, ftLab: Array [String]): Map [String, String] =
        val lm = LinkedHashMap [String, String] ()                          // empty list map
        for i <- ftLab.indices do lm += ftLab(i) -> fmt(ftVec(i))
        lm
    end fitMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a map of quality of fit measures (use of `LinkedHashMap` makes it ordered).
     *  @param  ftMat  the matrix of QoF values
     *  @param  ftLab  the array of QoF labels
     */
    def fitMap (ftMat: MatrixD, ftLab: Array [String]): Map [String, String] =
        val lm = LinkedHashMap [String, String] ()                          // empty list map
        for i <- ftLab.indices do lm += ftLab(i) -> (ftMat(i).toString + "\n")
        lm
    end fitMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the quality of fit measures for each response/output variable.
     *  @param  ftMat  the matrix of QoF values
     *  @param  ftLab  the array of QoF labels
     */
    def showFitMap (ftMat: MatrixD, ftLab: Array [String]): String =
        val sb = StringBuilder ("\n")
        for i <- ftLab.indices do sb ++= s"\t\t${ftLab(i)} \t -> ${ftMat(i)} \n"
        sb.toString
    end showFitMap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the table storing the statistics for QoF measures.
     *  @param stats  the table of statistics for QoF measures
     */
    def showQofStatTable (stats: Array [Statistic]): Unit =
        banner ("showQofStatTable: Statistical Table for QoF")
        val slabels = Statistic.labels
        println (Statistic.labels)
        for i <- stats.indices do
            if i == 0 then println ("-" * 88)
            println (stats(i))
        end for
        println ("-" * 88)
    end showQofStatTable

end FitM

