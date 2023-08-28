
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Thu Mar 22 22:31:32 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model Support: Quality of Fit (QoF)
 *
 *  @see facweb.cs.depaul.edu/sjost/csc423/documents/f-test-reg.htm
 *  @see avesbiodiv.mncn.csic.es/estadistica/ejemploaic.pdf
 *  @see en.wikipedia.org/wiki/Bayesian_information_criterion
 *  @see www.forecastpro.com/Trends/forecasting101August2011.html
 */

package scalation
package modeling

import scala.math.{abs, log, sqrt}
import scala.runtime.ScalaRunTime.stringOf

import scalation.mathstat._
import scalation.random.CDF.{fisherCDF, studentTCDF}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QoF` enum defines the Quality of Fit (QoF) measures
 */
enum QoF (name: String):

    case rSq    extends QoF ("rSq")                         // index  0
    case rSqBar extends QoF ("rSqBar")                      // index  1
    case sst    extends QoF ("sst")                         // index  2
    case sse    extends QoF ("sse")                         // index  3
    case mse0   extends QoF ("mse0")                        // index  4
    case rmse   extends QoF ("rmse")                        // index  5
    case mae    extends QoF ("mae")                         // index  6

    case dfm    extends QoF ("dfm")                         // index  7
    case df     extends QoF ("df")                          // index  8
    case fStat  extends QoF ("fStat")                       // index  9
    case aic    extends QoF ("aic")                         // index 10
    case bic    extends QoF ("bic")                         // index 11
    case mape   extends QoF ("mape")                        // index 12
    case smape  extends QoF ("smape")                       // index 13
//  case mase   extends QoF ("mase")                        // index 14

end QoF

import QoF._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fit` companion object provides factory methods for assessing quality of
 *  fit for standard types of modeling techniques.
 */
object Fit:

    val MIN_FOLDS = 3                                                       // minimum number of folds for cross-validation
    val N_QoF     = QoF.values.size                                         // the number of QoF measures

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the help string that describes the Quality of Fit (QoF) measures
     *  provided by the `Fit` trait.  The QoF measures are divided into two groups:
     *  general and statistical (that often require degrees of freedom and/or
     *  log-likelihoods).
     *  @see www.ncbi.nlm.nih.gov/pmc/articles/PMC5570302/
     *  @see https://en.wikipedia.org/wiki/Coefficient_of_determination
     */
    def help: String =
        """
help: Quality of Fit (QoF) measures:
    rSq    =  R-squared, the Coefficient of Determination (R^2)
    rSqBar =  adjusted R-squared (R^2-bar)
    sst    =  Sum of Squares Total (ssr + sse)
    sse    =  Sum of Squares for Error (SSE = RSS)
    mse0   =  raw Mean Square Error (MSE = SSE / m)
    rmse   =  Root Mean Square Error (RMSE)
    mae    =  Mean Absolute Error (MAE)

    dfm    =  degrees of freedom taken by the model, e.g., one lost per parameter
    df     =  degrees of freedom left for residuals
    fStat  =  Fisher's statistic
    aic    =  Akaike Information Criterion (AIC)
    bic    =  Bayesian Information Criterion (BIC)
    mape   =  Mean Absolute Percentage Error (MAPE)
    smape  =  symmetric Mean Absolute Percentage Error (sMAPE)
    mase   =  Mean Absolute Scaled Error (optional)
        """
    end help

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Collect QoF results for a model and return them in a vector.
     *  @param fit     the fit vector with regard to the training set
     *  @param cv_fit  the fit array of statistics for cross-validation (upon test sets)
     */
    def qofVector (fit: VectorD, cv_fit: Array [Statistic]): VectorD =
        val cv = if cv_fit == null then -0.0                                // cv not computed
                 else cv_fit(rSq.ordinal).mean                              // mean for R^2 cv
        VectorD (100 * fit(rSq.ordinal),                                    // R^2 as percentage
                 100 * fit(rSqBar.ordinal),                                 // R^2 Bar as percentage
                 fit(smape.ordinal),                                        // sMAPE
                 100 * cv)                                                  // R^2 cv as percentage
    end qofVector

    val qofVectorSize = 4                                                   // must correspond to size of qofVector

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a table to store statistics for QoF measures, where each row corresponds
     *  to the statistics on a particular QoF measure, e.g., rSq.
     */
    def qofStatTable: Array [Statistic] =
        val stats = Array.ofDim [Statistic] (N_QoF)                         // for collecting stats on QoF measures
        for i <- stats.indices do stats(i) = new Statistic (values(i).toString, unbiased = true)
        stats
    end qofStatTable

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tally the current QoF measures into the statistical accumulators.
     *  @param stats  the statistics table being updated
     *  @param qof    the current QoF measure vector
     */
    def tallyQof (stats: Array [Statistic], qof: VectorD): Unit =
        if qof(sst.ordinal) > 0.0 then                                      // requires variation in test set
            for q <- qof.indices do stats(q).tally (qof(q))                 // tally these QoF measures
        end if
    end tallyQof

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the Mean Absolute Error (MAE) for the forecasting model under test.
     *  @param y   the given time-series (must be aligned with the forecast)
     *  @param yp  the forecasted time-series
     *  @param h   the forecasting horizon or stride (defaults to 1)
     */
    def mae (y: VectorD, yp: VectorD, h: Int = 1): Double =
//      println (s"mae: y.dim = ${y.dim}, yp.dim = ${yp.dim}")
        var sum = 0.0
        for t <- h until y.dim do sum += abs (y(t) - yp(t-h))
        sum / yp.dim
    end mae

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the Mean Absolute Error (MAE) for the Naive Model (simple random walk)
     *  with horizon/stride h.  For comparison with the above method.
     *  @param y  the given time-series
     *  @param h  the forecasting horizon or stride (defaults to 1)
     */
    def mae_n (y: VectorD, h: Int = 1): Double =
        var sum = 0.0
        for t <- h until y.dim do sum += abs (y(t) - y(t-h))
        sum / (y.dim - h)
    end mae_n

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the Mean Absolute Scaled Error (MASE) for the given time-series.
     *  It is the ratio of MAE of the forecasting model under test and the MAE of
     *  the Naive Model (simple random walk).
     *  @param y   the given time-series (must be aligned with the forecast)
     *  @param yp  the forecasted time-series
     *  @param h   the forecasting horizon or stride (defaults to 1)
     */
    def mase (y: VectorD, yp: VectorD, h: Int = 1): Double =
        mae (y, yp, h) / mae_n (y, 1)                          // compare to Naive (one-step)
//      mae (y, yp, h) / mae_n (y, h)                          // compare to Naive (h-steps)
    end mase

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the symmetric Mean Absolute Percentage Error (sMAPE) score.
     *  @param y   the given time-series (must be aligned with the forecast)
     *  @param yp  the forecasted time-series
     */
    def smapeF (y: VectorD, yp: VectorD): Double =
        val e = y - yp
        200 * (e.abs / (y.abs + yp.abs)).sum / y.dim
    end smapeF

end Fit


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fit` trait provides methods to determine basic Quality of Fit QoF measures.
 *  @see reset to reset the degrees of freedom
 *  @param dfm  the degrees of freedom for model/regression
 *  @param df   the degrees of freedom for error
 */
trait Fit (private var dfm: Double, private var df: Double)
      extends FitM:

    private val debug = debugf ("Fit", false)                  // debug function
    private val flaw  = flawf  ("Fit")                         // flaw function

    private var df_t   = dfm + df                              // total degrees of freedom
    private var r_df   = df_t / df                             // ratio of degrees of freedom (total / error)

    private var mse    = -1.0                                  // mean of squares for error MSE (unbiased)
    private var rse    = -1.0                                  // residual standard error (RSE)
    private var msr    = -1.0                                  // mean of squares for regression/model (MSR)
    private var rSqBar = -1.0                                  // adjusted R-squared (R^2 Bar)
    private var fStat  = -1.0                                  // F statistic (Quality of Fit)
    private var p_fS   = -1.0                                  // p-value for fStat 
    private var aic    = -1.0                                  // Akaike Information Criterion (AIC)
    private var bic    = -1.0                                  // Bayesian Information Criterion (BIC)

    // Measures used for time series @see www.forecastpro.com/Trends/forecasting101August2011.html
    private var mape   = -1.0                                  // Mean Absolute Percentage Error (MAPE)
    private var smape  = -1.0                                  // symmetric Mean Absolute Percentage Error (sMAPE)
//  private var mase   = -1.0                                  // Mean Absolute Scaled Error (MASE)
//  private var nmae   = -1.0                                  // normalized MAE (MAD/Mean Ratio)

    protected var sig2e = -1.0                                 // MLE estimate of the population variance on the residuals 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the degrees of freedom to the new updated values.  For some models,
     *  the degrees of freedom is not known until after the model is built.
     *  @param df_update  the updated degrees of freedom (model, error)
     */
    def resetDF (df_update: (Double, Double)): Unit =
        dfm  = df_update._1; df = df_update._2                 // degrees of freedom
        df_t = dfm + df                                        // total degrees of freedom
        r_df = df_t / df                                       // ratio of degrees of freedom (total / error)
        debug ("resetDF", s"dfm = $dfm, df = $df")
    end resetDF

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the mean of the squares for error (sse / df).  Must call diagnose first.
     */
    def mse_ : Double = mse

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Diagnose the health of the model by computing the Quality of Fit (QoF) measures,
     *  from the error/residual vector and the predicted & actual responses.
     *  For some models the instances may be weighted.
     *  @see `Regression_WLS`
     *  @param y   the actual response/output vector to use (test/full)
     *  @param yp  the predicted response/output vector (test/full)
     *  @param w   the weights on the instances (defaults to null)
     */
    override def diagnose (y: VectorD, yp: VectorD, w: VectorD = null): VectorD =
        val e = super.diagnose (y, yp, w)

        if dfm <= 0 || df <= 0 then
            flaw ("diagnose", s"degrees of freedom dfm = $dfm and df = $df must be strictly positive")
        if dfm == 0 then dfm = 1                               // must have at least 1 DoF,                 // FIX - check
                                                               // e.g., b_0 or  b_0 + b_1 x_1 or b_1 x_1
        msr    = ssr / dfm                                     // mean of squares for regression/model
        mse    = sse / df                                      // mean of squares for error

        rse    = sqrt (mse)                                    // residual standard error
        rSqBar = 1 - (1-rSq) * r_df                            // adjusted R-squared
        fStat  = msr / mse                                     // F statistic (quality of fit)
        p_fS   = 1.0 - fisherCDF (fStat, dfm.toInt, df.toInt)  // p-value for fStat 
        if p_fS.isNaN then p_fS = 0.0                          // NaN => check error message produced by fisherCDF
        if sig2e == -1.0 then sig2e = e.variance_

        val ln_m = log (m)                                     // natural log of m (ln(m))
        aic    = ll() + 2 * (dfm + 1)                          // Akaike Information Criterion
                                                               //   the + 1 on dfm accounts for the sig2e, which is
                                                               //   an additional parameter to be estimated in MLE
        bic    = aic + (dfm + 1) * (ln_m - 2)                  // Bayesian Information Criterion
        mape   = 100 * (e.abs / y.abs).sum / m                 // mean absolute percentage error
        smape  = 200 * (e.abs / (y.abs + yp.abs)).sum / m      // symmetric mean absolute percentage error
//      mase   = Fit.mase (y, yp)                              // mean absolute scaled error
        fit
    end diagnose

//      nmae   = mae / mu                                      // normalized MAE (MAD/Mean Ratio)
//      nrmse  = rmse / mu                                     // normalized RMSE
//  issues concerning mean: full, train or test?
//      val ym   = if ym_ == -0.0 then { debug ("diagnose", "test mean"); mu }
//                 else                { debug ("diagnose", "train mean"); ym_ }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The log-likelihood function times -2.  Override as needed.
     *  @see www.stat.cmu.edu/~cshalizi/mreg/15/lectures/06/lecture-06.pdf
     *  @see www.wiley.com/en-us/Introduction+to+Linear+Regression+Analysis%2C+5th+Edition-p-9780470542811
     *       Section 2.11
     *  @param ms  raw Mean Squared Error
     *  @param s2  MLE estimate of the population variance of the residuals
     */
    def ll (ms: Double = mse0, s2: Double = sig2e, m2: Int = m): Double =
        -m2 / 2.0 * (log (_2Pi) + log (s2) + ms / s2)
    end ll

//  def ll (ms: Double = mse0, s2: Double = sig2e): Double = m * (log (_2Pi) + log (s2) + ms / s2)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the Quality of Fit (QoF) measures corresponding to the labels given.
     *  Note, if sse > sst, the model introduces errors and the rSq may be negative,
     *  otherwise, R^2 (rSq) ranges from 0 (weak) to 1 (strong).
     *  Override to add more quality of fit measures.
     */
    def fit: VectorD = VectorD (rSq, rSqBar, sst, sse, mse0, rmse, mae,
                                dfm, df, fStat, aic, bic, mape, smape)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the help string that describes the Quality of Fit (QoF) measures
     *  provided by the `Fit` trait.  Override to correspond to fitLabel.
     */
    def help: String = Fit.help

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a QoF summary for a model with diagnostics for each predictor x_j
     *  and the overall Quality of Fit (QoF).
     *  Note: `Fac_Cholesky is used to compute the inverse of xtx.
     *  @param x_     the testing/full data/input matrix
     *  @param fname  the array of feature/variable names
     *  @param b      the parameters/coefficients for the model
     *  @param vifs   the Variance Inflation Factors (VIFs)
     */
    def summary (x_ : MatrixD, fname: Array [String], b: VectorD, vifs: VectorD = null): String =

        val facCho = new Fac_Cholesky (x_.transpose * x_)      // create a Cholesky factorization of xtx
        val diag   = facCho.inverse(?)                         // take inverse and get main diagonal
        val stdErr = (diag * mse_).sqrt                        // standard error of coefficients

        val stats = (sumCoeff (b, stdErr, vifs), fmt(rse), fmt(rSq), fmt(rSqBar))
        debug ("summary", s"stats = $stats")

        (if fname != null then "fname = " + stringOf (fname) else "") +
        s"""
SUMMARY
    Parameters/Coefficients:
    Var      Estimate    Std. Error \t t value \t Pr(>|t|) \t VIF
----------------------------------------------------------------------------------
${stats._1}
    Residual standard error: ${stats._2} on $df degrees of freedom
    Multiple R-squared:  ${stats._3},	Adjusted R-squared:  ${stats._4}
    F-statistic: $fStat on $dfm and $df DF,  p-value: $p_fS
----------------------------------------------------------------------------------
        """
    end summary

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce the summary report portion for the parameters/coefficients.
     *  @param b       the parameters/coefficients for the model
     *  @param stdErr  the standard error for parameters/coefficients
     *  @param vf      the Variance Inflation Factors (VIFs)
     */
    private def sumCoeff (b: VectorD, stdErr: VectorD, vf: VectorD): String =
        debug ("sumCoeff", s"stdErr = $stdErr")
        var t, p: VectorD = null
        if stdErr != null then
            t  = b / stdErr                                           // Student's T statistic
            p  = if df > 0 then t.map ((x: Double) => 2.0 * studentTCDF (-abs (x), df))   // p value
                 else -VectorD.one (b.dim)
        end if
        val sb = new StringBuilder ()
        for j <- b.indices do
            sb.append ("    x" + j + "\t " + fmt(b(j)) +
                      ( if stdErr != null then
                            val p_j = if p(j).isNaN then 0.0 else p(j)
                            "\t " + fmt(stdErr(j)) +
                            "\t " + fmt(t(j)) +
                            "\t " + fmt(p_j) +
                            "\t " + (if j == 0 || vf == null then "NA" else fmt(vf(j-1))) + "\n"
                        else "?") )
        end for
        sb.mkString
    end sumCoeff

end Fit


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TestFit` class can be used for comparing two vectors on the basis of QoF.
 *  The degrees of freedom (dfm) for the "model" is assumed to be 1.
 *  Can be used when the degrees of freedom are not known.
 *  @param m  the size of vectors to compare
 */
class TestFit (m: Int) extends Fit (1, m-1)


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `fitTest` main function is use to test the `Fit` trait.
 *  > runMain scalation.modeling.fitTest
 */
@main def fitTest (): Unit =

//  import scalation.random.Normal

    for sig2 <- 10 to 50 by 10 do
//      val rv = Normal (0, sig2)
        val rv = SimpleUniform (-sig2, sig2)
        val y  = VectorD.range (1, 101) + 10.0
        val yp = y.map (_ + rv.gen) 
        val e  = y - yp
        new Plot (null, y, yp, "plot y and yp")
        new Plot (null, e, null, "plot e")

        val ft = new TestFit (y.dim)
        ft.diagnose (y, yp)
        println (FitM.fitMap (ft.fit, QoF.values.map (_.toString)))
    end for

end fitTest

