
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Thu Mar 22 22:31:32 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Support: Quality of Fit (QoF)
 *
 *  @see facweb.cs.depaul.edu/sjost/csc423/documents/f-test-reg.htm
 *  @see avesbiodiv.mncn.csic.es/estadistica/ejemploaic.pdf
 *  @see en.wikipedia.org/wiki/Bayesian_information_criterion
 *  @see www.forecastpro.com/Trends/forecasting101August2011.html
 */

package scalation
package modeling

import scala.collection.mutable.Map
import scala.math.{abs, log, sqrt}
import scala.runtime.ScalaRunTime.stringOf

import scalation.mathstat._
import scalation.random.CDF.{fisherCDF, studentTCDF}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `QoF` enum defines the Quality of Fit (QoF) measures/metrics.
 *  @param name  the name of the parameter
 */
enum QoF (val name: String):

    case rSq     extends QoF ("rSq")                         // index  0
    case rSqBar  extends QoF ("rSqBar")                      // index  1
    case sst     extends QoF ("sst")                         // index  2
    case sse     extends QoF ("sse")                         // index  3

    case sde     extends QoF ("sde")                         // index  4
    case mse0    extends QoF ("mse0")                        // index  5
    case rmse    extends QoF ("rmse")                        // index  6
    case mae     extends QoF ("mae")                         // index  7
    case smape   extends QoF ("smape")                       // index  8

    case m       extends QoF ("m")                           // index  9
    case dfm     extends QoF ("dfm")                         // index 10
    case df      extends QoF ("df")                          // index 11
    case fStat   extends QoF ("fStat")                       // index 12
    case aic     extends QoF ("aic")                         // index 13
    case bic     extends QoF ("bic")                         // index 14

    case mape    extends QoF ("mape")                        // index 15
    case mase    extends QoF ("mase")                        // index 16
    case smapeIC extends QoF ("smapeIC")                     // index 17

    case picp    extends QoF ("picp")                        // index 18 
    case pinc    extends QoF ("pinc")                        // index 19
    case ace     extends QoF ("ace")                         // index 20
    case pinaw   extends QoF ("pinaw")                       // index 21
    case pinad   extends QoF ("pinad")                       // index 22
    case iscore  extends QoF ("iscore")                      // index 23
    case wis     extends QoF ("wis")                         // index 24

end QoF

val qoF_names = QoF.values.map (_.toString)                 // The QoF names from the QoF enum

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
     *  @see en.wikipedia.org/wiki/Coefficient_of_determination
     */
    def help: String =
        """
help: Quality of Fit (QoF) metrics/measures:
    rSq     =  R-squared, the Coefficient of Determination (R^2)
    rSqBar  =  adjusted R-squared (R^2-bar)
    sst     =  Sum of Squares Total (ssr + sse)
    sse     =  Sum of Squares for Error (SSE = RSS)

    sde     =  Standard Deviation of Errors
    mse0    =  raw Mean Square Error (MSE = SSE / m)
    rmse    =  Root Mean Square Error (RMSE)
    mae     =  Mean Absolute Error (MAE)
    smape   =  symmetric Mean Absolute Percentage Error (sMAPE)

    m       =  Number of Observations
    dfm     =  Degrees of Freedom taken by the model, e.g., one lost per parameter
    df      =  Degrees of Freedom left for residuals/errors
    fStat   =  Fisher's Statistic
    aic     =  Akaike Information Criterion (AIC)
    bic     =  Bayesian Information Criterion (BIC)

    mape    =  Mean Absolute Percentage Error (MAPE)
    mase    =  Mean Absolute Scaled Error (MASE)
    smapeIC =  symmetric Mean Absolute Percentage Error Information Criterion (sMAPE-IC)

    picp    =  prediction interval coverage probability
    pinc    =  prediction interval nominal coverage
    ace     =  average coverage error
    pinaw   =  prediction interval normalized average width
    pinad   =  prediction interval normalized average deviation
    iscore  =  interval score
    wis     =  weighted interval score
        """
    end help

    val maxi = Set (QoF.rSq.ordinal, QoF.rSqBar.ordinal)                    // maximize these QoF metrics (min the rest)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a contrary/starting value, -∞ for maximization, ∞ for minimization
     *  @param qk  the QoF metric index/ordinal value
     */
    inline def extreme (qk: Int): Double = if maxi contains qk then NEGATIVE_INFINITY
                                           else POSITIVE_INFINITY

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
    inline def mae (y: VectorD, yp: VectorD, h: Int = 1): Double =
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
    /** Return the Prediction Interval Coverage Probability (PICP) metric, i.e.,
     *  the fraction is actual values inside the prediction interval.
     *  @param y    the given time-series (must be aligned with the interval forecast)
     *  @param low  the lower bound
     *  @param up   the upper bound
     */
    inline def picp_ (y: VectorD, low: VectorD, up: VectorD): Double =
        var count = 0
        for i<- y.indices if y(i) in (low(i), up(i)) do count += 1
        count / y.dim.toDouble
    end picp_

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the Prediction Interval Normalised Average Deviation (PINAD) metric, i.e.,
     *  the normalized (by range) average deviation outside the prediction interval.
     *  @param y    the given time-series (must be aligned with the interval forecast)
     *  @param low  the lower bound
     *  @param up   the upper bound
     */
    inline def pinad_ (y: VectorD, low: VectorD, up: VectorD): Double =
        var sum = 0.0
        for i <- y.indices do
            sum += (if y(i) < low(i) then low(i) - y(i)
                    else if y(i) > up(i) then y(i) - up(i)
                    else 0.0)
        sum / (y.dim * (y.max - y.min))
    end pinad_

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the Interval Score (IS) metric, i.e., the ...
     *  @see arxiv.org/pdf/2005.12881.pdf
     *  @param y      the given time-series (must be aligned with the interval forecast)
     *  @param low    the lower bound
     *  @param up     the upper bound
     &  @param alpha  the prediction level
     */
    def iscore_ (y: VectorD, low: VectorD, up: VectorD, alpha: Double = 0.1): Double =
        val fac = 2.0 / alpha
        var sum = 0.0
        for i <- y.indices do
            sum += up(i) - low(i)                                 // interval width
            if y(i) < low(i) then sum += fac * (low(i) - y(i))    // y_i below interval penalty
            if y(i) > up(i)  then sum += fac * (y(i) - up(i))     // y_i above interval penalty
        sum / y.dim
    end iscore_

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the Weighted Interval Score (WIS) metric, i.e., the ...
     *  @see arxiv.org/pdf/2005.12881.pdf
     *  @param y       the given time-series (must be aligned with the interval forecast)
     *  @param yp      the point prediction mean/median
     *  @param low     the lower bounds for various alpha levels
     *  @param up      the upper bounds for various alpha levels
     *  @param alphas  the array of prediction levels
     */
    def wis_ (y: VectorD, yp: VectorD, low: MatrixD, up: MatrixD,
              alphas: Array [Double] =
              Array (0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)): Double =
        val k = alphas.size
        var sum = alphas(0) * (y - yp).abs.mean
        for j <- 1 until k do sum += alphas(j) * iscore_ (y, low(j), up(j), alphas(j))
        sum / (2 * k + 1)
    end wis_

end Fit

import Fit._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Fit` trait provides methods to determine basic Quality of Fit QoF measures.
 *  @see reset to reset the degrees of freedom
 *  @param dfm  the degrees of freedom for model/regression
 *  @param df   the degrees of freedom for error
 */
trait Fit (protected var dfm: Double, protected var df: Double)
      extends FitM:

    private val debug   = debugf ("Fit", false)                 // debug function
    private val flaw    = flawf  ("Fit")                        // flaw function

    private val pIC     = 2.0                                   // penalty multiplier for sMAPE IC
    private var df_t    = dfm + df                              // total degrees of freedom
    private var r_df    = if df > 1.0 then df_t / df            // ratio of degrees of freedom (total / error)
                         else dfm + 1.0                         // case for for less than 1 dof error

    private var mse     = -1.0                                  // mean of squares for error MSE (unbiased)
    private var rse     = -1.0                                  // residual standard error (RSE)
    private var msr     = -1.0                                  // mean of squares for regression/model (MSR)

    private var rSqBar  = -1.0                                  // adjusted R-squared (R^2 Bar)
    private var fStat   = -1.0                                  // F statistic (Quality of Fit)
    private var p_fS    = -1.0                                  // p-value for fStat 
    private var aic     = -1.0                                  // Akaike Information Criterion (AIC)
    private var bic     = -1.0                                  // Bayesian Information Criterion (BIC)

    // Measures used for time series @see www.forecastpro.com/Trends/forecasting101August2011.html
    private var mape    = -1.0                                  // Mean Absolute Percentage Error (MAPE)
    private var mase    = -1.0                                  // Mean Absolute Scaled Error (MASE)
    private var smapeIC = -1.0                                  // symmetric Mean Absolute Percentage Error Information Criteria (sMAPE-IC)
//  private var nmae    = -1.0                                  // normalized MAE (MAD/Mean Ratio)

    private var picp    = -1.0                                  // prediction interval coverage probability
    private var pinc    = -1.0                                  // prediction interval nominal coverage
    private var ace     = -1.0                                  // average coverage error
    private var pinaw   = -1.0                                  // prediction interval normalized average width
    private var pinad   = -1.0                                  // prediction interval normalized average deviation
    private var iscore  = -1.0                                  // interval score
    private var wis     = -1.0                                  // weighted interval score

    protected var sig2e = -1.0                                  // MLE estimate of the population variance on the residuals 

    protected var yForm: Transform = null                      // optional transformation of the response variable y
    protected var scaledMetrics: Boolean = false                // whether to use scaled metrics (otherwise use the default orifinal scale)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the the y-transformation.
     */
    def getYForm: Transform = yForm

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the degrees of freedom to the new updated values.  For some models,
     *  the degrees of freedom is not known until after the model is built.
     *  @param df_update  the updated degrees of freedom (model, error)
     */
    def resetDF (df_update: (Double, Double)): Unit =
        dfm  = df_update._1; df = df_update._2                 // degrees of freedom
        df_t = dfm + df                                        // total degrees of freedom
        r_df = if df > 1.0 then df_t / df                      // ratio of degrees of freedom (total / error)
               else dfm + 1.0                                  // case for for less than 1 dof error
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
     *  @param y_   the actual response/output vector to use (test/full)
     *  @param yp_  the predicted response/output vector (test/full)
     *  @param w    the weights on the instances (defaults to null)
     */
    override def diagnose (y_ : VectorD, yp_ : VectorD, w: VectorD = null): VectorD =
        val (y, yp) = if scaledMetrics || yForm == null then (y_, yp_)
                      else (yForm.fi(y_), yForm.fi(yp_))
        super.diagnose (y, yp, w)                                 // compute `FitM` metrics

        val e = y - yp                                            // FIX - avoid computing twice
//      println (s"Fit.diagnose:\n y = $y,\n yp = $yp,\n e = $e")

        if dfm < 0 || df < 0 then
            flaw ("diagnose", s"degrees of freedom dfm = $dfm and df = $df must be non-negative")

        msr    = if dfm == 0 then 0.0 else ssr / dfm              // mean squared regression/model
        mse    = sse / df                                         // mean squares error

        rse    = sqrt (mse)                                       // residual standard error
        rSqBar = 1 - (1-rSq) * r_df                               // adjusted R-squared

        fStat  = msr / mse                                        // F statistic (quality of fit)
        p_fS = if dfm == 0 then -0.0
               else 1.0 - fisherCDF (fStat, dfm.toInt, df.toInt)  // p-value for fStat   
        if p_fS.isNaN then p_fS = -0.0                            // NaN => check error message produced by fisherCDF

        if sig2e == -1.0 then sig2e = e.variance_

        val ln_m = log (m)                                        // natural log of m (ln(m))
        aic    = ll() + 2 * (dfm + 1)                             // Akaike Information Criterion
                                                                  //   the + 1 on dfm accounts for the sig2e, which is
                                                                  //   an additional parameter to be estimated in MLE
        bic    = aic + (dfm + 1) * (ln_m - 2)                     // Bayesian Information Criterion
        mape   = 100 * (e.abs / y.abs).sum / m                    // mean absolute percentage error
        mase   = Fit.mase (y, yp)                                 // mean absolute scaled error
        smapeIC = smape + pIC * (dfm + 1) / y.dim.toDouble        // sSMAPE Information Criterion
        fit
    end diagnose

//      nmae   = mae / mu                                         // normalized MAE (MAD/Mean Ratio)
//      nrmse  = rmse / mu                                        // normalized RMSE
//  issues concerning mean: full, train or test?
//      val ym   = if ym_ == -0.0 then { debug ("diagnose", "test mean"); mu }
//                 else                { debug ("diagnose", "train mean"); ym_ }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Diagnose the health of the model by computing the Quality of Fit (QoF) metrics/measures,
     *  from the error/residual vector and the predicted & actual responses.
     *  For some models the instances may be weighted.  Include interval measures.
     *  Note: `wis` should be computed separately.
     *  @see `Regression_WLS`
     *  @param y      the actual response/output vector to use (test/full)
     *  @param yp     the point prediction mean/median
     *  @param low    the predicted lower bound
     *  @param up     the predicted upper bound
     *  @param alpha  the nominal level of uncertainty (alpha) (defaults to 0.9, 90%)
     *  @param w      the weights on the instances (defaults to null)
     */
    def diagnose_ (y: VectorD, yp: VectorD, low: VectorD, up: VectorD, alpha: Double = 0.1,
                   w: VectorD = null): VectorD =
        diagnose (y, yp, w)

        picp   = picp_ (y, low, up)                            // prediction interval coverage probability
        pinc   = 1 - alpha                                     // prediction interval nominal coverage
        ace    = picp - pinc                                   // average coverage error
        pinaw  = (up - low).mean / (y.max - y.min)             // prediction interval normalized average width
        pinad  = pinad_ (y, low, up)                           // prediction interval normalized average deviation
        iscore = iscore_ (y, low, up)                          // interval score
        fit
    end diagnose_

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Diagnose the health of the model by computing the Quality of Fit (QoF) measures,
     *  @param y       the given time-series (must be aligned with the interval forecast)
     *  @param yp      the point prediction mean/median
     *  @param low     the lower bounds for various alpha levels
     *  @param up      the upper bounds for various alpha levels
     *  @param alphas  the array of prediction levels
     */
    def diagnose_wis (y: VectorD, yp: VectorD, low: MatrixD, up: MatrixD,
                      alphas: Array [Double] =
                      Array (0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)): Double =
        wis = wis_ (y, yp, low, up, alphas)
        wis
    end diagnose_wis

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
    override def fit: VectorD = VectorD (rSq, rSqBar, sst, sse, sde, mse0, rmse, mae,
                                         smape, m, dfm, df, fStat, aic, bic, mape, mase, smapeIC,
                                         picp, pinc, ace, pinaw, pinad, iscore, wis)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the Quality of Fit (QoF) measures corresponding to the labels given.
     *  Override to add more quality of fit measures.
     */
//  def fit_ : VectorD = fit ++ VectorD (picp, pinc, ace, pinaw, pinad, iscore, wis)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the prediction interval forecasts and relevant QoF metrics/measures.
     *  @param yy       the aligned actual response/output vector to use (test/full)
     *  @param yfh      the forecasts for horizon h
     *  @param low      the predicted lower bound
     *  @param up       the predicted upper bound
     *  @param qof_all  all the QoF metrics (for point and interval forecasts)
     *  @param h        the forecasting horizon
     */
    def show_interval_forecasts (yy: VectorD, yfh: VectorD,
                                 low: VectorD, up: VectorD,
                                 qof_all: VectorD, h: Int): Unit =
        println (FitM.fitMap (qof_all, qoF_names))                     // fully evaluate h-steps ahead forecasts
        new PlotM (null, MatrixD (yy, yfh, low, up),                   // aligned actual, forecasted, lower, upper
                   Array ("yy", "yfh", "low", "up"),
                   "Plot Prediction Intervals for horizon $h", lines = true)
    end show_interval_forecasts

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the help string that describes the Quality of Fit (QoF) measures
     *  provided by the `Fit` trait.  Override to correspond to fitLabel.
     */
    override def help: String = Fit.help

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a QoF summary for a model with diagnostics for each predictor x_j
     *  and the overall Quality of Fit (QoF).
     *  Note: `Fac_Cholesky is used to compute the inverse of xtx.
     *  @param x_     the testing/full data/input matrix
     *  @param fname  the array of feature/variable names
     *  @param b      the parameters/coefficients for the model
     *  @param vifs   the Variance Inflation Factors (VIFs)
     */
    override def summary (x_ : MatrixD, fname: Array [String], b: VectorD, vifs: VectorD = null): String =

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
class TestFit (m: Int) extends Fit (1, m-1):

    def testDiagnose (y: VectorD, yp: VectorD): Map [String, String] =
        FitM.fitMap (diagnose (y, yp), QoF.values.map (_.toString))
    end testDiagnose

end TestFit
 

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `fitTest` main function is used to test the `Fit` trait on a simulated dataset.
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

        println (new TestFit (y.dim).testDiagnose (y, yp))
    end for

end fitTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `fitTest2` main function is used to test the `Fit` class on a simulated
 *  time series.
 *  @see `scalation.modeling.forecasting.randomWalkTest3` for another test case
 *  > runMain scalation.modeling.fitTest2
 */
@main def fitTest2 (): Unit =

    import scalation.random.Normal

    for sig2 <- 10 to 50 by 10 do
        val rv  = Normal (0, sig2)
        val w   = math.sqrt (sig2) * 1.96
        val yp  = VectorD.range (1, 101) + 10.0
        val y   = yp.map (_ + rv.gen)                           // simulated time series
        val low = yp.map (_ - w)
        val up  = yp.map (_ + w)
        new PlotM (null, MatrixD (y, yp, low, up), Array ("y", "yp", "low", "up"), "plot y, low and up")

        object ft extends Fit (1, y.dim)
        ft.diagnose_ (y, yp, low, up)
        ft.diagnose_wis (y, yp, MatrixD (low), MatrixD (up), Array (0.1))
        val qof = ft.fit
        println (FitM.fitMap (qof, qoF_names))
    end for

end fitTest2
