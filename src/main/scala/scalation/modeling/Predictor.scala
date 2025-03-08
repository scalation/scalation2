
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Feb 20 17:39:57 EST 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Framework: Predictor for Matrix Input, Vector Output
 */

package scalation
package modeling

import scala.collection.mutable.{ArrayBuffer, IndexedSeq, LinkedHashSet => LSET, Set}
import scala.math.{cbrt, min, sqrt}
import scala.util.control.Breaks.{break, breakable}

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Rectify the prediction/forecast when they are required to be non-negative, by
 *  setting negative values to zero.
 *  @param yp    the predictived/forecasted value
 *  @param nneg  whether the values are required to be non-negative (e.g., counts)
 */
inline def rectify (yp: Double, nneg: Boolean = true): Double =
    if nneg && yp < 0.0 then 0.0 else yp
end rectify


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Rectify the prediction/forecast when they are required to be non-negative, by
 *  setting negative values in the vector to zero.
 *  @param yp    the predictived/forecasted vector
 *  @param nneg  whether the values are required to be non-negative (e.g., counts)
 */
inline def rectify (yp: VectorD, nneg: Boolean): VectorD =
    if nneg then yp.map (rectify (_)) else yp
end rectify


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Predictor` trait provides a framwork for multiple predictive analytics
 *  techniques, e.g., `Regression`.  x is multi-dimensional [1, x_1, ... x_k].
 *  Fit the parameter vector b in for example the regression equation
 *      y  =  b dot x + e  =  b_0 + b_1 * x_1 + ... b_k * x_k + e
 *  @param x       the input/data m-by-n matrix
 *                     (augment with a first column of ones to include intercept in model)
 *  @param y       the response/output m-vector
 *  @param fname   the feature/variable names (if null, use x_j's)
 *  @param hparam  the hyper-parameters for the model
 */
trait Predictor (x: MatrixD, y: VectorD, protected var fname: Array [String], hparam: HyperParameter)
      extends Model
         with FeatureSelection:

    protected val DO_PLOT = true                                             // whether to plot y vs yp
    protected val LIMIT   = 5000                                             // do not plot more than 5000 points
    private   val debug   = debugf ("Predictor", true)                       // debug function
    private   val flaw    = flawf ("Predictor")                              // flaw function

    if x != null then
        if x.dim != y.dim then flaw ("init", "row dimensions of x and y are incompatible")
        if x.dim <= x.dim2 then
            flaw ("init", s"Predictor requires more rows ${x.dim} than columns ${x.dim2}")
    end if

    private val MIN_FOLDS = 3                                                // minimum number of folds for cross validation
    private val stream    = 0                                                // random number stream to use
    private val permGen   = TnT_Split.makePermGen (y.dim, stream)            // permutation generator

    protected var b: VectorD = null                                          // parameter/coefficient vector [b_0, b_1, ... b_k]
//  protected var e: VectorD = null                                          // residual/error vector [e_0, e_1, ... e_m-1]

    if x != null && fname == null then fname = x.indices2.map ("x" + _).toArray  // default feature/variable names

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the used data matrix x.  Mainly for derived classes where x is expanded
     *  from the given columns in x_, e.g., `SymbolicRegression.quadratic` adds squared columns.
     */
    def getX: MatrixD = x

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the used response vector y.  Mainly for derived classes where y is
     *  transformed, e.g., `TranRegression`, `ARX`.
     */
    def getY: VectorD = y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the feature/variable names.
     */
    def getFname: Array [String] = fname

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number of terms/parameters in the model, e.g., b_0 + b_1 x_1 + b_2 x_2 
     *  has three terms.
     */
    def numTerms: Int = getX.dim2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train a predictive model y_ = f(x_) + e where x_ is the data/input
     *  matrix and y_ is the response/output vector.  These arguments default
     *  to the full dataset x and y, but may be restricted to a training
     *  dataset.  Training involves estimating the model parameters b.
     *  @param x_  the training/full data/input matrix (defaults to full x)
     *  @param y_  the training/full response/output vector (defaults to full y)
     */
    def train (x_ : MatrixD = x, y_ : VectorD = y): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The train2 method should work like the train method, but should also
     *  optimize hyper-parameters (e.g., shrinkage or learning rate).
     *  Only implementing classes needing this capability should override this method.
     *  @param x_  the training/full data/input matrix (defaults to full x)
     *  @param y_  the training/full response/output vector (defaults to full y)
     */
    def train2 (x_ : MatrixD = x, y_ : VectorD = y): Unit =
        throw new UnsupportedOperationException ("train2: not supported - no hyper-parameters to optimize")
    end train2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the predictive model y_ = f(x_) + e and return its predictions and QoF vector.
     *  Testing may be in-sample (on the full dataset) or out-of-sample (on the testing set)
     *  as determined by the parameters passed in.
     *  Note: must call train before test.
     *  @param x_  the testing/full data/input matrix (defaults to full x)
     *  @param y_  the testing/full response/output vector (defaults to full y)
     */
    def test (x_ : MatrixD = x, y_ : VectorD = y): (VectorD, VectorD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train and test the predictive model y_ = f(x_) + e and report its QoF
     *  and plot its predictions.  Return the predictions and QoF.
     *  FIX - currently must override if y is transformed, @see `TranRegression`
     *  @param x_  the training/full data/input matrix (defaults to full x)
     *  @param y_  the training/full response/output vector (defaults to full y)
     *  @param xx  the testing/full data/input matrix (defaults to full x)
     *  @param yy  the testing/full response/output vector (defaults to full y)
     */
    def trainNtest (x_ : MatrixD = x, y_ : VectorD = y)
                   (xx: MatrixD = x, yy: VectorD = y): (VectorD, VectorD) =
        train (x_, y_)                                                 // train the model on training set
        debug ("trainNTest", s"b = $b")
        val (yp, qof) = test (xx, yy)                                  // test the model on testing set
        println (report (qof))                                         // report on Quality of Fit (QoF)
        if DO_PLOT then
            val lim = min (yy.dim, LIMIT)
            val (qyy, qyp) = (yy(0 until lim), yp(0 until lim))        // slice to LIMIT
            val (ryy, ryp) = orderByY (qyy, qyp)                       // order by yy
//          new Plot (null, ryy, ryp, s"$modelName: y black/actual vs. yp red/predicted")
            new Plot (null, ryy, ryp, s"$modelName: y black/actual vs. yp red/predicted", lines = true)
        end if
        (yp, qof)
    end trainNtest

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the formula y = b dot z,
     *  e.g., (b_0, b_1, b_2) dot (1, z_1, z_2).
     *  Must override when using transformations, e.g., `ExpRegression`.
     *  @param z  the new vector to predict
     */
    def predict (z: VectorD): Double = b dot z

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of vector y = f(x_, b), e.g., x_ * b for `Regression`.
     *  May override for efficiency.
     *  @param x_  the matrix to use for making predictions, one for each row
     */
    def predict (x_ : MatrixD): VectorD =
        VectorD (for i <- x_.indices yield predict (x_(i)))
    end predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the hyper-parameters.
     */
    def hparameter: HyperParameter = hparam

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of parameter/coefficient values.
     */
    def parameter: VectorD = b

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of residuals/errors.
     */
//  def residual: VectorD = e

//  F E A T U R E   S E L E C T I O N

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a sub-model that is restricted to the given columns of the data matrix.
     *  Must be implemented for models that support feature selection.
     *  Otherwise, use @see `NoBuildModel
     *  @param x_cols  the columns that the new model is restricted to
     */
    def buildModel (x_cols: MatrixD): Predictor & Fit

    private var theBest = BestStep ()()                                      // record the best model from feature selection

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the best-step to default
     */
    def resetBest (): Unit = theBest = BestStep ()()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the best model found from feature selection.
     */
    def getBest: BestStep = theBest

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** When the new best-step is better than theBest, replace theBest.
     *  @param best  new best-step found during feature selection
     *  @param qk    index of Quality of Fit (QoF) to use for comparing quality
     */
    private def updateBest (best: BestStep)(using qk: Int): Unit =
        if best.qof != null then
            if theBest.qof == null || (best gt theBest.qof(qk)) then theBest = best
    end updateBest

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the rSq-based QoF results for the l-th iteration.
     *  @param rSq    the matrix contain information about r-Sq-based QoF measures
     *  @param l      the l-th iteration
     *  @param cross  indicator of whether cross-validation are to be included
     *  @param fit_l  the fit vector for the l-th iteration
     *  @param mod_l  the predictive model for the l-th iteration
    private def updateQoF (rSq: MatrixD, l: Int, cross: Boolean, best: BestStep): Unit =
        rSq(l) =
            if cross then
                Fit.qofVector (best.qof, best.mod.crossValidate ())          // results for model mod_l, with cross-validation
            else
                Fit.qofVector (best.qof, null)                               // results for model mod_l, no cross-validation
    end updateQoF
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to find the most predictive variable to add the
     *  existing model, returning the variable to add and the new model.
     *  May be called repeatedly.
     *  @see `Fit` for index of QoF measures.
     *  @param cols  the columns of matrix x currently included in the existing model
     *  @param qk    index of Quality of Fit (QoF) to use for comparing quality
     */
    def forwardSel (cols: LSET [Int])(using qk: Int): BestStep =
        var best = BestStep ()()                                             // best step so far

        for j <- x.indices2 if ! (cols contains j) do
            val cols_j = cols union LSET (j)                                 // try adding variable/column x_j
            val x_cols = x(?, cols_j)                                        // x projected onto cols_j columns
            val mod_j  = buildModel (x_cols)                                 // regress with x_j added
            mod_j.train ()                                                   // train model
            best = best.better (j, mod_j.test ()._2, mod_j)                  // which is better
        end for

        if best.col == -1 then
            flaw ("forwardSel", "could not find a variable x_j to add: best.col = -1")
        best
    end forwardSel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evalaute the model with only one column, e.g., intercept only model.
     *  @param qk  index of Quality of Fit (QoF) to use for comparing quality
     */
    def select0 (qk: Int): BestStep =
        val x_cols = x(?, LSET (0))                                          // x projected onto columns {0}
        val mod_0  = buildModel (x_cols)                                     // regress with x_0 added
        mod_0.train ()                                                       // train model
        val qof_0 = mod_0.test ()._2
        BestStep (0, qof_0, mod_0)(qof_0(qk))                                // result for intercept only
    end select0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform forward selection to find the most predictive variables to have
     *  in the model, returning the variables added and the new Quality of Fit (QoF)
     *  measures for all steps.
     *  @see `Fit` for index of QoF measures.
     *  @param cross  whether to include the cross-validation QoF measure
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     */
    def forwardSelAll (cross: Boolean = true)(using qk: Int): (LSET [Int], MatrixD) =
        resetBest ()
        val rSq  = new MatrixD (x.dim2, Fit.qofVectorSize)                   // QoF: R^2, R^2 Bar, sMAPE, R^2 cv
        val cols = LSET (0)                                                  // start with x_0 in model (e.g., intercept)
        updateQoF (rSq, 0, cross, select0 (qk))                              // update Qof results for 0-th variable

        banner (s"forwardSelAll: (l = 0) INITIAL variable (0, ${fname(0)}) => cols = $cols")

        breakable {
            for l <- 1 until x.dim2 do
                val best = forwardSel (cols)                                 // add most predictive variable
                if best.col == -1 then break ()                              // could not find variable to add
                updateBest (best)
                cols += best.col                                             // add variable x_j
                updateQoF (rSq, l, cross, best)                              // update QoF results for l-th variable
                val (jj, jj_qof) = (best.col, best.qof(qk))
                banner (s"forwardSelAll: (l = $l) ADD variable ($jj, ${fname(jj)}) => cols = $cols @ $jj_qof")
            end for
        } // breakable

        (cols, rSq)
    end forwardSelAll

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the relative importance of selected variables, ordered highest to
     *  lowest, rescaled so the highest is one.
     *  @param cols  the selected columns/features/variables
     *  @param rSq   the matrix R^2 values (stand in for sse)
     */
    def importance (cols: Array [Int], rSq: MatrixD): Array [(Int, Double)] =
        val r2  = rSq(?, 0)                                                   // use column 0 for R^2
        val imp = Array.ofDim [(Int, Double)] (r2.dim)                        // for variables, except intercept
        val sf  = 1.0 / (r2(1) - r2(0))                                       // scale factor, so most important = 1
        imp(0)  = (cols(0), -0.0)
        for j <- 1 until imp.size do imp(j) = (cols(j), sf * (r2(j) - r2(j-1)))   // scaled improvement in R^2 (2 => cv)
        imp                                                                   // return the importance
    end importance

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to find the least predictive variable to remove
     *  from the existing model, returning the variable to eliminate, the new parameter
     *  vector and the new Quality of Fit (QoF).  May be called repeatedly.
     *  @see `Fit` for index of QoF measures.
     *  @param cols   the columns of matrix x currently included in the existing model
     *  @param first  first variable to consider for elimination
     *                      (default (1) assume intercept x_0 will be in any model)
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     */
    def backwardElim (cols: LSET [Int], first: Int = 1)(using qk: Int): BestStep =
        var best = BestStep ()()                                             // best step so far

        for j <- first until x.dim2 if cols contains j do
            val cols_j = cols diff LSET (j)                                  // try removing variable/column x_j
            val x_cols = x(?, cols_j)                                        // x projected onto cols_j columns
            val mod_j  = buildModel (x_cols)                                 // regress with x_j added
            mod_j.train ()                                                   // train model
            best = best.better (j, mod_j.test ()._2, mod_j)                  // which is better
        end for

        if best.col == -1 then
            flaw ("backwardElim", "could not find a variable x_j to eliminate: best.col = -1")
        best
    end backwardElim

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the full model before variable elimination as a starting point for
     *  backward elimination.
     *  @param qk  index of Quality of Fit (QoF) to use for comparing quality
     */
    def fullModel (qk: Int): BestStep =
        val mod_a = buildModel (x)                                           // regress with all variables x_j
        mod_a.train ()                                                       // train model
        val qof_a = mod_a.test ()._2
        BestStep (-1, qof_a, mod_a)(qof_a(qk))                               // result for full only
    end fullModel

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform backward elimination to find the least predictive variables to remove
     *  from the full model, returning the variables left and the new Quality of Fit (QoF)
     *  measures for all steps.
     *  @see `Fit` for index of QoF measures.
     *  @param first  first variable to consider for elimination
     *  @param cross  whether to include the cross-validation QoF measure
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     */
    def backwardElimAll (first: Int = 1, cross: Boolean = true)(using qk: Int):
                        (LSET [Int], MatrixD) =
        resetBest ()
        val rSq  = new MatrixD (x.dim2, Fit.qofVectorSize)                   // R^2, R^2 Bar, sMAPE, R^2 cv
        val cols = LSET.range (0, x.dim2)                                    // start with all x_j in model
        val rem  = ArrayBuffer [Int] ()                                      // start with no columns removed

        val best0 = fullModel (qk)
        updateQoF (rSq, 0, cross, best0)                                     // update QoF results for full model
        val jj_qof = best0.qof(qk)
        banner (s"backwardElimAll: (l = 0) INITIAL variables (all) => cols = $cols @ $jj_qof")

        breakable {
            for l <- 1 until x.dim2 - 1 do                                   // l indicates number of variables eliminated
                val best = backwardElim (cols, first)                        // remove least predictive variable
                if best.col == -1 then break ()                              // could not find variable to remove
                updateBest (best)
                cols -= best.col                                             // remove variable x_j
                rem  += best.col                                             // keep track of removed columns
                updateQoF (rSq, l, cross, best)                              // update QoF results
                val (jj, jj_qof) = (best.col, best.qof(qk))
                banner (s"backwardElimAll: (l = $l) REMOVE variable ($jj, ${fname(jj)}) => cols = $cols @ $jj_qof")
            end for
        } // breakable

        updateQoF (rSq, x.dim2-1, cross, select0 (qk))                       // update Qof results for 0-th variable
        rem += cols.max                                                      // remove last non-zero column
        rem += 0                                                             // remove column 0

        (LSET.from (rem.reverse), rSq.reverse)                               // reverse the order results
    end backwardElimAll 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform stepwise regression to find the most predictive variables to have
     *  in the model, returning the variables left and the new Quality of Fit (QoF)
     *  measures for all steps.  At each step it calls forwardSel and backwardElim
     *  and takes the best of the two actions.  Stops when neither action yields improvement.
     *  @see `Fit` for index of QoF measures.
     *  @param cross  whether to include the cross-validation QoF measure
     *  @param swap   whether to allow a swap step (swap out a feature for a new feature in one step)
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     */
    def stepwiseSelAll (cross: Boolean = true, swap: Boolean = true)(using qk: Int):
                       (LSET [Int], MatrixD) =
        resetBest ()
        val rSq    = new MatrixD (x.dim2 - 1, Fit.qofVectorSize)             // QoF: R^2, R^2 Bar, sMAPE, R^2 cv
        val cols   = LSET (0)                                                // start with x_0 in model
        var last_q = Fit.extreme (qk)                                        // current best QoF
        val vars   = ArrayBuffer [Int]()

        banner (s"stepwiseSelAll: (l = 0) INITIAL variable (0, ${fname(0)}) => cols = $cols")

        breakable {
            for l <- 1 until x.dim2 - 1 do
                val bestf = forwardSel (cols)                                // add most predictive variable OR
                val bestb = backwardElim (cols, 1)                           // remove least predictive variable
                debug ("stepwiseSelAll", s"bestf = $bestf, bestb = $bestb")

                if (bestb.col == -1 || (bestf ge bestb.qof(qk))) &&          // forward as good as backward
                   (bestf.col != -1 && (bestf gt last_q)) then               // a better model has been found
                    updateBest (bestf)
                    vars  += bestf.col
                    cols  += bestf.col                                       // ADD variable bestf.col
                    last_q = bestf.qof(qk)
                    updateQoF (rSq, l, cross, bestf)                         // update QoF results
                    println (s"\nstepwiseSelAll: (l = $l) ADD variable $bestf")
                    val (jj, jj_qof) = (bestf.col, last_q)
                    banner (s"stepwiseSelAll: (l = $l) ADD variable ($jj, ${fname(jj)}) => cols = $cols @ $jj_qof")

                else if bestb.col != -1 && (bestb gt last_q) then            // a better model has been found
                    updateBest (bestb)
                    vars  += bestb.col
                    cols  -= bestb.col                                       // REMOVE variable bestb.col 
                    last_q = bestb.qof(qk)
                    updateQoF (rSq, l, cross, bestb)                         // update QoF results
                    println (s"\nstepwiseSelAll: (l = $l) REMOVE variable $bestb")
                    val (jj, jj_qof) = (bestb.col, last_q)
                    banner (s"stepwiseSelAll: (l = $l) REMOVE variable ($jj, ${fname(jj)}) => cols = $cols @ $jj_qof")

                else
                    if ! swap then break ()
                    val (out, in) = (bestb.col, bestf.col)
                    val bestfb = swapVars (cols, out, in, qk)
                    updateBest (bestfb)
                    if out != -1 && in != -1 && (bestfb gt last_q) then      // a better model has been found
                        vars  += bestb.col
                        vars  += bestf.col
                        cols  -= bestb.col                                   // REMOVE variable bestb.col (swap out)
                        cols  += bestf.col                                   // ADD variable bestf.col (swap in)
                        last_q = bestfb.qof(qk)
                        updateQoF (rSq, l, cross, bestfb)                    // update QoF results
                        println (s"\nstepwiseSelAll: (l = $l) SWAP variable $bestb with $bestf")
                    else
                        break ()                                             // can't find a better model -> quit
                    end if
                end if

                val x_cols = x(?, cols)                                      // x projected onto cols columns
                val mod_   = buildModel (x_cols)                             // regress on this x
                mod_.train ()                                                // train model
                println (mod_.report (mod_.test ()._2))                      // test and report
            end for
        } // breakable

        println (s"stepwiseSelAll: selected features = $cols")
        println (s"stepwiseSelAll: selected features = ${cols.map (fname (_))}")
        println (s"stepwiseSelAll: features in/out   = $vars")

        (cols, rSq(1 until cols.size))
    end stepwiseSelAll

// FIX - not returning all selected variables - only (0, 4) but should be (0, 4, 6)
// FIX - test SWAP

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Swap out variable with in variable.
     *  @param cols  the columns of matrix x currently included in the existing model
     *  @param out   the variable to swap out
     *  @param in    the variable to swap in
     *  @param qk    index of Quality of Fit (QoF) to use for comparing quality
     */
    def swapVars (cols: LSET [Int], out: Int, in: Int, qk: Int): BestStep =
        val cols_  = cols diff LSET (out) union LSET (in)  // swap out var with in var
        val x_cols = x(?, cols_)                                             // x projected onto cols_j columns
        val mod_j  = buildModel (x_cols)                                     // regress with x_out removed and x_in added
        mod_j.train ()                                                       // train model
        val qof_in = mod_j.test ()._2
        BestStep (in, qof_in, mod_j)(qof_in(qk))                             // candidate step
    end swapVars

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the Variance Inflation Factor (VIF) for each variable to test
     *  for multi-collinearity by regressing x_j against the rest of the variables.
     *  A VIF over 50 indicates that over 98% of the variance of x_j can be predicted
     *  from the other variables, so x_j may be a candidate for removal from the model.
     *  Note:  override this method to use a superior regression technique.
     *  @param skip  the number of columns of x at the beginning to skip in computing VIF
     */
    def vif (skip: Int = 1): VectorD =
        val vifV = new VectorD (x.dim2 - skip)                               // VIF vector for x columns except skip columns
        if vifV.dim == 1 then { vifV(0) = 1.0; return vifV }                 // no other variables
        for j <- skip until x.dim2 do
            val x_j   = x(?, j)                                              // column j vector
            val x_noj = x.not (?, j)                                         // all columns except j matrix                   
            val mod_j = new Regression (x_noj, x_j)                          // regress with x_j removed
            mod_j.train ()                                                   // train model
            val rSq_j = (mod_j.test ()._2)(QoF.rSq.ordinal)                  // R^2 for predicting x_j
            if rSq_j.isNaN then Fac_LU.diagnoseMat (x_noj)                   // check for problems with matrix
//          debug ("vif", s"for variable x_$j, rSq_$j = $rSq_j")
            vifV(j-1) =  1.0 / (1.0 - rSq_j)                                 // store vif for x_1 in vifV(0)
        end for
        vifV
    end vif

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the indices for the test-set.
     *  @see `scalation.mathstat.TnT_Split`
     *  @param n_test  the size of test-set
     *  @param rando   whether to select indices randomly or in blocks
     */
    inline def testIndices (n_test: Int, rando: Boolean): IndexedSeq [Int] =
        TnT_Split.testIndices (permGen, n_test, rando)
    end testIndices

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Use validation to compute test Quality of Fit (QoF) measures by dividing
     *  the full dataset into a TESTING set and a TRAINING set.
     *  The test set is defined by idx and the rest of the data is the training set.
     *  @param rando  flag indicating whether to use randomized or simple validation
     *  @param ratio  the ratio of the TESTING set to the full dataset (most common 70-30, 80-20)
     *  @param idx    the prescribed TESTING set indices (default => generate)
     */
    def validate (rando: Boolean = true, ratio: Double = 0.2)
                 (idx: IndexedSeq [Int] = testIndices ((ratio * y.dim).toInt, rando)): VectorD =
        val (x_e, x_, y_e, y_) = TnT_Split (x, y, idx)                       // Test-n-Train Split

        train (x_, y_)                                                       // train model on the training set
        val qof = test (x_e, y_e)._2                                         // test on test-set and get QoF measures
        if qof(QoF.sst.ordinal) <= 0.0 then                                  // requires variation in test-set
            flaw ("validate", "chosen testing set has no variability")
        end if
        println (FitM.fitMap (qof, QoF.values.map (_.toString)))
        qof
    end validate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Use k-fold cross-validation to compute test Quality of Fit (QoF) measures
     *  by iteratively dividing the full dataset into a TESTING set and a TRAINING set.
     *  Each test set is defined by idx and the rest of the data is the training set.
     *  @see showQofStatTable in `Fit` object for printing the returned stats.
     *  @param k      the number of cross-validation iterations/folds (defaults to 5x).
     *  @param rando  flag indicating whether to use randomized or simple cross-validation
     */
    def crossValidate (k: Int = 5, rando: Boolean = true): Array [Statistic] =
        if k < MIN_FOLDS then flaw ("crossValidate", s"k = $k must be at least $MIN_FOLDS")
        val stats   = Fit.qofStatTable                                       // create table for QoF measures
        val fullIdx = if rando then permGen.igen                             // permuted indices
                      else VectorI.range (0, y.dim)                          // ordered indices
        val sz      = y.dim / k                                              // size of each fold
        val ratio   = 1.0 / k                                                // fraction of dataset used for testing

        for fold <- 0 until k do
            banner (s"crossValidate: fold $fold: train-test splits sizes = (${y.dim - sz}, $sz)")
            val idx = fullIdx (fold * sz until (fold+1) * sz).toMuIndexedSeq   // instance indices for this fold
            val qof = validate (rando, ratio)(idx)
            debug ("crossValidate", s"fold $fold: qof = $qof")
            if qof(QoF.sst.ordinal) > 0.0 then                               // requires variation in test-set
                for q <- qof.indices do stats(q).tally (qof(q))              // tally these QoF measures
            end if
        end for

        stats
    end crossValidate

end Predictor


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Predictor` companion object provides a method for testing predictive
 *  models.
 */
object Predictor:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test (in-sample) by training and testing on the FULL dataset.
     *  Test (out-of-sample) by training on the TRAINING set and testing on the TESTING set.
     *  @param mod    the model to be used
     *  @param ext    the model subtype extension (e.g., indicating the transformation function used)
     *  @param check  whether to check the assertion that the in-sample and out-of-sample results
     *                are in rough agreement (e.g., at 20%)
     */
    def test (mod: Predictor, ext: String = "", check: Boolean = true): Unit =
        val iq = QoF.rSq.ordinal
        banner (s"Test ${mod.modelName} $ext")
        val (yp, qof) = mod.trainNtest ()()                                  // train and test the model on full dataset (in-sample)

        println ("Validate: Out-of-Sample Testing")
        val qof2 = mod.validate ()()                                         // train on training set, test on testing set
        if check then assert (rel_diff (qof(iq), qof2(iq)) < 0.2)            // check agreement of in-sample and out-of-sample results
        println (FitM.fitMap (mod.validate ()(), QoF.values.map (_.toString)))
    end test

end Predictor


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `predictorTest` main function is used to test the `Predictor` trait
 *  and its derived classes using the `Example_AutoMPG` dataset containing
 *  data matrices x, ox and response vector y.
 *  Shift imports for the Example_BasketBall or Example_BPressure datasets.
 *  @see `Example_AutoMPG_Correlation
 *  > runMain scalation.modeling.predictorTest
 */
@main def predictorTest (): Unit =

    import Example_AutoMPG._
//  import Example_BasketBall._
//  import Example_BPressure._
    import Predictor.test

    val fname_6  = Array ("modelyear")                                       // modelyear has highest positive correlation
    val fname_4  = Array ("weight")                                          // weight has highest correlation magnitude
    val fname_04 = Array ("intercept", "weight")
    val hp2      = Regression.hp                                             // the hyper-parameters of Regression

    test (new NullModel (y), check = false)                                  // 1
    test (new SimplerRegression (ox(?, Set (6)), y, fname_6))                // 2
    test (new SimpleRegression (ox(?, Set (0, 4)), y, fname_04))             // 3
    test (new Regression (x, y, x_fname))                                    // 4 - no intercept
    test (new Regression (ox, y, ox_fname))                                  // 5
    test (RidgeRegression.center (x, y, x_fname))                            // 6 - no intercept
    test (new LassoRegression (x, y, x_fname))                               // 7 - no intercept
    test (new LassoRegression (ox, y, ox_fname))                             // 8
    test (new RegressionWLS (ox, y, ox_fname))                               // 9
    test (new TranRegression (ox, y, ox_fname, hp2, id, id), "id")           // 10 - id
    test (new TranRegression (ox, y, ox_fname, hp2, sqrt, sq), "sqrt")       // 11 - sqrt
    test (new TranRegression (ox, y, ox_fname, hp2, cbrt, cb), "cbrt")       // 12 - cbrt
    test (new TranRegression (ox, y, ox_fname), "log")                       // 13 - log
    test (TranRegression (ox, y, ox_fname), "box-cox")                       // 14 - box-cox
    test (SymbolicRegression.quadratic (x, y, x_fname))                      // 15
    test (SymbolicRegression.quadratic (x, y, x_fname, true))                // 16
    test (SymbolicRegression.cubic (x, y, x_fname))                          // 17
    test (SymbolicRegression.cubic (x, y, x_fname, true))                    // 18
    test (SymbolicRegression (x, y, x_fname, Set (-2.0, -1, 2, 3, 4)))       // 19
    test (new PolyRegression (ox(?, Set (4)), y, 4, fname_4))                // 20
    test (new PolyORegression (ox(?, Set (4)), y, 4, fname_4))               // 21 
    test (new TrigRegression (ox(?, Set (4)), y, 8, fname_4))                // 22
    test (new ExpRegression (ox, y, ox_fname))                               // 23
    test (new KNN_Regression (x, y, x_fname), "k=3")                         // 25
    KNN_Regression.hp("kappa") = 5
    test (new KNN_Regression (x, y, x_fname), "k=5")                         // 24
    KNN_Regression.hp("kappa") = 7
    test (new KNN_Regression (x, y, x_fname), "k=7")                         // 26
    test (RegressionCat (oxr, y, 6, xr_fname))                               // 27 - include the origin cat. col.
    test (new RegressionTree (x, y, x_fname))                                // 28
//  test (new RegressionTreeGB (x, y, x_fname))                              // 29

end predictorTest

