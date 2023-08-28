
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Mustafa Nural
 *  @version 2.0
 *  @date    Tue Apr 18 14:24:14 EDT 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: Lasso Regression (L1 Shrinkage/Regularization)
 */

package scalation
package modeling

import scalation.mathstat._
import scalation.optimization.LassoAdmm

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LassoRegression` class supports multiple linear regression.  In this case,
 *  'x' is multi-dimensional [1, x_1, ... x_k].  Fit the parameter vector 'b' in
 *  the regression equation
 *      y  =  b dot x + e  =  b_0 + b_1 * x_1 + ... b_k * x_k + e
 *  where 'e' represents the residuals (the part not explained by the model).
 *  @see see.stanford.edu/materials/lsoeldsee263/05-ls.pdf
 *  @param x       the data/input m-by-n matrix
 *  @param y       the response/output m-vector
 *  @param fname_  the feature/variable names (defaults to null)
 *  @param hparam  the shrinkage hyper-parameter, lambda (0 => OLS) in the penalty term 'lambda * b dot b'
 */
class LassoRegression (x: MatrixD, y: VectorD, fname_ : Array [String] = null,
                       hparam: HyperParameter = LassoRegression.hp)
      extends Predictor (x, y, fname_, hparam)
         with Fit (dfm = x.dim2 - 1, df = x.dim - x.dim2):

    private val flaw   = flawf ("LassoRegression")                       // flaw function
    private val debug  = debugf ("LassoRegression", true)                // debug function
    private var lambda = hparam ("lambda").toDouble                      // weight to put on regularization

    modelName = "LassoRegression"

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the value of the shrinkage parameter 'lambda'.
     */
    def lambda_ : Double = lambda

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find an optimal value for the shrinkage parameter 'lambda' using Cross Validation
     *  to minimize 'sse_cv'.  The search starts with the low default value for 'lambda'
     *  doubles it with each iteration, returning the minimum 'lambda' and it corresponding
     *  cross-validated 'sse'.
     */
    def findLambda: (Double, Double) =
        var l      = lambda
        var l_best = l
        var sse    = Double.MaxValue
        for i <- 0 to 20 do
            LassoRegression.hp("lambda") = l
            val rrg   = new LassoRegression (x, y)
            val stats = rrg.crossValidate ()
            val sse2  = stats(QoF.sse.ordinal).mean
            banner (s"LassoRegession with lambda = ${rrg.lambda_} has sse = $sse2")
            if sse2 < sse then
                sse = sse2; l_best = l
            end if
            FitM.showQofStatTable (stats)
            l *= 2
        end for
        (l_best, sse)
    end findLambda

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the predictor by fitting the parameter vector (b-vector) in the
     *  multiple regression equation
     *      y  =  b dot x + e  =  [b_0, ... b_k] dot [1, x_1 , ... x_k] + e
     *  regularized by the sum of magnitudes of the coefficients.
     *  @see pdfs.semanticscholar.org/969f/077a3a56105a926a3b0c67077a57f3da3ddf.pdf
     *  @see `scalation.optimization.LassoAdmm`
     *  @param x_  the training/full data/input matrix
     *  @param y_  the training/full response/output vector
     */
    def train (x_ : MatrixD = x, y_ : VectorD = y): Unit =
        b = LassoAdmm.solve (x_, y_, lambda)                             // Alternating Direction Method of Multipliers

        if b(0).isNaN then flaw ("train", s"parameter b = $b")
        debug ("train", s"LassoAdmm estimates parameter b = $b")
    end train

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test a predictive model y_ = f(x_) + e and return its QoF vector.
     *  Testing may be be in-sample (on the training set) or out-of-sample
     *  (on the testing set) as determined by the parameters passed in.
     *  Note: must call train before test.
     *  @param x_  the testing/full data/input matrix (defaults to full x)
     *  @param y_  the testing/full response/output vector (defaults to full y)
     */
    def test (x_ : MatrixD = x, y_ : VectorD = y): (VectorD, VectorD) =
        val yp = predict (x_)                                            // make predictions
        (yp, diagnose (y_, yp))                                          // return predictions and QoF vector
    end test

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Produce a QoF summary for a model with diagnostics for each predictor 'x_j'
     *  and the overall Quality of Fit (QoF).
     *  @param x_      the testing/full data/input matrix
     *  @param fname_  the array of feature/variable names
     *  @param b_      the parameters/coefficients for the model
     *  @param vifs    the Variance Inflation Factors (VIFs)
     */
    override def summary (x_ : MatrixD = getX, fname_ : Array [String] = fname, b_ : VectorD = b,
                          vifs: VectorD = vif ()): String =
        super.summary (x_, fname_, b_, vifs)                             // summary from `Fit`
    end summary

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a sub-model that is restricted to the given columns of the data matrix.
     *  @param x_cols  the columns that the new model is restricted to
     */
    override def buildModel (x_cols: MatrixD): LassoRegression =
        debug ("buildModel", s"${x_cols.dim} by ${x_cols.dim2}")
        new LassoRegression (x_cols, y, null, hparam)
    end buildModel

end LassoRegression


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LassoRegression` companion object provides factory methods for the
 *  `LassoRegression` class.
 */
object LassoRegression:

    /** Base hyper-parameter specification for `LassoRegression`
     */
    val hp  = new HyperParameter; hp += ("lambda", 0.01, 0.01)           // L1 regularization/shrinkage parameter

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `LassoRegression` object from a combined data matrix.
     *  @param xy      the combined data matrix
     *  @param fname   the feature/variable names (defaults to null)
     *  @param hparam  the hyper-parameters (defaults to hp)
     *  @param col     the designated response column (defaults to the last column)
     */
    def apply (xy: MatrixD, fname: Array [String] = null,
               hparam: HyperParameter = hp)(col: Int = xy.dim2 - 1): LassoRegression =
        new LassoRegression (xy.not(?, col), xy(?, col), fname, hparam)
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `LassoRegression` object from a data matrix and a response vector.
     *  This method provides data rescaling.
     *  @param x       the data/input m-by-n matrix
     *                     (augment with a first column of ones to include intercept in model)
     *  @param y       the response/output m-vector
     *  @param fname   the feature/variable names (defaults to null)
     *  @param hparam  the hyper-parameters (defaults to hp)
     */
    def rescale (x: MatrixD, y: VectorD, fname: Array [String] = null,
                 hparam: HyperParameter = hp): LassoRegression =
            val xn = normalize ((x.mean, x.stdev)) (x)
            new LassoRegression (xn, y, fname, hparam)
    end rescale

end LassoRegression


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `lassoRegressionTest` main function tests `LassoRegression` class using
 *  the following regression equation.
 *      y  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  It comapres `LassoRegression` to `Regression`.
 *  @see http://statmaster.sdu.dk/courses/st111/module03/index.html
 *  > runMain scalation.modeling.lassoRegressionTest
 */
@main def lassoRegressionTest (): Unit =

    // 5 data points: constant term, x_1 coordinate, x_2 coordinate
    val x = MatrixD ((5, 3), 1.0, 36.0,  66.0,                     // 5-by-3 matrix
                             1.0, 37.0,  68.0,
                             1.0, 47.0,  64.0,
                             1.0, 32.0,  53.0,
                             1.0,  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (1.0, 20.0, 80.0)

//  println ("model: y = b_0 + b_1*x_1 + b_2*x_2")
    println ("model: y = b₀ + b₁*x₁ + b₂*x₂")
    println ("x = " + x)
    println ("y = " + y)

    banner ("Regression")
    val reg = new Regression (x, y)                                // create a regression model
    reg.trainNtest ()()                                            // train and test the model
    println (reg.summary ())                                       // parameter/coefficient statistics
    println (s"predict ($z) = ${reg.predict (z)}")                 // make an out-of-sample prediction

    val yp = reg.predict (x)                                       // predict y for several points
    println (s"predict (x) = $yp")

    banner ("LassoRegression")
    val mod = new LassoRegression (x, y)                           // create a Lasso regression model
    mod.trainNtest ()()                                            // train and test the model
    println (mod.summary ())                                       // parameter/coefficient statistics
    println (s"predict ($z) = ${mod.predict (z)}")                 // make an out-of-sample prediction

    val yyp = mod.predict (x)                                      // predict y for several points
    println (s"predict (x) = $yyp")

    new Plot (null, y, yp, "Regression y vs. yp")
    new Plot (null, y, yyp, "Lasso Regression y vs. yyp")
    new Plot (x(?, 1), y, yyp, "y, yyp vs. col 1")
    new Plot (x(?, 2), y, yyp, "y, yyp vs. col 2")

end lassoRegressionTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `lassoRegressionTest2` main function tests `LassoRegression` class using
 *  the following regression equation.
 *      y  =  b dot x  =  b_1*x1 + b_2*x_2.
 *  Try non-default value for the 'lambda' hyper-parameter.
 *  > runMain scalation.modeling.lassoRegressionTest2
 */
@main def lassoRegressionTest2 (): Unit =

    import LassoRegression.hp

    println (s"hp = $hp")
    val hp2 = hp.updateReturn ("lambda", 1.0)                      // try different values
    println (s"hp2 = $hp2")

    // 5 data points:        one   x_1    x_2
    val x = MatrixD ((5, 3), 1.0, 36.0,  66.0,                     // 5-by-3 matrix
                             1.0, 37.0,  68.0,
                             1.0, 47.0,  64.0,
                             1.0, 32.0,  53.0,
                             1.0,  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (1.0, 20.0, 80.0)

    println ("x = " + x + "\ny = " + y + "\nz = " + z)

    banner ("LassoRegression")
    val mod = new LassoRegression (x, y, hparam = hp2)             // create a Lasso regression model
    mod.trainNtest ()()                                            // train and test the model
    println (mod.summary ())                                       // parameter/coefficient statistics
    println (s"predict ($z) = ${mod.predict (z)}")                 // make an out-of-sample prediction

end lassoRegressionTest2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `lassoRegressionTest3` main function tests `LassoRegression` class using
 *  the following regression equation.
 *      y  =  b dot x  =  b_1*x1 + b_2*x_2.
 *  Test regression, forward selection and backward elimination.
 *  > runMain scalation.modeling.lassoRegressionTest3
 */
@main def lassoRegressionTest3 (): Unit =

    // 5 data points:            one   x_1    x_2
    val x = MatrixD ((5, 3), 1.0, 36.0,  66.0,                     // 5-by-3 matrix
                             1.0, 37.0,  68.0,
                             1.0, 47.0,  64.0,
                             1.0, 32.0,  53.0,
                             1.0,  1.0, 101.0)
    val y = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)
    val z = VectorD (1.0, 20.0, 80.0)

    println ("x = " + x + "\ny = " + y + "\nz = " + z)

    banner ("LassoRegression")
    val mod = new LassoRegression (x, y)                           // create a Lasso regression model
    mod.trainNtest ()()                                            // train and test the model
    println (mod.summary ())                                       // parameter/coefficient statistics
    println (s"predict ($z) = ${mod.predict (z)}")                 // make an out-of-sample prediction

    banner ("Forward Selection Test")
    mod.forwardSelAll (cross = false)

    banner ("Backward Elimination Test")
    mod.backwardElimAll (cross = false)

end lassoRegressionTest3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `lassoRegressionTest4` main function tests the `LassoRegression` class using
 *  the AutoMPG dataset.  It illustrates using the `Relation` class for reading the
 *  data from a .csv file "auto-mpg.csv".  Assumes no missing values.
 *  It also combines feature selection with cross-validation and plots
 *  R^2, R^2 Bar and R^2 cv vs. the instance index.
 *  > runMain scalation.modeling.lassoRegressionTest4
 *
@main def lassoRegressionTest4 (): Unit =

    import scalation.database.relation.Relation

    banner ("AutoMPG relation")
    val auto_tab = Relation (DATE_DIR + "auto-mpg.csv", "auto_mpg", null, -1)
    auto_tab.show ()

    banner ("AutoMPG (x, y) dataset")
    val (x, y) = auto_tab.toMatrixDD (ArrayBuffer.range (1, 7), 0)
    println (s"x = $x")
    println (s"y = $y")

    banner ("LassoRegression for AutoMPG")
    val mod = new LassoRegression (x, y)                           // create a Lasso regression model
    mod.trainNtest ()()                                            // train and test the model
    println (mod.summary ())                                       // parameter/coefficient statistics

    banner ("Forward Selection Test")
    val (cols, rSq) = mod.forwardSelAll ()                         // R^2, R^2 Bar, R^2 cv
    val k = cols.size
    val t = VectorD.range (1, k)                                   // instance index
    new PlotM (t, rSq.transpose, Array ("R^2", "R^2 bar", "R^2 cv"),
               "R^2 vs n for LassoRegression", lines = true)
    println (s"rSq = $rSq")

end lassoRegressionTest4
 */


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `lassoRegressionTest5` main function tests the `LassoRegression` class using
 *  the AutoMPG dataset.  It illustrates using the `Relation` class for reading the'
 *  data from a .csv file "auto-mpg.csv".  Assumes no missing values.
 *  It also uses the 'findLambda' method to search for a shrinkage parameter
 *  that roughly mininizes 'sse_cv'.
 *  > runMain scalation.modeling.lassoRegressionTest5
 *
@main def lassoRegressionTest5 (): Unit =

    import scalation.database.relation.Relation

    banner ("AutoMPG relation")
    val auto_tab = Relation (DATA_DIR + "auto-mpg.csv", "auto_mpg", null, -1)
    auto_tab.show ()

    banner ("AutoMPG (x, y) dataset")
    val (x, y) = auto_tab.toMatrixDD (ArrayBuffer.range (1, 7), 0)

    banner ("LassoRegression for AutoMPG")
    val mod = new LassoRegression (x, y)                           // create a Lasso regression model
    mod.trainNtest ()()                                            // train and test the model
    println (mod.summary ())                                       // parameter/coefficient statistics

    println (s"best (lambda, sse) = ${mod.findLambda}")

end lassoRegressionTest5
 */


