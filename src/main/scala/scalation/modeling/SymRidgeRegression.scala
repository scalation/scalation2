
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Thu Dec 23 13:54:30 EST 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: Symbolic Ridge Regression, including Quadratic and Cubic Ridge Regression
 */

package scalation
package modeling

import scala.collection.mutable.Set

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SymRidgeRegression` object supports symbolic ridge regression that allows
 *  variables/columns to be raised to various powers, e.g., x^2, x^3, x^.5.
 *  IMPORTANT:  must not include INTERCEPT (column of ones) in initial data matrix),
 *  i.e., DO NOT include a column of ones in x (will cause singularity)
 */
object SymRidgeRegression:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `RidgeRegression` object from a data matrix and a response vector.
     *  Partial support for "Symbolic Ridge Regression" as matrix x can be raised
     *  to several powers (e.g., x^1 and x^2).  Note, x^1 is automatically included.
     *  Note, Ridge Regression will NOT have an INTERCEPT column.
     *  @param x       the initial data/input m-by-n matrix (before expansion)
     *                     must not include an intercept column of all ones
     *  @param y       the response/output m-vector
     *  @param fname   the feature/variable names (use null for default)
     *  @param powers  the set of powers to raise matrix x to
     *  @param cross   whether to include 2-way cross/interaction terms x_i x_j (defaults to true)
     *  @param cross3  whether to include 3-way cross/interaction terms x_i x_j x_k (defaults to false)
     *  @param hparam  the hyper-parameters (use RidgeRegression.hp for default)
     */
    def apply (x: MatrixD, y: VectorD, fname: Array [String],
               powers: Set [Double], cross: Boolean = true, cross3: Boolean = false,
               hparam: HyperParameter = RidgeRegression.hp): RidgeRegression =
        var xx     = x                                                    // start with multiple regression for x
        var fname_ = fname

        for p <- powers if p != 1 do
            xx       = xx ++^ x~^p                                        // add power terms x^p
            fname_ ++= fname.map ((n) => s"$n^${p.toInt}")
        end for

        if cross then
           xx       = xx ++^ x.crossAll                                   // add 2-way cross terms x_i x_j
           fname_ ++= SymbolicRegression.crossNames (fname)
        end if

        if cross3 then
           xx       = xx ++^ x.crossAll3                                  // add 3-way cross terms x_i x_j x_k
           fname_ ++= SymbolicRegression.crossNames3 (fname)
        end if

        val mod = new RidgeRegression (xx, y, fname_, hparam)
        mod.modelName = "SymRidgeRegression" + (if cross then "X" else "") + (if cross3 then "XX" else "")
        mod
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `SymRidgeRegression` object from a data matrix and a response vector.
     *  This factory function provides data rescaling.
     *  @param x          the data/input m-by-n matrix
     *                        (augment with a first column of ones to include intercept in model)
     *  @param y          the response/output m-vector
     *  @param powers     the set of powers to raise matrix x to
     *  @param cross      whether to include 2-way cross/interaction terms x_i x_j (defaults to true)
     *  @param cross3     whether to include 3-way cross/interaction terms x_i x_j x_k (defaults to false)
     *  @param fname      the feature/variable names (use null for default)
     *  @param hparam     the hyper-parameters (use Regression.hp for default)
     */
    def rescale (x: MatrixD, y: VectorD, fname: Array [String] = null,
                 powers: Set [Double], cross: Boolean = true, cross3: Boolean = false,
                 hparam: HyperParameter = Regression.hp): RidgeRegression =
        val xn = normalize ((x.mean, x.stdev)) (x)
        apply (xn, y, fname, powers, cross, cross3, hparam)
    end rescale

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `RidgeRegression` object that uses multiple regression to fit a quadratic
     *  surface to the data.  For example in 2D, the quadratic regression equation is
     *      y  =  b dot x + e  =  [b_0, ... b_k] dot [1, x_0, x_0^2, x_1, x_1^2] + e
     *  @param x       the initial data/input m-by-n matrix (before quadratic term expansion)
     *                     must not include an intercept column of all ones
     *  @param y       the response/output m-vector
     *  @param fname   the feature/variable names (use null for default)
     *  @param cross   whether to include cross terms x_i * x_j
     *  @param hparam  the hyper-parameters ((use RidgeRegression.hp for default)
     */
    def quadratic (x: MatrixD, y: VectorD, fname: Array [String],
                   cross: Boolean = false, hparam: HyperParameter = RidgeRegression.hp): RidgeRegression =
        val mod = apply (x, y, fname, Set (2), false, cross, hparam)
        mod.modelName = "SymRidgeRegression.quadratic" + (if cross then "X" else "")
        mod
    end quadratic

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a `RidgeRegression` object that uses multiple regression to fit a cubic
     *  surface to the data.  For example in 2D, the cubic regression equation is
     *      y  =  b dot x + e  =  [b_0, ... b_k] dot [1, x_0, x_0^2, x_0^3,
     *                                                   x_1, x_1^2, x_1^3,
     *                                                   x_0*x_1, x_0^2*x_1, x_0*x_1^2] + e
     *  @param x       the initial data/input m-by-n matrix (before quadratic term expansion)
     *                     must not include an intercept column of all ones
     *  @param y       the response/output m-vector
     *  @param fname   the feature/variable names (use null for default)
     *  @param cross   whether to include 2-way cross/interaction terms x_i x_j (defaults to flase)
     *  @param cross3  whether to include 3-way cross/interaction terms x_i x_j x_k (defaults to false)
     *  @param hparam  the hyper-parameters ((use RidgeRegression.hp for default)
     */
    def cubic (x: MatrixD, y: VectorD, fname: Array [String],
               cross: Boolean = false, cross3: Boolean = false,
               hparam: HyperParameter = RidgeRegression.hp): RidgeRegression =
        val mod = apply (x, y, fname, Set (2, 3), cross, cross3, hparam)
        mod.modelName = "SymRidgeRegression.cubic" + (if cross then "X" else "") + (if cross3 then "X" else "")
        mod
    end cubic

end SymRidgeRegression

import Example_AutoMPG._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `symRidgeRegressionTest` main function tests the `SymRidgeRegression`
 *  object using the AutoMPG dataset.  Assumes no missing values.
 *  It tests custom "Symbolic Ridge Regression", with powers specified in "Set (...)" and
 *  applies forward selection, backward elimination, or stepwise regression.
 *  > runMain scalation.modeling.symRidgeRegressionTest
 */
@main def symRidgeRegressionTest (): Unit =

//  println (s"x = $x")
//  println (s"y = $y")

    banner ("AutoMPG Symbolic Ridge Regression")
    val mod = SymRidgeRegression (x, y, x_fname, Set (-2, -1, 0.5, 2))    // add cross-terms and given powers
    mod.trainNtest ()()                                                   // train and test the model
    println (mod.summary ())                                              // parameter/coefficient statistics

    for tech <- SelectionTech.values do 
        banner (s"Feature Selection Technique: $tech")
        val (cols, rSq) = mod.selectFeatures (tech)                       // R^2, R^2 bar, R^2 cv
        val k = cols.size
        println (s"k = $k, n = ${x.dim2}")
        new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "R^2 cv"),
                   s"R^2 vs n for Symbolic Ridge Regression with $tech", lines = true)
        println (s"$tech: rSq = $rSq")
    end for

end symRidgeRegressionTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `symRidgeRegressionTest2` main function tests the `SymRidgeRegression`
 *  object using the AutoMPG dataset.  Assumes no missing values.
 *  It tests "Quadratic Ridge Regression" (with cross = false) and
 *  applies forward selection, backward elimination, or stepwise regression.
 *  > runMain scalation.modeling.symRidgeRegressionTest2
 */
@main def symRidgeRegressionTest2 (): Unit =

//  println (s"x = $x")
//  println (s"y = $y")

    banner ("AutoMPG Quadratic Ridge Regression")
    val mod = SymRidgeRegression.quadratic (x, y, x_fname)                // add x^2 terms
    mod.trainNtest ()()                                                   // train and test the model
    println (mod.summary ())                                              // parameter/coefficient statistics

    for tech <- SelectionTech.values do
        banner (s"Feature Selection Technique: $tech")
        val (cols, rSq) = mod.selectFeatures (tech)                       // R^2, R^2 bar, R^2 cv
        val k = cols.size
        println (s"k = $k, n = ${x.dim2}")
        new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "R^2 cv"),
                   s"R^2 vs n for Quadratic Ridge Regression with $tech", lines = true)
        println (s"$tech: rSq = $rSq")
    end for

end symRidgeRegressionTest2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `symRidgeRegressionTest3` main function tests the `SymRidgeRegression`
 *  object using the AutoMPG dataset.  Assumes no missing values.
 *  It tests "Quadratic X Ridge Regression" (with cross = true) and
 *  applies forward selection, backward elimination, or stepwise regression.
 *  > runMain scalation.modeling.symRidgeRegressionTest3
 */
@main def symRidgeRegressionTest3 (): Unit =

//  println (s"x = $x")
//  println (s"y = $y")

    banner ("AutoMPG Quadratic X Ridge Regression")
    val mod = SymRidgeRegression.quadratic (x, y, x_fname, true)          // add cross-terms and x^2 terms
    mod.trainNtest ()()                                                   // train and test the model
    println (mod.summary ())                                              // parameter/coefficient statistics

    for tech <- SelectionTech.values do 
        banner (s"Feature Selection Technique: $tech")
        val (cols, rSq) = mod.selectFeatures (tech)                       // R^2, R^2 bar, R^2 cv
        val k = cols.size
        println (s"k = $k, n = ${x.dim2}")
        new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "R^2 cv"),
                   s"R^2 vs n for Quadratic X Ridge Regression with $tech", lines = true)
        println (s"$tech: rSq = $rSq")
    end for

end symRidgeRegressionTest3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `symRidgeRegressionTest4` main function tests the `SymRidgeRegression`
 *  object using the AutoMPG dataset.  Assumes no missing values.
 *  It tests "Cubic Ridge Regression" (with cross = false) and
 *  applies forward selection, backward elimination, or stepwise regression.
 *  > runMain scalation.modeling.symRidgeRegressionTest4
 */
@main def symRidgeRegressionTest4 (): Unit =

//  println (s"x = $x")
//  println (s"y = $y")

    banner ("AutoMPG Cubic Ridge Regression")
    val mod = SymRidgeRegression.cubic (x, y, x_fname)                    // add x^2 and x^3 terms
    mod.trainNtest ()()                                                   // train and test the model
    println (mod.summary ())                                              // parameter/coefficient statistics

    for tech <- SelectionTech.values do 
        banner (s"Feature Selection Technique: $tech")
        val (cols, rSq) = mod.selectFeatures (tech)                       // R^2, R^2 bar, R^2 cv
        val k = cols.size
        println (s"k = $k, n = ${x.dim2}")
        new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "R^2 cv"),
                   s"R^2 vs n for Cubic Ridge Regression with $tech", lines = true)
        println (s"$tech: rSq = $rSq")
    end for

end symRidgeRegressionTest4


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `symRidgeRegressionTest5` main function tests the `SymRidgeRegression`
 *  object using the AutoMPG dataset.  Assumes no missing values.
 *  It tests "Cubic X Ridge Regression" (with cross = true) and
 *  applies forward selection, backward elimination, or stepwise regression.
 *  > runMain scalation.modeling.symRidgeRegressionTest5
 */
@main def symRidgeRegressionTest5 (): Unit =

//  println (s"x = $x")
//  println (s"y = $y")

    banner ("AutoMPG Cubic X Ridge Regression")
    val mod = SymRidgeRegression.cubic (x, y, x_fname, true)              // add cross-terms, x^2 and x^3 terms
    mod.trainNtest ()()                                                   // train and test the model
    println (mod.summary ())                                              // parameter/coefficient statistics

    for tech <- SelectionTech.values do 
        banner (s"Feature Selection Technique: $tech")
        val (cols, rSq) = mod.selectFeatures (tech)                       // R^2, R^2 bar, R^2 cv
        val k = cols.size
        println (s"k = $k, n = ${x.dim2}")
        new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "R^2 cv"),
                   s"R^2 vs n for Cubic X Ridge Regression with $tech", lines = true)
        println (s"$tech: rSq = $rSq")
    end for

end symRidgeRegressionTest5


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `symRidgeRegressionTest6` main function tests the `SymRidgeRegression`
 *  object using the AutoMPG dataset.  Assumes no missing values.
 *  It tests "Cubic XX Ridge Regression" (with cross, cross3 = true) and
 *  applies forward selection, backward elimination, or stepwise regression.
 *  WARNING: setting cross3 = true can lead to an explotion of terms.
 *  > runMain scalation.modeling.symRidgeRegressionTest6
 */
@main def symRidgeRegressionTest6 (): Unit =

//  println (s"x = $x")
//  println (s"y = $y")

    banner ("AutoMPG Cubic XX Ridge Regression")
    val mod = SymRidgeRegression.cubic (x, y, x_fname,                  // add x^2 and x^3 terms
                                        true, true)                     // add cross and cross3 terms
    mod.trainNtest ()()                                                 // train and test the model
    println (mod.summary ())                                            // parameter/coefficient statistics

    for tech <- SelectionTech.values do
        banner (s"Feature Selection Technique: $tech")
        val (cols, rSq) = mod.selectFeatures (tech)                     // R^2, R^2 bar, R^2 cv
        val k = cols.size
        println (s"k = $k, n = ${x.dim2}")
        new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "R^2 cv"),
                   s"R^2 vs n for Cubic XX Ridge Regression with $tech", lines = true)
        println (s"$tech: rSq = $rSq")
    end for

end symRidgeRegressionTest6


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `symRidgeRegressionTest7` main function tests the `SymRidgeRegression`
 *  object using the AutoMPG dataset.  Assumes no missing values.
 *  It tests custom "Symbolic Ridge Regression", with powers specified in "Set (...)" and
 *  applies forward selection, backward elimination, or stepwise regression.
 *  This test case performs data rescaling.
 *  > runMain scalation.modeling.symRidgeRegressionTest7
 */
@main def symRidgeRegressionTest7 (): Unit =

//  println (s"x = $x")
//  println (s"y = $y")

    banner ("AutoMPG Symbolic Ridge Regression")
    val mod = SymRidgeRegression.rescale (x, y, x_fname,
                                          Set (-2, -1, 0.5, 2))          // add cross-terms and given powers
    mod.trainNtest ()()                                                  // train and test the model
    println (mod.summary ())                                             // parameter/coefficient statistics

    for tech <- SelectionTech.values do
        banner (s"Feature Selection Technique: $tech")
        val (cols, rSq) = mod.selectFeatures (tech)                      // R^2, R^2 bar, R^2 cv
        val k = cols.size
        println (s"k = $k, n = ${x.dim2}")
        new PlotM (null, rSq.transpose, Array ("R^2", "R^2 bar", "R^2 cv"),
                   s"R^2 vs n for Symbolic Ridge Regression with $tech", lines = true)
        println (s"$tech: rSq = $rSq")
    end for

end symRidgeRegressionTest7

