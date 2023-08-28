
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Apr  8 14:11:45 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Example: Diabetes Dataset
 */

package scalation
package modeling
package classifying

import scala.collection.mutable.Set

import scalation.mathstat.{MatrixD, MatrixI}

import ActivationFun._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Example_Diabetes` object loads the dibetes dataset for
 *  classifying whether a patient has diabetes.
 */
object Example_Diabetes:

    val nfile = "diabetes.csv"
    val xy    = MatrixD.load (nfile)
    val fname = Array ("pregnancies", "glucose", "blood pressure", "skin thickness", "insulin",
                        "BMI", "diabetes pedigree function", "age")
    val cname = Array ("tested_positive", "tested_negative")
    val k     = cname.size

    val x = xy(?, 0 until 8)                                 // columns 0, 1, 2, 3, 4, 5, 6, 7
    val y = xy(?, 8).toInt                                   // column 8

    val continuous = Set.range (0, xy.dim2 - 1)              // features that are continous

end Example_Diabetes


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_DiabetesTest` main function tests 16 of 18 classifiers on the Breast Cancer dataset.
 *  Ex: Classify whether a there is breast cancer.
 *
 *  BaggingTrees, DecisionTree_C45, DecisionTree_C45wp, DecisionTree_ID3, DecisionTree_ID3wp,
 *  HiddenMarkov, KNN_Classifier, LinDiscAnalyis, LogisticRegression, NaiveBayes,
 *  NaiveBayesR, NeuralNet_Class_3L, NullModel, RandomForest, SupportVectorMachine, TANBayes.
 *
 *  Require having only a single feature: SimpleLDA, SimpleLogisticRegression => SKIP
 *
 *  > runMain scalation.modeling.classifying.example_DiabetesTest
 */
@main def example_DiabetesTest (): Unit =

    import Example_Diabetes._

    var mod: Classifier & FitC = null                        // build models usiing factory method

    val xr = MatrixI.roundMat (x)                            // rounded to integers

// Basic/Null Classifiers (1)

    banner ("Example_Diabetes: NullModel")
    mod = NullModel (xy)()
    mod.trainNtest ()()
    println (mod.summary ())

// Bayesian Classifiers (4)

    banner ("Example_Diabetes: NaiveBayes")
    mod = new NaiveBayes (xr, y, fname)
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_Diabetes: NaiveBayesR")                 // FIX - fails
    mod = NaiveBayesR (xy, fname)()
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_Diabetes: TANBayes")
    mod = new TANBayes (xr, y, fname)
    mod.trainNtest ()()
    println (mod.summary ())

//  banner ("Example_Iris: HiddenMarkov")                    // FIX - Under Development
//  mod = new HiddenMarkov (xr, y, fname)
//  mod.trainNtest ()()
//  println (mod.summary ())

// Logistic Regression/LDA Classifiers (2)

    banner ("Example_Diabetes: LogisticRegression")
//  for ic <- 35 to 65 by 5 do                               // find best (f1) decision threshold
    for ic <- 40 to 40 by 5 do                               // found best (f1) decision threshold
        Classifier.hp("cThresh") = ic / 100.0
        mod = LogisticRegression (xy, fname)()
        mod.trainNtest ()()
        println (mod.summary ())
    end for

    banner ("Example_Diabetes: LinDiscAnalyis")
    mod = LinDiscAnalyis (xy, fname)()
    mod.trainNtest ()()
    println (mod.summary ())

// K-Nearest Neighbors Classifiers (1)

    banner ("Example_Diabetes: KNN_Classifier")
    mod = KNN_Classifier (xy, fname)()
    mod.trainNtest ()()
    println (mod.summary ())

// Decision/Classification Tree Classifiers (6)

    banner ("Example_Diabetes: DecisionTree_ID3")
    mod = new DecisionTree_ID3 (xr, y, fname)
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_Diabetes: DecisionTree_ID3wp")
    mod = new DecisionTree_ID3wp (xr, y, fname)
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_Diabetes: DecisionTree_C45")
    mod = DecisionTree_C45 (xy, fname, conts = continuous)()
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_Diabetes: DecisionTree_C45wp")
    mod = DecisionTree_C45wp (xy, fname, conts = continuous)()
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_Diabetes: BaggingTrees")
    mod = BaggingTrees (xy, fname, conts = continuous)()
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_Diabetes: RandomForest")
    mod = RandomForest (xy, fname, conts = continuous)()
    mod.trainNtest ()()
    println (mod.summary ())

// Support Vector Machine Classifiers (1)

    banner ("Example_Diabetes: SupportVectorMachine")
    mod = SupportVectorMachine (xy, fname)()
    mod.trainNtest ()()
    println (mod.summary ())

// Neural Network Classifiers (1)

    banner ("Example_Diabetes: NeuralNet_Class_3L")
    NeuralNet_Class_3L.hp("cThresh") = 0.45
    mod = NeuralNet_Class_3L (xy, fname, f = f_id)()
    mod.trainNtest ()()
    println (mod.summary ())

end example_DiabetesTest

