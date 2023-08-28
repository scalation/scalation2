
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Apr  8 14:11:45 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Example: Breast Cancer Dataset
 */

package scalation
package modeling
package classifying

import scalation.mathstat.MatrixD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Example_BreastCancer` object loads the breast cancer dataset for
 *  classifying whether a patient has breast cancer.
 */
object Example_BreastCancer:

    val nfile = "breast_cancer.csv"
    val xy    = MatrixD.load (nfile)
    val fname = Array ("Clump Thickness", "Uniformity of Cell Size", "Uniformity of Cell Shape", "Marginal Adhesion",
                       "Single Epithelial Cell Size", "Bare Nuclei", "Bland Chromatin", "Normal Nucleoli", "Mitoses")
    val cname = Array ("benign", "malignant")
    val k     = cname.size

end Example_BreastCancer


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_BreastCancerTest` main function tests 16 of 18 classifiers on the Breast Cancer dataset.
 *  Ex: Classify whether a there is breast cancer.
 *
 *  BaggingTrees, DecisionTree_C45, DecisionTree_C45wp, DecisionTree_ID3, DecisionTree_ID3wp,
 *  HiddenMarkov, KNN_Classifier, LinDiscAnalyis, LogisticRegression, NaiveBayes,
 *  NaiveBayesR, NeuralNet_Class_3L, NullModel, RandomForest, SupportVectorMachine, TANBayes.
 *
 *  Require having only a single feature: SimpleLDA, SimpleLogisticRegression => SKIP
 *
 *  > runMain scalation.modeling.classifying.example_BreastCancerTest
 */
@main def example_BreastCancerTest (): Unit =

    import Example_BreastCancer._

    var mod: Classifier & FitC = null                        // build models usiing factory method

// Basic/Null Classifiers (1)

    banner ("Example_BreastCancer: NullModel")
    mod = NullModel (xy)()
    mod.trainNtest ()()
    println (mod.summary ())

// Bayesian Classifiers (4)

    banner ("Example_BreastCancer: NaiveBayes")
    mod = NaiveBayes (xy, fname)()
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_BreastCancer: NaiveBayesR")            // FIX - fails
    mod = NaiveBayesR (xy, fname)()
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_BreastCancer: TANBayes")
    mod = TANBayes (xy, fname)()
    mod.trainNtest ()()
    println (mod.summary ())

//  banner ("Example_Iris: HiddenMarkov")                    // FIX - Under Development
//  mod = HiddenMarkov (xy, fname)()
//  mod.trainNtest ()()
//  println (mod.summary ())

// Logistic Regression/LDA Classifiers (2)

    banner ("Example_BreasCancer: LogisticRegression")
//  for ic <- 35 to 65 by 5 do                               // find best (f1) decision threshold
    for ic <- 65 to 65 by 5 do                               // found best (f1) decision threshold
        Classifier.hp("cThresh") = ic / 100.0
        mod = LogisticRegression (xy, fname)()
        mod.trainNtest ()()
        println (mod.summary ())
    end for

    banner ("Example_BreastCancer: LinDiscAnalyis")
    mod = LinDiscAnalyis (xy, fname)()
    mod.trainNtest ()()
    println (mod.summary ())

// K-Nearest Neighbors Classifiers (1)

    banner ("Example_BreastCancer: KNN_Classifier")
    mod = KNN_Classifier (xy, fname)()
    mod.trainNtest ()()
    println (mod.summary ())

// Decision/Classification Tree Classifiers (6)

    banner ("Example_BreastCancer: DecisionTree_ID3")
    mod = DecisionTree_ID3 (xy, fname)()
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_BreastCancer: DecisionTree_ID3wp")
    mod = DecisionTree_ID3wp (xy, fname)()
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_BreastCancer: DecisionTree_C45")
    mod = DecisionTree_C45 (xy, fname)()
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_BreastCancer: DecisionTree_C45wp")
    mod = DecisionTree_C45wp (xy, fname)()
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_BreastCancer: BaggingTrees")
    mod = BaggingTrees (xy, fname)()
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_BreastCancer: RandomForest")
    mod = RandomForest (xy, fname)()
    mod.trainNtest ()()
    println (mod.summary ())

// Support Vector Machine Classifiers (1)

    banner ("Example_BreastCancer: SupportVectorMachine")
    mod = SupportVectorMachine (xy, fname)()
    mod.trainNtest ()()
    println (mod.summary ())

// Neural Network Classifiers (1)

    banner ("Example_BreastCancer: NeuralNet_Class_3L")
    NeuralNet_Class_3L.hp("cThresh") = 0.55
    mod = NeuralNet_Class_3L (xy, fname)()
    mod.trainNtest ()()
    println (mod.summary ())

end example_BreastCancerTest

