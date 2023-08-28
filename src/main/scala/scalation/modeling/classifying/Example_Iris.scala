
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Feb 16 16:14:34 EST 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Example Dataset: Iris Flowers
 */

package scalation
package modeling
package classifying

import scala.collection.mutable.Set

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Example_Iris` object is used to test all classifiers.
 *  This is the well-known classification problem on how to classify a flower
 *      val x = xy(?, 1 until 5)               // columns 1, 2, 3, 4
 *      val y = xy(?, 5).toInt                 // column 5
 *  @see https://en.wikipedia.org/wiki/Iris_flower_data_set
 */
object Example_Iris:

    val fname = Array ("Sepal length", "Sepal width", "Petal length", "Petal width")

    // combined data matrix [ x | y ]
    // dataset ----------------------------------------------------------------
    // Columns: Index, Sepal length, Sepal width, Petal length, Petal width, Species
    // x0: Sepal length
    // x1: Sepal width
    // x2: Petal length
    // x3: Petal width
    // y:  Species - the response/classification decision
    // variables/features:    idx   x0   x1   x2   x3  y
    val xy = MatrixD ((150, 6), 1, 5.1, 3.5, 1.4, 0.2, 0,    // I. setosa
                                2, 4.9, 3.0, 1.4, 0.2, 0,    // I. setosa
                                3, 4.7, 3.2, 1.3, 0.2, 0,    // I. setosa
                                4, 4.6, 3.1, 1.5, 0.2, 0,    // I. setosa
                                5, 5.0, 3.6, 1.4, 0.3, 0,    // I. setosa
                                6, 5.4, 3.9, 1.7, 0.4, 0,    // I. setosa
                                7, 4.6, 3.4, 1.4, 0.3, 0,    // I. setosa
                                8, 5.0, 3.4, 1.5, 0.2, 0,    // I. setosa
                                9, 4.4, 2.9, 1.4, 0.2, 0,    // I. setosa
                               10, 4.9, 3.1, 1.5, 0.1, 0,    // I. setosa
                               11, 5.4, 3.7, 1.5, 0.2, 0,    // I. setosa
                               12, 4.8, 3.4, 1.6, 0.2, 0,    // I. setosa
                               13, 4.8, 3.0, 1.4, 0.1, 0,    // I. setosa
                               14, 4.3, 3.0, 1.1, 0.1, 0,    // I. setosa
                               15, 5.8, 4.0, 1.2, 0.2, 0,    // I. setosa
                               16, 5.7, 4.4, 1.5, 0.4, 0,    // I. setosa
                               17, 5.4, 3.9, 1.3, 0.4, 0,    // I. setosa
                               18, 5.1, 3.5, 1.4, 0.3, 0,    // I. setosa
                               19, 5.7, 3.8, 1.7, 0.3, 0,    // I. setosa
                               20, 5.1, 3.8, 1.5, 0.3, 0,    // I. setosa
                               21, 5.4, 3.4, 1.7, 0.2, 0,    // I. setosa
                               22, 5.1, 3.7, 1.5, 0.4, 0,    // I. setosa
                               23, 4.6, 3.6, 1.0, 0.2, 0,    // I. setosa
                               24, 5.1, 3.3, 1.7, 0.5, 0,    // I. setosa
                               25, 4.8, 3.4, 1.9, 0.2, 0,    // I. setosa
                               26, 5.0, 3.0, 1.6, 0.2, 0,    // I. setosa
                               27, 5.0, 3.4, 1.6, 0.4, 0,    // I. setosa
                               28, 5.2, 3.5, 1.5, 0.2, 0,    // I. setosa
                               29, 5.2, 3.4, 1.4, 0.2, 0,    // I. setosa
                               30, 4.7, 3.2, 1.6, 0.2, 0,    // I. setosa
                               31, 4.8, 3.1, 1.6, 0.2, 0,    // I. setosa
                               32, 5.4, 3.4, 1.5, 0.4, 0,    // I. setosa
                               33, 5.2, 4.1, 1.5, 0.1, 0,    // I. setosa
                               34, 5.5, 4.2, 1.4, 0.2, 0,    // I. setosa
                               35, 4.9, 3.1, 1.5, 0.2, 0,    // I. setosa
                               36, 5.0, 3.2, 1.2, 0.2, 0,    // I. setosa
                               37, 5.5, 3.5, 1.3, 0.2, 0,    // I. setosa
                               38, 4.9, 3.6, 1.4, 0.1, 0,    // I. setosa
                               39, 4.4, 3.0, 1.3, 0.2, 0,    // I. setosa
                               40, 5.1, 3.4, 1.5, 0.2, 0,    // I. setosa
                               41, 5.0, 3.5, 1.3, 0.3, 0,    // I. setosa
                               42, 4.5, 2.3, 1.3, 0.3, 0,    // I. setosa
                               43, 4.4, 3.2, 1.3, 0.2, 0,    // I. setosa
                               44, 5.0, 3.5, 1.6, 0.6, 0,    // I. setosa
                               45, 5.1, 3.8, 1.9, 0.4, 0,    // I. setosa
                               46, 4.8, 3.0, 1.4, 0.3, 0,    // I. setosa
                               47, 5.1, 3.8, 1.6, 0.2, 0,    // I. setosa
                               48, 4.6, 3.2, 1.4, 0.2, 0,    // I. setosa
                               49, 5.3, 3.7, 1.5, 0.2, 0,    // I. setosa
                               50, 5.0, 3.3, 1.4, 0.2, 0,    // I. setosa
                               51, 7.0, 3.2, 4.7, 1.4, 1,    // I. versicolor
                               52, 6.4, 3.2, 4.5, 1.5, 1,    // I. versicolor
                               53, 6.9, 3.1, 4.9, 1.5, 1,    // I. versicolor
                               54, 5.5, 2.3, 4.0, 1.3, 1,    // I. versicolor
                               55, 6.5, 2.8, 4.6, 1.5, 1,    // I. versicolor
                               56, 5.7, 2.8, 4.5, 1.3, 1,    // I. versicolor
                               57, 6.3, 3.3, 4.7, 1.6, 1,    // I. versicolor
                               58, 4.9, 2.4, 3.3, 1.0, 1,    // I. versicolor
                               59, 6.6, 2.9, 4.6, 1.3, 1,    // I. versicolor
                               60, 5.2, 2.7, 3.9, 1.4, 1,    // I. versicolor
                               61, 5.0, 2.0, 3.5, 1.0, 1,    // I. versicolor
                               62, 5.9, 3.0, 4.2, 1.5, 1,    // I. versicolor
                               63, 6.0, 2.2, 4.0, 1.0, 1,    // I. versicolor
                               64, 6.1, 2.9, 4.7, 1.4, 1,    // I. versicolor
                               65, 5.6, 2.9, 3.6, 1.3, 1,    // I. versicolor
                               66, 6.7, 3.1, 4.4, 1.4, 1,    // I. versicolor
                               67, 5.6, 3.0, 4.5, 1.5, 1,    // I. versicolor
                               68, 5.8, 2.7, 4.1, 1.0, 1,    // I. versicolor
                               69, 6.2, 2.2, 4.5, 1.5, 1,    // I. versicolor
                               70, 5.6, 2.5, 3.9, 1.1, 1,    // I. versicolor
                               71, 5.9, 3.2, 4.8, 1.8, 1,    // I. versicolor
                               72, 6.1, 2.8, 4.0, 1.3, 1,    // I. versicolor
                               73, 6.3, 2.5, 4.9, 1.5, 1,    // I. versicolor
                               74, 6.1, 2.8, 4.7, 1.2, 1,    // I. versicolor
                               75, 6.4, 2.9, 4.3, 1.3, 1,    // I. versicolor
                               76, 6.6, 3.0, 4.4, 1.4, 1,    // I. versicolor
                               77, 6.8, 2.8, 4.8, 1.4, 1,    // I. versicolor
                               78, 6.7, 3.0, 5.0, 1.7, 1,    // I. versicolor
                               79, 6.0, 2.9, 4.5, 1.5, 1,    // I. versicolor
                               80, 5.7, 2.6, 3.5, 1.0, 1,    // I. versicolor
                               81, 5.5, 2.4, 3.8, 1.1, 1,    // I. versicolor
                               82, 5.5, 2.4, 3.7, 1.0, 1,    // I. versicolor
                               83, 5.8, 2.7, 3.9, 1.2, 1,    // I. versicolor
                               84, 6.0, 2.7, 5.1, 1.6, 1,    // I. versicolor
                               85, 5.4, 3.0, 4.5, 1.5, 1,    // I. versicolor
                               86, 6.0, 3.4, 4.5, 1.6, 1,    // I. versicolor
                               87, 6.7, 3.1, 4.7, 1.5, 1,    // I. versicolor
                               88, 6.3, 2.3, 4.4, 1.3, 1,    // I. versicolor
                               89, 5.6, 3.0, 4.1, 1.3, 1,    // I. versicolor
                               90, 5.5, 2.5, 4.0, 1.3, 1,    // I. versicolor
                               91, 5.5, 2.6, 4.4, 1.2, 1,    // I. versicolor
                               92, 6.1, 3.0, 4.6, 1.4, 1,    // I. versicolor
                               93, 5.8, 2.6, 4.0, 1.2, 1,    // I. versicolor
                               94, 5.0, 2.3, 3.3, 1.0, 1,    // I. versicolor
                               95, 5.6, 2.7, 4.2, 1.3, 1,    // I. versicolor
                               96, 5.7, 3.0, 4.2, 1.2, 1,    // I. versicolor
                               97, 5.7, 2.9, 4.2, 1.3, 1,    // I. versicolor
                               98, 6.2, 2.9, 4.3, 1.3, 1,    // I. versicolor
                               99, 5.1, 2.5, 3.0, 1.1, 1,    // I. versicolor
                              100, 5.7, 2.8, 4.1, 1.3, 1,    // I. versicolor
                              101, 6.3, 3.3, 6.0, 2.5, 2,    // I. virginica
                              102, 5.8, 2.7, 5.1, 1.9, 2,    // I. virginica
                              103, 7.1, 3.0, 5.9, 2.1, 2,    // I. virginica
                              104, 6.3, 2.9, 5.6, 1.8, 2,    // I. virginica
                              105, 6.5, 3.0, 5.8, 2.2, 2,    // I. virginica
                              106, 7.6, 3.0, 6.6, 2.1, 2,    // I. virginica
                              107, 4.9, 2.5, 4.5, 1.7, 2,    // I. virginica
                              108, 7.3, 2.9, 6.3, 1.8, 2,    // I. virginica
                              109, 6.7, 2.5, 5.8, 1.8, 2,    // I. virginica
                              110, 7.2, 3.6, 6.1, 2.5, 2,    // I. virginica
                              111, 6.5, 3.2, 5.1, 2.0, 2,    // I. virginica
                              112, 6.4, 2.7, 5.3, 1.9, 2,    // I. virginica
                              113, 6.8, 3.0, 5.5, 2.1, 2,    // I. virginica
                              114, 5.7, 2.5, 5.0, 2.0, 2,    // I. virginica
                              115, 5.8, 2.8, 5.1, 2.4, 2,    // I. virginica
                              116, 6.4, 3.2, 5.3, 2.3, 2,    // I. virginica
                              117, 6.5, 3.0, 5.5, 1.8, 2,    // I. virginica
                              118, 7.7, 3.8, 6.7, 2.2, 2,    // I. virginica
                              119, 7.7, 2.6, 6.9, 2.3, 2,    // I. virginica
                              120, 6.0, 2.2, 5.0, 1.5, 2,    // I. virginica
                              121, 6.9, 3.2, 5.7, 2.3, 2,    // I. virginica
                              122, 5.6, 2.8, 4.9, 2.0, 2,    // I. virginica
                              123, 7.7, 2.8, 6.7, 2.0, 2,    // I. virginica
                              124, 6.3, 2.7, 4.9, 1.8, 2,    // I. virginica
                              125, 6.7, 3.3, 5.7, 2.1, 2,    // I. virginica
                              126, 7.2, 3.2, 6.0, 1.8, 2,    // I. virginica
                              127, 6.2, 2.8, 4.8, 1.8, 2,    // I. virginica
                              128, 6.1, 3.0, 4.9, 1.8, 2,    // I. virginica
                              129, 6.4, 2.8, 5.6, 2.1, 2,    // I. virginica
                              130, 7.2, 3.0, 5.8, 1.6, 2,    // I. virginica
                              131, 7.4, 2.8, 6.1, 1.9, 2,    // I. virginica
                              132, 7.9, 3.8, 6.4, 2.0, 2,    // I. virginica
                              133, 6.4, 2.8, 5.6, 2.2, 2,    // I. virginica
                              134, 6.3, 2.8, 5.1, 1.5, 2,    // I. virginica
                              135, 6.1, 2.6, 5.6, 1.4, 2,    // I. virginica
                              136, 7.7, 3.0, 6.1, 2.3, 2,    // I. virginica
                              137, 6.3, 3.4, 5.6, 2.4, 2,    // I. virginica
                              138, 6.4, 3.1, 5.5, 1.8, 2,    // I. virginica
                              139, 6.0, 3.0, 4.8, 1.8, 2,    // I. virginica
                              140, 6.9, 3.1, 5.4, 2.1, 2,    // I. virginica
                              141, 6.7, 3.1, 5.6, 2.4, 2,    // I. virginica
                              142, 6.9, 3.1, 5.1, 2.3, 2,    // I. virginica
                              143, 5.8, 2.7, 5.1, 1.9, 2,    // I. virginica
                              144, 6.8, 3.2, 5.9, 2.3, 2,    // I. virginica
                              145, 6.7, 3.3, 5.7, 2.5, 2,    // I. virginica
                              146, 6.7, 3.0, 5.2, 2.3, 2,    // I. virginica
                              147, 6.3, 2.5, 5.0, 1.9, 2,    // I. virginica
                              148, 6.5, 3.0, 5.2, 2.0, 2,    // I. virginica
                              149, 6.2, 3.4, 5.4, 2.3, 2,    // I. virginica
                              150, 5.9, 3.0, 5.1, 1.8, 2)    // I. virginica

    // data for ternary classifiers
    val k = 3
    val x = xy(?, 1 until 5)                                 // columns 1, 2, 3, 4
    val y = xy(?, 5).toInt                                   // column 5

    // data for binary classifiers
    val kk = 2
    val xx = x(0 until 100)                                  // first 100 rows
    val yy = y(0 until 100)                                  // first 100 elements
    val yb = y.copy                                          // imbalanced copy of y
    for i <- 100 until 150 do yb(i) = 1

    val continuous = Set (0, 1, 2, 3)                        // features that are continuous

end Example_Iris


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_IrisTest` main function tests 16 of 18 classifiers on the Iris dataset.
 *  As this is an easy classification problem, classifiers should be nearly perfect.
 *
 *  BaggingTrees, DecisionTree_C45, DecisionTree_C45wp, DecisionTree_ID3, DecisionTree_ID3wp,
 *  HiddenMarkov, KNN_Classifier, LinDiscAnalyis, LogisticRegression, NaiveBayes,
 *  NaiveBayesR, NeuralNet_Class_3L, NullModel, RandomForest, SupportVectorMachine, TANBayes.
 *
 *  Require having only a single feature: SimpleLDA, SimpleLogisticRegression => SKIP
 *
 *  > runMain scalation.modeling.classifying.example_IrisTest
 */
@main def example_IrisTest (): Unit =

    import Example_Iris._

    val xr = MatrixI.roundMat (xx)                           // rounded to integers

    var mod: Classifier & FitC = null                        // build models using constructors

// Basic/Null Classifiers (1)

    banner ("Example_Iris: NullModel")
    mod = new NullModel (yy)
    mod.trainNtest ()()
    println (mod.summary ())

// Bayesian Classifiers (4)

    banner ("Example_Iris: NaiveBayes")
    mod = new NaiveBayes (xr, yy, fname)
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_Iris: NaiveBayesR")                     // FIX - fails
    mod = new NaiveBayesR (xx, yy, fname)
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_Iris: TANBayes")
    mod = new TANBayes (xr, yy, fname)
    mod.trainNtest ()()
    println (mod.summary ())

//  banner ("Example_Iris: HiddenMarkov")                    // FIX - Under Development
//  mod = new HiddenMarkov (xr, yy, fname)
//  mod.trainNtest ()()
//  println (mod.summary ())

// Logistic Regression/LDA Classifiers (2)

    banner ("Example_Iris: LogisticRegression")
    mod = new LogisticRegression (xx, yy, fname)
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_Iris: LinDiscAnalyis")
    mod = new LinDiscAnalyis (xx, yy, fname)
    mod.trainNtest ()()// Logistic Regression/LDA Classifiers (2)
    println (mod.summary ())

// K-Nearest Neighbors Classifiers (1)

    banner ("Example_Iris: KNN_Classifier")
    mod = new KNN_Classifier (xx, yy, fname)
    mod.trainNtest ()()
    println (mod.summary ())

// Decision/Classification Tree Classifiers (6)

    banner ("Example_Iris: DecisionTree_ID3")
    mod = new DecisionTree_ID3 (xr, yy, fname)
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_Iris: DecisionTree_ID3wp")
    mod = new DecisionTree_ID3wp (xr, yy, fname)
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_Iris: DecisionTree_C45")
    mod = new DecisionTree_C45 (xx, yy, fname, conts = continuous)
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_Iris: DecisionTree_C45wp")
    mod = new DecisionTree_C45wp (xx, yy, fname, conts = continuous)
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_Iris: BaggingTrees")
    mod = new BaggingTrees (xx, yy, fname, conts = continuous)
    mod.trainNtest ()()
    println (mod.summary ())

    banner ("Example_Iris: RandomForest")
    mod = new RandomForest (xx, yy, fname, conts = continuous)
    mod.trainNtest ()()
    println (mod.summary ())

// Support Vector Machine Classifiers (1)

    banner ("Example_Iris: SupportVectorMachine")            // FIX - fails
    mod = new SupportVectorMachine (xx, yy.map2 (0, -1), fname)
    mod.trainNtest ()()
    println (mod.summary ())

// Neural Network Classifiers (1)

    banner ("Example_Iris: NeuralNet_Class_3L")
    mod = new NeuralNet_Class_3L (xx, yy, fname)
    mod.trainNtest ()()
    println (mod.summary ())

end example_IrisTest

