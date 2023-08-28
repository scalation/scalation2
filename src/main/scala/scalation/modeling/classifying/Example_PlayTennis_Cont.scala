
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Feb 16 16:14:34 EST 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Example Dataset: Continuous Version of Play Tennis
 */

package scalation
package modeling
package classifying

import scala.collection.mutable.Set

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Example_PlayTennis_Cont` object is used to test integer/continuous classifiers.
 *  This is the well-known classification problem on whether to play tennis
 *  based on given weather conditions.  Applications may need to slice 'xy'.
 *  The 'Cont' version uses continuous values for Temperature and Humidity.
 *      val x = xy.not (?, 4)       // columns 0, 1, 2, 3
 *      val y = xy(?, 4)            // column 4
 *  @see euclid.nmu.edu/~mkowalcz/cs495f09/slides/lesson004.pdf
 *  @see sefiks.com/2018/05/13/a-step-by-step-c4-5-decision-tree-example
 */
object Example_PlayTennis_Cont:

    // combined data matrix [ x | y ]
    // dataset ----------------------------------------------------------------
    // x0: Outlook:     Rain (0),   Overcast (1), Sunny (2)
    // x1: Temperature: Continuous
    // x2: Humidity:    Continuous
    // x3: Wind:        Weak (0),   Strong (1)
    // y:  the response/classification decision
    // variables/features:      x0    x1     x2      x3    y
    val xy = MatrixD ((14, 5),  2,    85,    85,     0,    0,      // day  1
                                2,    80,    90,     1,    0,      // day  2
                                1,    83,    78,     0,    1,      // day  3
                                0,    70,    96,     0,    1,      // day  4
                                0,    68,    80,     0,    1,      // day  5
                                0,    65,    70,     1,    0,      // day  6
                                1,    64,    65,     1,    1,      // day  7
                                2,    72,    95,     0,    0,      // day  8
                                2,    69,    70,     0,    1,      // day  9
                                0,    75,    80,     0,    1,      // day 10
                                2,    75,    70,     1,    1,      // day 11
                                1,    72,    90,     1,    1,      // day 12
                                1,    81,    75,     0,    1,      // day 13
                                0,    71,    80,     1,    0)      // day 14

    val fname = Array ("Outlook", "Temp", "Humidity", "Wind")          // feature/variable names
    val conts = Set (1, 2)                                             // set of continuous features
    val cname = Array ("No", "Yes")                                    // class names for y
    val k     = cname.size                                             // number of classes

    val x = xy.not (?, 4)                                              // columns 0, 1, 2, 3
    val y = xy(?, 4).toInt                                             // column 4

end Example_PlayTennis_Cont


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `example_PalyTennis_ContTest` test several classifiers on the (cont) Play Tennis dataset.
 *  Tests all classes that extend from `Classifier` and include continuous predictors.
 *  > runMain scalation.modeling.classifying.Example_PlayTennis_ContTest
 */
@main def example_PlayTennis_ContTest (): Unit =

    import Example_PlayTennis_Cont._

    banner ("NullModel")
    val nm = new NullModel (y)
    nm.trainNtest ()()
    println (nm.summary ())

    banner ("NaiveBayesR")
    val nbr = new NaiveBayesR (x, y)
    nbr.trainNtest ()()
    println (nbr.summary ())

    banner ("SimpleLogisticRegression")
    val srg = new SimpleLogisticRegression (x, y)
    srg.trainNtest ()()
    println (srg.summary ())

    banner ("LogisticRegression")
    val lrg = new LogisticRegression (x, y)
    lrg.trainNtest ()()
    println (lrg.summary ())

    banner ("KNN_Classifier")
    val knn = new KNN_Classifier (x, y)
    knn.trainNtest ()()
    println (knn.summary ())

    banner ("SimpleLDA")
    val sda = new SimpleLDA (x(?, 0 until 1), y)       // just use column 0
    sda.trainNtest ()()
    println (sda.summary ())

    banner ("LinDiscAnalyis")
    val lda = new LinDiscAnalyis (x, y)
    lda.trainNtest ()()
    println (lda.summary ())

/*
    banner ("DecisionTree_C45")
    val dtc = new DecisionTree_C45 (x, y, conts = conts)
    dtc.trainNtest ()()
    println (dtc.summary ())

    banner ("RandomForest")
    val rf = new RandomForest (x, y, conts = conts)
    rf.trainNtest ()()
    println (rf.summary ())

    banner ("SupportVectorMachine")
    val svm = new SupportVectorMachine (x, y)
    svm.trainNtest ()()
    println (svm.summary ())
*/

end example_PlayTennis_ContTest

