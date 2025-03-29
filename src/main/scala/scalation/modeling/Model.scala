
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Jun  9 16:42:16 EDT 2019
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Framework: Base Trait for all Models
 */

package scalation
package modeling

import java.net.URI

import scala.runtime.ScalaRunTime.stringOf

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Order vectors y_ and yp_ according to the ascending order of y_.
 *  This can be used for graphical comparison or true and predicted values.
 *  @param y_   the vector to order by (e.g., true response values)
 *  @param yp_  the vector to be order by y_ (e.g., predicted response values)
 */
def orderByY (y_ : VectorD, yp_ : VectorD): (VectorD, VectorD) =
    val rank = y_.iqsort                                                 // rank order for vector y_
    (y_.reorder (rank), yp_.reorder (rank))                              // (y_ in ascending order, yp_ ordered by y_)
end orderByY

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Order matrices y_ and yp_ according to the ascending order of y_
 *  (column vector by column vector).
 *  This can be used for graphical comparison or true and predicted values.
 *  @param y_   the matrix to order by (e.g., true response values)
 *  @param yp_  the matrix to be order by y_ (e.g., predicted response values)
 */
def orderByYY (y_ : MatrixD, yp_ : MatrixD): (MatrixD, MatrixD) =
    val (oy, oyp) = (new MatrixD (y_.dim, y_.dim2), new MatrixD (yp_.dim, yp_.dim2))
    for j <- y_.indices2 do
        val yj    = y_(?, j)                                             // column j of y_
        val rank  = yj.iqsort                                            // rank order for vector yj
        oy(?, j)  = yj.reorder (rank)                                    // yj in ascending order
        oyp(?, j) = yp_(?, j).reorder (rank)                             // column j of yp_ ordered by yj
    end for
    (oy, oyp)
end orderByYY


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Model` trait provides a common framework for all models and serves as
 *  base trait for `Classifier`, `Forecaster`, `Predictor`, and `PredictorMV` traits.
 *  The train and test methods must be called first, e.g.,
 *       val model = NullModel (y)
 *       model.train (null, y)
 *       model.test (null, y)
 */
trait Model:

    /** The optional reference to an ontological concept
     */
    var modelConcept: URI = null

    /** The name for the model (or modeling technique).
     */
    var modelName: String = "Model"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the used data matrix x.  Mainly for derived classes where x is expanded
     *  from the given columns in x_, e.g., `SymbolicRegression.quadratic` adds squared columns.
     */
    def getX: MatrixD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the used response vector y.  Mainly for derived classes where y is
     *  transformed, e.g., `TranRegression`, `ARX`.
     */
    def getY: VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the used response matrix y, if needed.
     *  @see `neuralnet.PredictorMV`
     */
    def getYY: MatrixD = null

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the feature/variable names.
     */
    def getFname: Array [String]

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Train the model 'y_ = f(x_) + e' on a given dataset, by optimizing the model
     *  parameters in order to minimize error '||e||' or maximize log-likelihood 'll'.
     *  @param x_  the training/full data/input matrix (impl. classes may default to x)
     *  @param y_  the training/full response/output vector (impl. classes may default to y)
     */
    def train (x_ : MatrixD, y_ : VectorD): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test/evaluate the model's Quality of Fit (QoF) and return the predictions
     *  and QoF vectors.
     *  This may include the importance of its parameters (e.g., if 0 is in a parameter's
     *  confidence interval, it is a candidate for removal from the model).
     *  Extending traits and classess should implement various diagnostics for
     *  the test and full (training + test) datasets.
     *  @param x_  the testiing/full data/input matrix (impl. classes may default to x)
     *  @param y_  the testiing/full response/output vector (impl. classes may default to y)
     */
    def test (x_ : MatrixD, y_ : VectorD): (VectorD, VectorD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the value of y = f(z) by evaluating the model equation.
     *  Single output models return `Double`, while multi-output models return `VectorD`.
     *  @param z  the new vector to predict
     */
    def predict (z: VectorD): Double | VectorD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the model hyper-parameters (if none, return null).  Hyper-parameters
     *  may be used to regularize parameters or tune the optimizer.
     */
    def hparameter: HyperParameter

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the vector of model parameter/coefficient values.
     *  Single output models have `VectorD` parameters, while multi-output models have `MatrixD`.
     */
    def parameter: VectorD | MatrixD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a basic report on a trained and tested model.
     *  @param ftVec  the vector of qof values produced by the `Fit` trait
     */
    def report (ftVec: VectorD): String =
        s"""
REPORT
    ----------------------------------------------------------------------------
    modelName  mn  = $modelName
    ----------------------------------------------------------------------------
    hparameter hp  = $hparameter
    ----------------------------------------------------------------------------
    features   fn  = ${stringOf (getFname)}
    ----------------------------------------------------------------------------
    parameter  b   = $parameter
    ----------------------------------------------------------------------------
    fitMap     qof = ${FitM.fitMap (ftVec, QoF.values.map (_.toString))}
    ----------------------------------------------------------------------------
        """
    end report

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a basic report on a trained and tested multi-variate model.
     *  @param ftMat  the matrix of qof values produced by the `Fit` trait
     */
    def report (ftMat: MatrixD): String =
        s"""
REPORT
    ----------------------------------------------------------------------------
    modelName  mn  = $modelName
    ----------------------------------------------------------------------------
    hparameter hp  = $hparameter
    ----------------------------------------------------------------------------
    features   fn  = ${stringOf (getFname)}
    ----------------------------------------------------------------------------
    parameter  b   = $parameter
    ----------------------------------------------------------------------------
    fitMap     qof = ${FitM.fitMap (ftMat, QoF.values.map (_.toString))}
    ----------------------------------------------------------------------------
        """
    end report

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Use k-fold cross-validation to compute test Quality of Fit (QoF) measures
     *  by iteratively dividing the full dataset into a TRAINING and a TESTING set.
     *  Each test set is defined by idx and the rest of the data is the training set.
     *  @see showQofStatTable in `Fit` object for printing the returned stats.
     *  @param k      the number of cross-validation iterations/folds (defaults to 5x).
     *  @param rando  flag indicating whether to use randomized or simple cross-validation
     */
    def crossValidate (k: Int = 5, rando: Boolean = true): Array [Statistic]

end Model

