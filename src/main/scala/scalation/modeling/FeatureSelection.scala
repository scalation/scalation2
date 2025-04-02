
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Sep 27 20:58:20 EDT 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Framework: Support for Feature Selection and Best-Step
 *
 *  @see     bookdown.org/max/FES/selection.html
 */

package scalation
package modeling

import scala.collection.mutable.LinkedHashSet => LSET

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SelectionTech` enumeration indicates the available feature selection
 *  techniques.
 */
enum SelectionTech:

     case Forward, Backward, Stepwise

end SelectionTech

// Change as needed the default (given instance) QoF metric used for Feature Selection

//given qk: Int = QoF.rSqBar.ordinal                                    // which QoF metric index to use by default - Regression
given qk: Int = QoF.smapeIC.ordinal                                     // which QoF metric index to use by default - Time Series

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FeatureSelection` trait establishes a framework for feature selection,
 *  i.e., selecting the features (e.g., variable x_j, cross term x_j x_k, or
 *  functional form x_j^2) to include in the model.
 */
trait FeatureSelection:

    private val debug = debugf ("FeatureSelection", true)               // debug function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform feature selection to find the most predictive features/variables
     *  to  have in the model, returning the features/variables added and the new
     *  Quality of Fit (QoF) measures/metrics for all steps.
     *  @see `Fit` for index of QoF measures/metrics.
     *  @param tech   the feature selection technique to apply
     *  @param cross  whether to include the cross-validation QoF measure
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     */
    def selectFeatures (tech: SelectionTech, cross: Boolean = true)(using qk: Int):
                       (LSET [Int], MatrixD) =
        debug ("selectFeatures", s"select features based on QoF metric with index qk = $qk")
        tech match
        case SelectionTech.Forward  => forwardSelAll (cross)
        case SelectionTech.Backward => backwardElimAll (1, cross)
        case SelectionTech.Stepwise => stepwiseSelAll (cross)
    end selectFeatures

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform FORWARD SELECTION to find the MOST predictive features/variables
     *  to ADD into the model, returning the features/variables added and the new
     *  Quality of Fit (QoF) measures/metrics for all steps.
     *  @see `Fit` for index of QoF measures/metrics.
     *  @param cross  whether to include the cross-validation QoF measure
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     */
    def forwardSelAll (cross: Boolean = true)(using qk: Int): (LSET [Int], MatrixD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform BACKWARD ELIMINATION to find the LEAST predictive features/variables
     *  to REMOVE from the full model, returning the features/variables left and the
     *  new Quality of Fit (QoF)  measures/metrics for all steps.
     *  @see `Fit` for index of QoF measures/metrics.
     *  @param first  first variable to consider for elimination
     *  @param cross  whether to include the cross-validation QoF measure
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     */
    def backwardElimAll (first: Int = 1, cross: Boolean = true)(using qk: Int): (LSET [Int], MatrixD)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform STEPWISE SELECTION to find a GOOD COMBINATION of predictive features/variables
     *  to have in the model, returning the features/variables left and the new Quality of Fit
     *  (QoF) measures/metrics for all steps.  At each step, it calls forward and backward
     *  and takes the best of the two actions.  Stops when neither action yields improvement.
     *  @see `Fit` for index of QoF measures/metrics.
     *  @param cross  whether to include the cross-validation QoF measure
     *  @param swap   whether to allow a swap step (swap out a feature for a new feature in one step)
     *  @param qk     index of Quality of Fit (QoF) to use for comparing quality
     */
    def stepwiseSelAll (cross: Boolean = true, swap: Boolean = true)(using qk: Int):
                       (LSET [Int], MatrixD)

end FeatureSelection


type Model_FS = (Predictor | neuralnet.PredictorMV) & Fit


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BestStep` is used to record the best improvement step found so far during
 *  feature selection.  Note, best depends on whether maximizing or minimizing
 *  @param col    the column/variable to ADD/REMOVE for this step
 *  @param qof    the Quality of Fit (QoF) for this step
 *  @param mod    the model including selected features/variables for this step
 *  @param qk     the index for the Quality of Fit (QoF) measure/metric used for comparison
 *  @param bestq  the best QoF for metric qk so far
 */
case class BestStep (col: Int = -1, qof: VectorD = null, mod: Model_FS = null)
                    (using qk: Int)(bestq: Double = Fit.extreme (qk)):

    private val debug = debugf ("BestStep", false)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether this step is better than that step.
     *  @param that_qof  the Qof for that step
     */
    infix def gt (that_qof: Double): Boolean =
        if Fit.maxi.contains (qk) then qof(qk) > that_qof          // maximize, e.g., R^2
        else qof(qk) < that_qof                                    // minimize, e.g., mse, smape
    end gt
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether this step is better than or equal to that step.
     *  @param that_qof  the QoF for that step
     */
    infix def ge (that_qof: Double): Boolean =
        if Fit.maxi.contains (qk) then qof(qk) >= that_qof         // maximize, e.g., R^2
        else qof(qk) <= that_qof                                   // minimize, e.g., mse, smape
    end ge
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the better between this and candidate step.
     *  @param cand  the new candidate
     */
    def better (cand: BestStep): BestStep =
        debug ("better", s"cand = $cand vs. this = $this")
        if qof == null then cand
        else if cand gt qof(qk) then cand else this
    end better

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the better between this and the to be formed candidate step.
     *  @param j      the index of the feature/variable
     *  @param qof_j  the QoF for mod_j
     *  @param mod_j  the model with j
     */
    def better (j: Int, qof_j: VectorD, mod_j: Model_FS): BestStep =
        better (BestStep (j, qof_j, mod_j)(qof_j(qk)))
    end better

end BestStep


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Update the rSq-based and smape QoF results for the l-th iteration of feature
 *  selection.
 *  @see `Predictor`
 *  @param rSq    the matrix contain information about r-Sq-based QoF measures
 *  @param l      the l-th iteration
 *  @param cross  indicator of whether cross-validation are to be included
 *  @param best   the best step so far
 */
def updateQoF (rSq: MatrixD, l: Int, cross: Boolean, best: BestStep): Unit =
    rSq(l) =
        if cross then
            Fit.qofVector (best.qof, best.mod.crossValidate ())       // results for model mod_l, with cross-validation
        else
            Fit.qofVector (best.qof, null)                            // results for model mod_l, no cross-validation
end updateQoF

