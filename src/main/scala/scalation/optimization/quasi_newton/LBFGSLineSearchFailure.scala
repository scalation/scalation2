
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Jan 9 15:24:14 EST 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Data Describing a Failure in the Execution of a Line Search Algorithm
 */

package scalation
package optimization
package quasi_newton

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSLineSearchFailure` case class describes a failure that occurred in the
 *  execution of a line search algorithm in the native implementation of the
 *  L-BFGS algorithm. Every line search algorithm used by the native L-BFGS
 *  implementation should return an instance of this case class upon
 *  encountering an error when searching for the optimal step to take in a given
 *  line.
 *
 *  @param returnCode             `LBFGSReturnCode` describing the error responsible for causing
 *                                a failure in the line search algorithm.  Must be an error code,
 *                                as returning a non-error code causes undefined behavior.
 *  @param bestIncompleteResults  `LBFGSLineSearchIncompleteResults` containing the best results
 *                                obtained from the incomplete execution of the line search algorithm.
 */
case class LBFGSLineSearchFailure (returnCode: LBFGSReturnCode,
                                   bestIncompleteResults: LBFGSLineSearchIncomplete)

