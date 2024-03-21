
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Jan 9 15:24:14 EST 2024
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Data that describes a failure in the execution of a line search algorithm
 *  used by the native implementation of the Limited memory
 *  Broyden–Fletcher–Goldfarb–Shanno (BFGS) for unconstrained optimization
 *  (L-BFGS) algorithm.
 */

// Package definition.
package scalation
package optimization
package quasi_newton

// Case class.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSLineSearchFailure` class describes a failure that occurred in the
 *  execution of a line search algorithm in the native implementation of the
 *  L-BFGS algorithm. Every line search algorithm used by the native L-BFGS
 *  implementation should return an instance of this case class upon
 *  encountering an error when searching for the optimal step to take in a given
 *  line.
 *
 *  @param returnCode               [[LBFGSReturnCode]] describing the error
 *                                  responsible for causing a failure in the
 *                                  line search algorithm. Must be an error
 *                                  code, as returning a non-error code causes
 *                                  undefined behavior.
 *  @param bestIncompleteResults    [[LBFGSLineSearchIncompleteResults]]
 *                                  containing the best results obtained from
 *                                  the incomplete execution of the line search
 *                                  algorithm.
 */
case class LBFGSLineSearchFailure(
    returnCode: LBFGSReturnCode,
    bestIncompleteResults: LBFGSLineSearchIncompleteResults
)
