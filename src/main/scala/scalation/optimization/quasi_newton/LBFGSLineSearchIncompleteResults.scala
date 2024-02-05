
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Fri Jan 19 18:02:16 EST 2024
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Incomplete optimization results obtained before an error occurred in a call
 *  to a line search algorithm used by the native implementation of the Limited
 *  memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for unconstrained
 *  optimization (L-BFGS) algorithm.
 */

// Package definition.
package scalation
package optimization
package quasi_newton

// Project imports.
import scalation.mathstat.VectorD

// Case class.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSLineSearchIncompleteResults` class describes the incomplete
 *  optimization results obtained before an error occurred in the execution of a
 *  line search algorithm in the native implementation of the L-BFGS algorithm.
 *  This information might be useful for the user to determine the effectiveness
 *  of a certain line search algorithm before an error occurred.
 *
 *  @param variableValues   [[VectorD]] containing the values of the variables
 *                          after applying the best step found for the searched
 *                          line before the occurrence of an error in the line
 *                          search algorithm.
 *  @param functionValue    Objective function value for the variable values
 *                          obtained after applying the best step found for the
 *                          searched line before the occurrence of an error in
 *                          the line search algorithm.
 */
case class LBFGSLineSearchIncompleteResults(
    variableValues: VectorD,
    functionValue: Double
)
