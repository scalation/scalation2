
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Aug 22 13:45:22 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @naote   Holds the Results of Running an L-BFGS Algorithm
 */

package scalation
package optimization
package quasi_newton

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSResults` case class is used to group together all the outputs resulting
 *  from a call to a method implementing the L-BFGS optimization algorithm.
 *
 *  @param returnCode            `LBFGSReturnCode` that represents the outcome of L-BFGS optimization.
 *  @param optimizedVariables    `VectorD` that contains the optimized values of the variables that
 *                               were provided as inputs to the L-BFGS optimization.
 *  @param finalFunctionValue    `Option` value that represents the final value obtained for the
 *                               objective function in the L-BFGS optimization.  If the objective
 *                               function was never evaluated due to errors in the arguments
 *                               provided by the user to the L-BFGS method, this field will be set
 *                               to `None` or `Some(0)` depending on how the L-BFGS method was
 *                               implemented.  For new L-BFGS implementations, returning `None` is
 *                                preferred over `Some(0)` when the objective function is never evaluated.
 *  @param lineSearchIncomplete  `Option` value that represents the best incomplete results obtained by
 *                               the line search algorithm before a failure occurred when performing a
 *                               line search during the L-BFGS optimization.  If the L-BFGS optimization
 *                               is successful or produces an error that is not the result of a call
 *                               to a line search algorithm, this value will be set to `None`.  If the
 *                               L-BFGS optimization is stopped due to an error produced by a call to
 *                               a line search algorithm, this value will be set to `Some` with an
 *                               instance of `LBFGSLineSearchIncomplete` that represents the best
 *                               result obtained by the line search algorithm before the error
 *                               occurred.  Some L-BFGS implementations are incapable of returning
 *                               this data and will always return `None`, regardless of the circumstances.
 */
case class LBFGSResults (returnCode: LBFGSReturnCode, optimizedVariables: VectorD,
                         finalFunctionValue: Option [Double],
                         lineSearchIncomplete: Option [LBFGSLineSearchIncomplete])


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSLineSearchIncomplete` case class describes the incomplete
 *  optimization results obtained before an error occurred in the execution of a
 *  line search algorithm in the native implementation of the L-BFGS algorithm.
 *  This information might be useful for the user to determine the effectiveness
 *  of a certain line search algorithm before an error occurred.
 *
 *  @param variableValues  `VectorD` containing the values of the variables after applying the
 *                         best step found for the searched line before the occurrence of an error
 *                         in the line search algorithm.
 *  @param functionValue   Objective function value for the variable values obtained after applying
 *                         the best step found for the searched line before the occurrence of an
 *                         error in the line search algorithm.
 */
case class LBFGSLineSearchIncomplete (variableValues: VectorD, functionValue: Double)

