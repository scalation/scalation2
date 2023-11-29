
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Wed Oct 18 18:27:55 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Backtracking Armijo line search implementation used by the native
 *  implementation of the Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS)
 *  for Bound constrained optimization (L-BFGS-B) algorithm. This Scala
 *  implementation was made based on the C implementation of the same algorithm
 *  found in the link below.
 *
 *  @see github.com/chokkan/liblbfgs
 */

// Package definition.
package scalation
package optimization

// Project imports.
import scalation.mathstat.VectorD

// Object.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSBacktrackingArmijo` object implements the backtracking Armijo line
 *  search algorithm for use in the native implementation of L-BFGS.
 */
object LBFGSBacktrackingArmijo extends LBFGSLineSearch:
    // Public methods.
    override def lineSearch(
       n: Int,
       x: VectorD,
       f: Double,
       g: VectorD,
       s: VectorD,
       stp: Double,
       cd: LBFGSCallbackData,
       params: LBFGSParameters
    ): LBFGSLineSearchReturn =
        var count = 0
        var width = 0.0
        var dginit = 0.0
        var dgtest = 0.0
        val dec = 0.5

        var xNew = x
        var gNew = g
        var fNew = f
        var stpNew = stp

        /* Check the input parameters for errors. */
        if stp <= 0.0 then
            return LBFGSReturnCode.InvalidParameters
        end if
    
        /* Compute the initial gradient in the search direction. */
        dginit = g dot s
    
        /* Make sure that s points to a descent direction. */
        if 0 < dginit then
            return LBFGSReturnCode.IncreaseGradient
        end if
    
        /* The initial value of the objective function. */
        dgtest = params.ftol * dginit
    
        while true do
            xNew = x + (s * stpNew)
    
            /* Evaluate the function and gradient values. */
            val evaluationResults = cd.evaluationLogic.evaluate(cd.instance, xNew, n, stpNew)
            fNew = evaluationResults.objectiveFunctionValue
            gNew = evaluationResults.gradientVector
    
            count += 1
    
            if fNew > f + stpNew * dgtest then
                width = dec
            else
                /* Exit with the Armijo condition. */
                return LBFGSLineSearchStep(
                    xNew,
                    gNew,
                    fNew,
                    stpNew,
                    count
                )
            end if
    
            if stpNew < params.minStep then
                /* The step is the minimum value. */
                return LBFGSReturnCode.MinimumStep
            end if
            if stpNew > params.maxStep then
                /* The step is the maximum value. */
                return LBFGSReturnCode.MaximumStep
            end if
            if params.maxLineSearch <= count then
                /* Maximum number of iteration. */
                return LBFGSReturnCode.MaximumLineSearch
            end if
    
            stpNew *= width
        end while

        LBFGSReturnCode.LogicError
end LBFGSBacktrackingArmijo