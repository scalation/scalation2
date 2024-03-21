
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Fri Oct 20 14:17:49 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Backtracking Wolfe line search implementation used by the native
 *  implementation of the Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS)
 *  for unconstrained optimization (L-BFGS) algorithm. This Scala implementation
 *  was made based on the C implementation of the same algorithm found in the
 *  link below.
 *
 *  @see github.com/chokkan/liblbfgs
 */

// Package definition.
package scalation
package optimization
package quasi_newton

// Project imports.
import scalation.mathstat.VectorD

// Object.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSBacktrackingWolfe` object implements the backtracking Wolfe line
 *  search algorithm for use in the native implementation of L-BFGS.
 */
object LBFGSBacktrackingWolfe extends LBFGSLineSearch:
    // Public methods.
    override def lineSearch(
       n: Int,
       x: VectorD,
       f: Double,
       g: VectorD,
       s: VectorD,
       stp: Double,
       cd: LBFGSCallbackData,
       params: LBFGSLineSearchParameters,
       orthantWise: Option[OrthantWiseParameters] = None
    ): LBFGSLineSearchReturn =
        var count = 0
        var width = 0.0
        var dg = 0.0
        var dginit = 0.0
        var dgtest = 0.0
        val dec = 0.5
        val inc = 2.1

        var xNew = x
        var gNew = g
        var fNew = f
        var stpNew = stp

        /* Check the input parameters for errors. */
        if stp <= 0.0 then
            return LBFGSLineSearchFailure(LBFGSReturnCode.InvalidParameters, LBFGSLineSearchIncompleteResults(xNew, fNew))
        end if

        /* Compute the initial gradient in the search direction. */
        dginit = g dot s

        /* Make sure that s points to a descent direction. */
        if 0 < dginit then
            return LBFGSLineSearchFailure(LBFGSReturnCode.IncreaseGradient, LBFGSLineSearchIncompleteResults(xNew, fNew))
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
                /* Check the Wolfe condition. */
                dg = gNew dot s
                if dg < params.wolfe * dginit then
                    width = inc
                else
                    /* Exit with the regular Wolfe condition. */
                    return LBFGSLineSearchStep(
                        xNew,
                        gNew,
                        fNew,
                        stpNew,
                        count
                    )
                end if
            end if

            if stpNew < params.minStep then
                /* The step is the minimum value. */
                return LBFGSLineSearchFailure(LBFGSReturnCode.MinimumStep, LBFGSLineSearchIncompleteResults(xNew, fNew))
            end if
            if stpNew > params.maxStep then
                /* The step is the maximum value. */
                return LBFGSLineSearchFailure(LBFGSReturnCode.MaximumStep, LBFGSLineSearchIncompleteResults(xNew, fNew))
            end if
            if params.maxLineSearch <= count then
                /* Maximum number of iteration. */
                return LBFGSLineSearchFailure(LBFGSReturnCode.MaximumLineSearch, LBFGSLineSearchIncompleteResults(xNew, fNew))
            end if

            stpNew *= width
        end while

        LBFGSLineSearchFailure(LBFGSReturnCode.LogicError, LBFGSLineSearchIncompleteResults(xNew, fNew))
end LBFGSBacktrackingWolfe
