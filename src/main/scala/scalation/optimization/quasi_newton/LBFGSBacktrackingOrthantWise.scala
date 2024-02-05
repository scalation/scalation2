
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Oct 30 17:37:09 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Backtracking Orthant-Wise line search implementation used by the native
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
/** The `LBFGSBacktrackingOrthantWise` object implements the backtracking
 *  Orthant-Wise line search algorithm for use in the native implementation of
 *  L-BFGS.
 */
object LBFGSBacktrackingOrthantWise extends LBFGSLineSearch:
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
       orthantWise: Option[OrthantWiseParameters]
    ): LBFGSLineSearchReturn =
        var count = 0
        val width = 0.5
        var norm = 0.0
        var dgtest = 0.0
        val wp = new VectorD(n)

        var xNew = x
        var gNew = g
        var fNew = f
        var stpNew = stp

        /* Input parameters should have an orthantWise. */
        if orthantWise.isEmpty then
            return LBFGSLineSearchFailure(LBFGSReturnCode.InvalidParameters, LBFGSLineSearchIncompleteResults(xNew, fNew))
        end if

        val orthantwiseParams = orthantWise.get
        
        /* Check the input parameters for errors. */
        if stp <= 0.0 then
            return LBFGSLineSearchFailure(LBFGSReturnCode.InvalidParameters, LBFGSLineSearchIncompleteResults(xNew, fNew))
        end if

        /* Choose the orthant for the new point. */
        for i <- 0 until n do
            wp(i) = if x(i) == 0.0 then -g(i) else x(i)

        while true do
            /* Update the current point. */
            xNew = x + (s * stpNew)

            /* The current point is projected onto the orthant. */
            xNew = orthantwiseParams.project(xNew, wp)

            /* Evaluate the function and gradient values. */
            val evaluationResults = cd.evaluationLogic.evaluate(cd.instance, xNew, n, stpNew)
            fNew = evaluationResults.objectiveFunctionValue
            gNew = evaluationResults.gradientVector

            /* Compute the L1 norm of the variables and add it to the object value. */
            norm = orthantwiseParams.x1Norm(xNew)
            fNew += norm * orthantwiseParams.c

            count += 1

            dgtest = 0.0
            for i <- 0 until n do
                dgtest += (xNew(i) - x(i)) * g(i)
            end for

            if fNew <= f + params.ftol * dgtest then
                /* The sufficient decrease condition. */
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
end LBFGSBacktrackingOrthantWise
