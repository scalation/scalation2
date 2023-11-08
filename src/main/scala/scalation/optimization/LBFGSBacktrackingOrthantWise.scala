
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Oct 30 17:37:09 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Backtracking Orthant-Wise line search implementation used by the native
 *  implementation of the Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS)
 *  for Bound constrained optimization (L-BFGS-B) algorithm. This Scala
 *  implementation was made based on the C implementation of the same algorithm
 *  found in the link below.
 *
 *  @see github.com/chokkan/liblbfgs
 */

// Package definition.
package scalation.optimization

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
       params: LBFGSParameters
    ): LBFGSLineSearchReturn =
        var count = 0
        var width = 0.5
        var norm = 0.0
        var dgtest = 0.0
        val wp = new VectorD(n)

        /* Input parameters should have an orthantWise. */
        if params.orthantWise.isEmpty then
            return LBFGSReturnCode.InvalidParameters
        end if

        val orthantwiseParams = params.orthantWise.get

        var xNew = x
        var gNew = g
        var fNew = f
        var stpNew = stp

        /* Check the input parameters for errors. */
        if stp <= 0.0 then
            return LBFGSReturnCode.InvalidParameters
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
end LBFGSBacktrackingOrthantWise
