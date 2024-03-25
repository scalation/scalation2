
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira, Shivam Anant Rathi
 *  @version 2.0
 *  @note    Tue Aug 22 15:39:53 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Native Scala implementation of the Limited memory
 *  Broyden–Fletcher–Goldfarb–Shanno (BFGS) for unconstrained optimization
 *  (L-BFGS) algorithm. This Scala implementation was made based on the C
 *  implementation of the same algorithm found in the following link.
 *
 *  @see github.com/chokkan/liblbfgs
 */

// Package definition.
package scalation
package optimization
package quasi_newton

// General imports.
import scala.math.abs

// Project imports.
import scalation.mathstat.VectorD
import scalation.optimization.functions.*

// Object.
object LBFGS extends PathMonitor:
    // Public methods.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Performs the L-BFGS optimization that optimizes variables to minimize a
     *  function value.
     */
    def lbfgsMain(
        n: Int,
        x: VectorD,
        functionLogic: EvaluationLogic | OptimizationLogic,
        params: LBFGSParameters = LBFGSParameters(),
        instance: Any = None
    ): LBFGSResults =
        clearPath()

        checkLBFGSArgumentsForErrors(n, params) match
            case Some(errorReturnCode) => return LBFGSResults(errorReturnCode, x, None, None)
            case _ =>

        var xNew: VectorD = x
        add2Path(x)

        var lineSearchResults: LBFGSLineSearchReturn = null
        val useOrthantWiseLogic = params.orthantWise.nonEmpty

        var k = 1
        var end = 0
        var j, ls, bound = 0
        var step = 0.0

        /* Constant parameters and their default values. */
        val m = params.m

        try
            var xp, g, gp, d = new VectorD(n)
            var pg, pf, s, y = VectorD.nullv
            val lm = Array.ofDim[LBFGSIterationData](m)

            var ys, yy, xnorm, gnorm, beta, fx, rate = 0.0

            val lineSearchImplementation = LBFGSLineSearch.getImplementation(params.lineSearch)

            /* Construct a callback data. */
            val cd = LBFGSCallbackData(n, instance, functionLogic)

            /* Allocate working space. */
            if useOrthantWiseLogic then
            /* Allocate working space for OW-LQN. */
                pg = new VectorD(n)
            end if

            /* Allocate an array for storing previous values of the objective function.
             */
            if 0 < params.past then pf = new VectorD(params.past)

            /* Evaluate the function value and its gradient. */
            val evaluationResults = cd.evaluationLogic.evaluate(cd.instance, x, cd.n, 0)
            fx = evaluationResults.objectiveFunctionValue
            g = evaluationResults.gradientVector

            if useOrthantWiseLogic then
                /* Compute the L1 norm of the variable and add it to the object value.
                 */
                val orthantWiseParams = params.orthantWise.get
                xnorm = orthantWiseParams.x1Norm(x)
                fx += xnorm * orthantWiseParams.c
                pg = orthantWiseParams.pseudoGradient(x, g)
            end if


            /* Store the initial value of the objective function. */
            if pf != VectorD.nullv then pf(0) = fx

            /*
                Compute the direction;
                we assume the initial hessian matrix H_0 as the identity matrix.
             */
            if !useOrthantWiseLogic then
                d = -g
            else
                d = -pg
            end if

            /*
               Make sure that the initial variables are not a minimizer.
             */
            xnorm = x.norm

            if !useOrthantWiseLogic then
                gnorm = g.norm
            else
                gnorm = pg.norm
            end if


            if xnorm < 1.0 then xnorm = 1.0

            if gnorm / xnorm <= params.epsilon then
                return LBFGSResults(LBFGSReturnCode.AlreadyMinimized, xNew, Some(fx), None)
            end if

            /* Compute the initial step:
                step = 1.0 / sqrt(vecdot(d, d, n))
             */
            step = 1.0 / d.norm

            while true do
                /* Store the current position and gradient vectors. */
                xp = xNew
                gp = g

                /* Search for an optimal step. */
                if !useOrthantWiseLogic then
                    lineSearchResults = lineSearchImplementation.lineSearch(n, xNew, fx, g, d, step, cd, params.lineSearchParams, params.orthantWise)
                else
                    lineSearchResults = lineSearchImplementation.lineSearch(n, xNew, fx, pg, d, step, cd, params.lineSearchParams, params.orthantWise)
                end if

                lineSearchResults match
                    case lineSearchStep: LBFGSLineSearchStep =>
                        xNew = lineSearchStep.x
                        g = lineSearchStep.g
                        fx = lineSearchStep.fx
                        step = lineSearchStep.step
                        ls = lineSearchStep.numberOfIterations
                        add2Path(xNew)
                    case lineSearchFailure: LBFGSLineSearchFailure =>
                        val failureReturnCode =
                            if lineSearchFailure.returnCode.isErrorCode then lineSearchFailure.returnCode
                            else LBFGSReturnCode.UnknownError
                        /* Return with the value of the previous point. */
                        return LBFGSResults(failureReturnCode, xp, Some(fx), Some(lineSearchFailure.bestIncompleteResults))

                if useOrthantWiseLogic then
                    val orthantWiseParams = params.orthantWise.get
                    pg = orthantWiseParams.pseudoGradient(xNew, g)
                end if

                /* Compute x and g norms. */
                xnorm = xNew.norm

                if !useOrthantWiseLogic then
                    gnorm = g.norm
                else
                    gnorm = pg.norm
                end if

                /* Report the progress. */
                functionLogic match
                    case o: OptimizationLogic =>
                        val ret = o.progress(cd.instance, xNew, g, fx, xnorm, gnorm, step, cd.n, k, ls)
                        if ret != LBFGSReturnCode.Success then return LBFGSResults(ret, xNew, Some(fx), None)
                    case _ =>

                /*
                Convergence test.
                The criterion is given by the following formula:
                    |g(x)| / \max(1, |x|) < \epsilon
                */
                if xnorm < 1.0 then xnorm = 1.0

                if gnorm / xnorm <= params.epsilon then
                    return LBFGSResults(LBFGSReturnCode.Success, xNew, Some(fx), None)
                end if

                /*
                Test for stopping criterion.
                The criterion is given by the following formula:
                    |(f(past_x) - f(x))| / f(x) < \delta
                */
                if pf != VectorD.nullv then
                    /* We don't test the stopping criterion while k < past. */
                    if params.past <= k then
                        /* Compute the relative improvement from the past. */
                        rate = (pf(k % params.past) - fx) / fx

                        /* The stopping criterion. */
                        if abs(rate) < params.delta then
                            return LBFGSResults(LBFGSReturnCode.Stop, xNew, Some(fx), None)
                        end if
                    end if

                    /* Store the current value of the objective function. */
                    pf(k % params.past) = fx
                end if

                if params.maxIterations != 0 && params.maxIterations < k + 1 then
                    return LBFGSResults(LBFGSReturnCode.MaximumIteration, xNew, Some(fx), None)
                end if

                /*
                Update vectors s and y:
                    s_{k+1} = x_{k+1} - x_{k} = \step * d_{k}.
                    y_{k+1} = g_{k+1} - g_{k}.
                */
                s = xNew - xp
                y = g - gp

                /*
                Compute scalars ys and yy:
                    ys = y^t \cdot s = 1 / \rho.
                    yy = y^t \cdot y.
                Notice that yy is used for scaling the hessian matrix H_0 (Cholesky
                factor).
                */
                ys = y dot s
                yy = y dot y

                lm(end) = LBFGSIterationData(s, y, ys, 0)

                /*
                Recursive formula to compute dir = -(H \cdot g).
                This is described in page 779 of:
                Jorge Nocedal.
                Updating Quasi-Newton Matrices with Limited Storage.
                Mathematics of Computation, Vol. 35, No. 151,
                pp. 773--782, 1980.
                */
                bound = if m <= k then m else k
                k += 1
                end = (end + 1) % m

                /* Compute the steepest direction. */
                if !useOrthantWiseLogic then
                /* Compute the negative of gradients. */
                    d = -g
                else
                    d = -pg
                end if

                j = end
                for i <- 0 until bound do
                    j = (j + m - 1) % m
                    /* if (--j == -1) j = m-1; */
                    val it = lm(j)
                    /* \alpha_{j} = \rho_{j} s^{t}_{j} \cdot q_{k+1}. */
                    it.alpha = it.s dot d
                    it.alpha = it.alpha / it.ys
                    /* q_{i} = q_{i+1} - \alpha_{i} y_{i}. */
                    d += (it.y * (-it.alpha))
                end for

                d *= (ys / yy)

                for i <- 0 until bound do
                    val it = lm(j)
                    /* \beta_{j} = \rho_{j} y^t_{j} \cdot \gamma_{i}. */
                    beta = it.y dot d
                    beta /= it.ys
                    /* \gamma_{i+1} = \gamma_{i} + (\alpha_{j} - \beta_{j}) s_{j}. */
                    d += it.s * (it.alpha - beta)
                    j = (j + 1) % m
                /* if (++j == m) j = 0; */
                end for

                /*
                    Constrain the search direction for orthant-wise updates.
                 */
                if useOrthantWiseLogic then
                    val orthantWiseParams = params.orthantWise.get
                    for i <- orthantWiseParams.start until orthantWiseParams.end.getOrElse(d.length) do
                        if d(i) * pg(i) >= 0 then
                            d(i) = 0
                        end if
                    end for
                end if

                /*
                    Now the search direction d is ready. We try the default step first.
                 */
                // Previously, the default step was hardcoded to 1.
                step = params.lineSearchParams.defaultStep
            end while

            LBFGSResults(LBFGSReturnCode.UnknownError, xNew, Some(fx), None)
        catch
            case e: OutOfMemoryError => LBFGSResults(LBFGSReturnCode.OutOfMemory, x, None, None)

    // Private methods.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Checks if the L-BFGS optimization arguments have an error and returns an
     *  [[Option]] with the [[LBFGSReturnCode]] that represents the first error
     *  found.
     *
     *  @param n                        The number of variables.
     *  @param params                   [[LBFGSParameters]] class representing
     *                                  the parameters chosen to control the
     *                                  L-BFGS optimization.
     *  @return Option[LBFGSReturnCode] [[Option]] value with a
     *                                  [[LBFGSReturnCode]] return code that
     *                                  represents the first error found in the
     *                                  `params` argument. If there are no
     *                                  errors in the `params` argument,
     *                                  [[None]] is returned.
     */
    private def checkLBFGSArgumentsForErrors(n: Int, params: LBFGSParameters): Option[LBFGSReturnCode] =
        if n <= 0 then return Some(LBFGSReturnCode.InvalidN)
        if params.epsilon < 0.0 then return Some(LBFGSReturnCode.InvalidEpsilon)
        if params.past < 0 then return Some(LBFGSReturnCode.InvalidTestPeriod)
        if params.delta < 0.0 then return Some(LBFGSReturnCode.InvalidDelta)
        if params.lineSearchParams.minStep < 0.0 then return Some(LBFGSReturnCode.InvalidMinStep)
        if params.lineSearchParams.maxStep < params.lineSearchParams.minStep then return Some(LBFGSReturnCode.InvalidMaxStep)
        if params.lineSearchParams.ftol < 0.0 then return Some(LBFGSReturnCode.InvalidFTOL)
        if params.lineSearch == LBFGSLineSearchAlgorithm.BacktrackingWolfe ||
            params.lineSearch == LBFGSLineSearchAlgorithm.BacktrackingStrongWolfe then
            if params.lineSearchParams.wolfe <= params.lineSearchParams.ftol || 1.0 <= params.lineSearchParams.wolfe then return Some(LBFGSReturnCode.InvalidWolfe)
        end if
        if params.lineSearchParams.gtol < 0.0 then return Some(LBFGSReturnCode.InvalidGTOL)
        if params.lineSearchParams.xtol < 0.0 then return Some(LBFGSReturnCode.InvalidXTOL)
        if params.lineSearchParams.maxLineSearch <= 0 then return Some(LBFGSReturnCode.InvalidMaxLineSearch)
        if params.orthantWise.nonEmpty then
            val orthantWiseParams = params.orthantWise.get
            if orthantWiseParams.c <= 0.0 then return Some(LBFGSReturnCode.InvalidOrthantwise)
            if orthantWiseParams.start < 0 || n <= orthantWiseParams.start then
                return Some(LBFGSReturnCode.InvalidOrthantwiseStart)
            end if
            if orthantWiseParams.end.isDefined then
                val orthantWiseParamsEnd = orthantWiseParams.end.get
                if orthantWiseParamsEnd <= orthantWiseParams.start || n < orthantWiseParamsEnd then
                    return Some(LBFGSReturnCode.InvalidOrthantwiseEnd)
                end if
            end if
            if params.lineSearch != LBFGSLineSearchAlgorithm.BacktrackingOrthantWise then
                return Some(LBFGSReturnCode.InvalidLineSearch)
            end if
        else if params.lineSearch == LBFGSLineSearchAlgorithm.BacktrackingOrthantWise then
            return Some(LBFGSReturnCode.InvalidLineSearch)
        end if

        None
end LBFGS

// Test functions.
@main def bealeFunctionLBFGSTest(): Unit =
    // Variable declaration.
    // val functionDomainLowerBound = VectorD(-10, -10)
    // val functionDomainUpperBound = VectorD(10, 10)
    val functionOptimizationLogic = FunctionOptimization(BealeFunction)

    // Testing.
    println("-----------------Test 1--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(3, 0), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))
    println("-----------------Test 2--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(2, -1), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))
    println("-----------------Test 3--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(0, 1), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))
    println("-----------------Test 4--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(-2, -1), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))

    // val plot = new PlotC(BealeFunction.objectiveFunction, functionDomainLowerBound, functionDomainUpperBound, LBFGS.getPath, BealeFunction.functionMinimum)
    // writeImage("./plots/LBFGS/LBFGS_bealeFunction_plot.png", plot)
end bealeFunctionLBFGSTest

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `boothFunctionLBFGSTest` main function uses the Booth Function to test
 * the `lbfgsMain` method provided by the [[LBFGS]] object. Multiple tests are
 * performed with different values for the variables.
 *
 * The Booth Function can be described as follows:
 *
 *  - Input dimension: 2;
 *
 *  - Function domain: -10 &le; x,,i,, &le; 10;
 *
 *  - Function definition: f(x) = (x,,0,, + 2 * x,,1,, - 7)^2^ + (2 * x,,0,, +
 *    x,,1,, - 5)^2^;
 *
 *  - Global minimum: x* = (1, 3); f(x*) = 0;
 *
 * This test function can be run on the sbt shell with the following command:
 * {{{
 *  > runMain scalation.optimization.L_BFGS_C.boothFunctionLBFGSTest
 *   }}}
 */
@main def boothFunctionLBFGSTest(): Unit =
    // Variable declaration.
//    val functionDomainLowerBound = VectorD(-10, -10)
//    val functionDomainUpperBound = VectorD(10, 10)
    val functionOptimizationLogic = FunctionOptimization(BoothFunction)

    // Testing.
    println("-----------------Test 1--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(2, 1), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))
    println("-----------------Test 2--------------------------------------")
//    println(LBFGS.lbfgsMain(2, VectorD(-1, 5), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))
//    println("-----------------Test 3--------------------------------------")
//    println(LBFGS.lbfgsMain(2, VectorD(-5, 5), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))
//    println("-----------------Test 4--------------------------------------")
//    println(LBFGS.lbfgsMain(2, VectorD(-5, -2), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))

//    val plot = new PlotC(BoothFunction.objectiveFunction, functionDomainLowerBound, functionDomainUpperBound, LBFGS.getPath, BoothFunction.functionMinimum)
//    writeImage("./plots/LBFGS/LBFGS_boothFunction_plot.png", plot)
end boothFunctionLBFGSTest

@main def camel3FunctionLBFGSTest(): Unit =
    // Variable declaration.
//    val functionDomainLowerBound = VectorD(-10, -10)
//    val functionDomainUpperBound = VectorD(10, 10)
    val functionOptimizationLogic = FunctionOptimization(Camel3Function)

    // Testing.
    println("-----------------Test 1--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(2, 2), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))
    println("-----------------Test 2--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(-3, -3), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))
    println("-----------------Test 3--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(4, -4), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))
    println("-----------------Test 4--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(-5, 5), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))

//    val plot = new PlotC(Camel3Function.objectiveFunction, functionDomainLowerBound, functionDomainUpperBound, LBFGS.getPath, Camel3Function.functionMinimum)
//    writeImage("./plots/LBFGS/LBFGS_camel3Function_plot.png", plot)
end camel3FunctionLBFGSTest

@main def cubeFunctionLBFGSTest(): Unit =
    // Variable declaration.
//    val functionDomainLowerBound = VectorD(-10, -10)
//    val functionDomainUpperBound = VectorD(10, 10)
    val functionOptimizationLogic = FunctionOptimization(CubeFunction)

    // Testing.
    println("-----------------Test 1--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(0, 0), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))
    println("-----------------Test 2--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(3, 3), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))
    println("-----------------Test 3--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(4, 5), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))
    println("-----------------Test 4--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(5, -5), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))

//    val plot = new PlotC(CubeFunction.objectiveFunction, functionDomainLowerBound, functionDomainUpperBound, LBFGS.getPath, CubeFunction.functionMinimum)
//    writeImage("./plots/LBFGS/LBFGS_cubeFunction_plot.png", plot)
end cubeFunctionLBFGSTest

@main def freudensteinRothFunctionLBFGSTest(): Unit =
    // Variable declaration.
//    val functionDomainLowerBound = VectorD(-10, -10)
//    val functionDomainUpperBound = VectorD(10, 10)
    val functionOptimizationLogic = FunctionOptimization(FreudensteinRothFunction)

    // Testing.
    println("-----------------Test 1--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(5, 7), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))
    println("-----------------Test 2--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(3, 2), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))
    println("-----------------Test 3--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(1, 0), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))
    println("-----------------Test 4--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(-1, -2), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))

//    val plot = new PlotC(FreudensteinRothFunction.objectiveFunction, functionDomainLowerBound, functionDomainUpperBound, LBFGS.getPath, FreudensteinRothFunction.functionMinimum)
//    writeImage("./plots/LBFGS/LBFGS_freudensteinRothFunction_plot.png", plot)
end freudensteinRothFunctionLBFGSTest

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `mccormickFunctionLBFGSTest` main function uses the McCormick Function to test
 *  the `lbfgsMain` method provided by the [[LBFGS]] object. Multiple tests are
 *  performed with different values for the variables.
 *
 *  This test function can be run on the sbt shell with the following command:
 *  {{{
 *  > runMain scalation.optimization.L_BFGS_C.mccormickFunctionLBFGSTest
 *  }}}
 */
@main def mccormickFunctionLBFGSTest(): Unit =
    // Variable declaration.
//    val functionDomainLowerBound = VectorD(-4, -4)
//    val functionDomainUpperBound = VectorD(4, 4)
    val functionOptimizationLogic = FunctionOptimization(McCormickFunction)

    // Testing.
    println("-----------------Test 1--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(-1, -1), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))
    println("-----------------Test 2--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(0, 0), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))
    println("-----------------Test 3--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(0, 2), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))
    println("-----------------Test 4--------------------------------------")
    println(LBFGS.lbfgsMain(2, VectorD(3, 3), functionOptimizationLogic, params=LBFGSParameters(lineSearchParams = LBFGSLineSearchParameters(defaultStep=4))))

//    val plot = new PlotC(McCormickFunction.objectiveFunction, functionDomainLowerBound, functionDomainUpperBound, LBFGS.getPath, McCormickFunction.functionMinimum)
//    writeImage("./plots/LBFGS/LBFGS_mccormickFunction_plot.png", plot)
end mccormickFunctionLBFGSTest