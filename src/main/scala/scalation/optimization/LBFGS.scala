
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Aug 22 15:39:53 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Native Scala implementation of the Limited memory
 *  Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained optimization
 *  (L-BFGS-B) algorithm. Originally proposed by Byrd et. al in 1995. See the
 *  first two links for the original paper and authors' software (written in
 *  Fortran) distribution site, respectively. This Scala implementation was made
 *  based on the C implementation of the same algorithm found in the last link.
 *  @see www.ece.northwestern.edu/~nocedal/PSfiles/limited.ps.gz
 *  @see users.iems.northwestern.edu/~nocedal/lbfgsb.html
 *  @see github.com/chokkan/liblbfgs
 */

// Package definition.
package scalation
package optimization

// General imports.
import scala.math.abs

// Project imports.
import scalation.mathstat.VectorD

// Object.
object LBFGS:
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
        checkLBFGSArgumentsForErrors(n, params) match
            case Some(errorReturnCode) => return LBFGSResults(errorReturnCode, x, None)
            case _ =>

        var xNew: VectorD = x
        var lineSearchResults: LBFGSLineSearchReturn = null

        var k = 1
        var end = 0
        var i, j, ls, bound = 0
        var step = 0.0

        /* Constant parameters and their default values. */
        val m = params.m

        try
            var xp, g, gp, d = new VectorD(n)
            var pg, pf, s, y = VectorD.nullv
            val lm = Array.ofDim[LBFGSIterationData](m)

            var ys, yy, xnorm, gnorm, beta, fx, rate = 0.0

            val linesearch: LBFGSLineSearch = determineLineSearchImplementation(params.lineSearch)

            /* Construct a callback data. */
            val cd = LBFGSCallbackData(n, instance, functionLogic)

            /* Allocate working space. */
            if params.orthantWise.nonEmpty then
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

            if params.orthantWise.nonEmpty then
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
            if params.orthantWise.isEmpty then
                d = -g
            else
                d = -pg
            end if

            /*
               Make sure that the initial variables are not a minimizer.
             */
            xnorm = x.norm

            if params.orthantWise.isEmpty then
                gnorm = g.norm
            else
                gnorm = pg.norm
            end if


            if xnorm < 1.0 then xnorm = 1.0

            if gnorm / xnorm <= params.epsilon then
                return LBFGSResults(LBFGSReturnCode.AlreadyMinimized, xNew, Some(fx))
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
                if params.orthantWise.isEmpty then
                    lineSearchResults = linesearch.lineSearch(n, xNew, fx, g, d, step, cd, params)
                else
                    lineSearchResults = linesearch.lineSearch(n, xNew, fx, pg, d, step, cd, params)
                end if

                lineSearchResults match
                    case lineSearchStep: LBFGSLineSearchStep =>
                        xNew = lineSearchStep.x
                        g = lineSearchStep.g
                        fx = lineSearchStep.fx
                        step = lineSearchStep.step
                        ls = lineSearchStep.numberOfIterations
                    case returnCode: LBFGSReturnCode =>
                        /* Return with the value of the previous point. */
                        return LBFGSResults(returnCode, xp, Some(fx))

                if params.orthantWise.isDefined then
                    val orthantWiseParams = params.orthantWise.get
                    pg = orthantWiseParams.pseudoGradient(xNew, g)
                end if

                /* Compute x and g norms. */
                xnorm = xNew.norm

                if params.orthantWise.isEmpty then
                    gnorm = g.norm
                else
                    gnorm = pg.norm
                end if

                /* Report the progress. */
                functionLogic match
                    case o: OptimizationLogic =>
                        val ret = o.progress(cd.instance, xNew, g, fx, xnorm, gnorm, step, cd.n, k, ls)
                        if ret != LBFGSReturnCode.Success then return LBFGSResults(ret, xNew, Some(fx))
                    case _ =>

                /*
                Convergence test.
                The criterion is given by the following formula:
                    |g(x)| / \max(1, |x|) < \epsilon
                */
                if xnorm < 1.0 then xnorm = 1.0

                if gnorm / xnorm <= params.epsilon then
                    return LBFGSResults(LBFGSReturnCode.Success, xNew, Some(fx))
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
                            return LBFGSResults(LBFGSReturnCode.Stop, xNew, Some(fx))
                        end if
                    end if

                    /* Store the current value of the objective function. */
                    pf(k % params.past) = fx
                end if

                if params.maxIterations != 0 && params.maxIterations < k + 1 then
                    return LBFGSResults(LBFGSReturnCode.MaximumIteration, xNew, Some(fx))
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

                lm(end) = LBFGSIterationData(s, y, 1 / ys, 0)

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
                if params.orthantWise.isEmpty then
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
                    it.alpha = it.alpha * it.rho
                    /* q_{i} = q_{i+1} - \alpha_{i} y_{i}. */
                    d += (it.y * (-it.alpha))
                end for

                d *= (ys / yy)

                for i <- 0 until bound do
                    val it = lm(j)
                    /* \beta_{j} = \rho_{j} y^t_{j} \cdot \gamma_{i}. */
                    beta = it.y dot d
                    beta *= it.rho
                    /* \gamma_{i+1} = \gamma_{i} + (\alpha_{j} - \beta_{j}) s_{j}. */
                    d += it.s * (it.alpha - beta)
                    j = (j + 1) % m
                /* if (++j == m) j = 0; */
                end for

                /*
                    Constrain the search direction for orthant-wise updates.
                 */
                if params.orthantWise.nonEmpty then
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
                step = params.defaultStep
            end while

            LBFGSResults(LBFGSReturnCode.UnknownError, xNew, Some(fx))
        catch
            case e: OutOfMemoryError => LBFGSResults(LBFGSReturnCode.OutOfMemory, x, None)

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
        if params.minStep < 0.0 then return Some(LBFGSReturnCode.InvalidMinStep)
        if params.maxStep < params.minStep then return Some(LBFGSReturnCode.InvalidMaxStep)
        if params.ftol < 0.0 then return Some(LBFGSReturnCode.InvalidFTOL)
        if params.lineSearch == LBFGSLineSearchAlgorithm.BacktrackingWolfe ||
            params.lineSearch == LBFGSLineSearchAlgorithm.BacktrackingStrongWolfe then
            if params.wolfe <= params.ftol || 1.0 <= params.wolfe then return Some(LBFGSReturnCode.InvalidWolfe)
        end if
        if params.gtol < 0.0 then return Some(LBFGSReturnCode.InvalidGTOL)
        if params.xtol < 0.0 then return Some(LBFGSReturnCode.InvalidXTOL)
        if params.maxLineSearch <= 0 then return Some(LBFGSReturnCode.InvalidMaxLineSearch)
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determines what [[LBFGSLineSearch]] implementation to use in the L-BFGS
     *  optimization.
     *
     *  @param selection        [[LBFGSLineSearchAlgorithm]] that describes the
     *                          user selection for the line search algorithm to
     *                          be used in the L-BFGS optimization.
     *  @return LBFGSLineSearch [[LBFGSLineSearch]] implementation of the line
     *                          search algorithm selected by the user to be
     *                          used in the L-BFGS optimization.
     */
    private def determineLineSearchImplementation(selection: LBFGSLineSearchAlgorithm): LBFGSLineSearch =
        selection match
            case LBFGSLineSearchAlgorithm.Default => LBFGSMoreThuente
            case LBFGSLineSearchAlgorithm.MoreThuente => LBFGSMoreThuente
            case LBFGSLineSearchAlgorithm.BacktrackingDefault => LBFGSBacktrackingWolfe
            case LBFGSLineSearchAlgorithm.BacktrackingArmijo => LBFGSBacktrackingArmijo
            case LBFGSLineSearchAlgorithm.BacktrackingWolfe => LBFGSBacktrackingWolfe
            case LBFGSLineSearchAlgorithm.BacktrackingStrongWolfe => LBFGSBacktrackingStrongWolfe
            case LBFGSLineSearchAlgorithm.BacktrackingOrthantWise => LBFGSBacktrackingOrthantWise
end LBFGS

// Test functions.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `boothFunctionLBFGSTest` main function uses the Booth Function to test
 *  the `lbfgsMain` method provided by the [[LBFGS]] object. Multiple tests are
 *  performed with different values for the variables.
 *
 *  The Booth Function can be described as follows:
 *
 *  - Input dimension: 2;
 *
 *  - Function domain: -10 &le; x,,i,, &le; 10;
 *
 *  - Function definition: f(x) = (x,,0,, + 2 * x,,1,, - 7)^2^ + (2 * x,,0,, +
 *  x,,1,, - 5)^2^;
 *
 *  - Global minimum: x* = (1, 3); f(x*) = 0;
 *
 *  This test function can be run on the sbt shell with the following command:
 *  {{{
 *  > runMain scalation.optimization.L_BFGS_C.boothFunctionLBFGSTest
 *  }}}
 */
@main def boothFunctionLBFGSTest(): Unit =
    // Function definitions.
    def objectiveFunction(x: VectorD): Double = (x(0) + 2 * x(1) - 7) ~^ 2 + (2 * x(0) + x(1) - 5) ~^ 2
    def gradientFunction(x: VectorD): VectorD = VectorD(10*x(0) + 8*x(1) - 34, 8*x(0) + 10*x(1) - 38)

    // Variable declaration.
    val functionOptimizationLogic = FunctionOptimization(objectiveFunction, gradientFunction)

    // Testing.
    println(LBFGS.lbfgsMain(2, VectorD(1, 3), functionOptimizationLogic))
    println(LBFGS.lbfgsMain(2, VectorD(2, 3.5), functionOptimizationLogic))
    println(LBFGS.lbfgsMain(2, VectorD(0, 0), functionOptimizationLogic))
    println(LBFGS.lbfgsMain(2, VectorD(-4, 7), functionOptimizationLogic))
end boothFunctionLBFGSTest

@main def bealeFunctionLBFGSTest(): Unit =
    // Function definitions.
    def objectiveFunction(x: VectorD): Double = (1.5 - x(0) + x(0)*x(1))~^2 + (2.25 - x(0) + x(0)*(x(1)~^2))~^2 + (2.625 - x(0) + x(0)*(x(1)~^3))~^2
    def gradientFunction(x: VectorD): VectorD = VectorD(2 * (1.5 - x(0) + x(0) * x(1)) * (-1 + x(1)) +
      2 * (2.25 - x(0) + x(0) * (x(1)~^2)) * (-1 + (x(1)~^2)) +
      2 * (2.625 - x(0) + x(0) * (x(1)~^3)) * (-1 + (x(1)~^3)),
        2 * (1.5 - x(0) + x(0) * x(1)) * x(0) +
          2 * (2.25 - x(0) + x(0) * (x(1)~^2)) * (2 * x(0) * x(1)) +
          2 * (2.625 - x(0) + x(0) * (x(1)~^3)) * (3 * x(0) * (x(1)~^2)))

    // Variable declaration.
    val functionOptimizationLogic = FunctionOptimization(objectiveFunction, gradientFunction)

    // Testing.
    //println(LBFGS.lbfgsMain(2, VectorD(-4.5, -4.5), functionOptimizationLogic))
    //println(LBFGS.lbfgsMain(2, VectorD(-2, 2), functionOptimizationLogic))
    //println(LBFGS.lbfgsMain(2, VectorD(0, 1), functionOptimizationLogic))
    println(LBFGS.lbfgsMain(2, VectorD(2, -2), functionOptimizationLogic))
end bealeFunctionLBFGSTest

@main def bohachevsky1FunctionLBFGSTest(): Unit =
    // Function definitions.
    def objectiveFunction(x: VectorD): Double = x(0)~^2 + 2*x(1)~^2 - 0.3*math.cos(3*math.Pi*x(0)) - 0.4*math.cos(4*math.Pi*x(1)) + 0.7
    def gradientFunction(x: VectorD): VectorD = VectorD(2 * x(0) - 0.3 * 3 * math.Pi * math.sin(3 * math.Pi * x(0)),
        4 * x(1) - 0.4 * 4 * math.Pi * math.sin(4 * math.Pi * x(1)))

    // Variable declaration.
    val functionOptimizationLogic = FunctionOptimization(objectiveFunction, gradientFunction)

    // Testing.
    //println(LBFGS.lbfgsMain(2, VectorD(-10, 10), functionOptimizationLogic))
    //println(LBFGS.lbfgsMain(2, VectorD(-5, 5), functionOptimizationLogic))
    //println(LBFGS.lbfgsMain(2, VectorD(5, -5), functionOptimizationLogic))
    println(LBFGS.lbfgsMain(2, VectorD(10, -10), functionOptimizationLogic))
end bohachevsky1FunctionLBFGSTest

@main def bohachevsky2FunctionLBFGSTest(): Unit =
    // Function definitions.
    def objectiveFunction(x: VectorD): Double = x(0)~^2 + 2*x(1)~^2 - 0.3*math.cos(3*math.Pi*x(0))*math.cos(4*math.Pi*x(1)) + 0.3
    def gradientFunction(x: VectorD): VectorD = VectorD(2 * x(0) + 0.3 * 3 * math.Pi * math.sin(3 * math.Pi * x(0)) * math.cos(4 * math.Pi * x(1)),
        4 * x(1) - 0.3 * 4 * math.Pi * math.cos(3 * math.Pi * x(0)) * math.sin(4 * math.Pi * x(1)))

    // Variable declaration.
    val functionOptimizationLogic = FunctionOptimization(objectiveFunction, gradientFunction)

    // Testing.
    //println(LBFGS.lbfgsMain(2, VectorD(-10, 10), functionOptimizationLogic))
    //println(LBFGS.lbfgsMain(2, VectorD(-5, 5), functionOptimizationLogic))
    //println(LBFGS.lbfgsMain(2, VectorD(5, -5), functionOptimizationLogic))
    println(LBFGS.lbfgsMain(2, VectorD(10, -10), functionOptimizationLogic))
end bohachevsky2FunctionLBFGSTest

@main def bohachevsky3FunctionLBFGSTest(): Unit =
    // Function definitions.
    def objectiveFunction(x: VectorD): Double = x(0)~^2 + 2*x(1)~^2 - 0.3*math.cos(3*math.Pi*x(0)+4*math.Pi*x(1)) + 0.3
    def gradientFunction(x: VectorD): VectorD = VectorD(2 * x(0) + 0.3 * 3 * math.Pi * math.sin(3 * math.Pi * x(0) + 4 * math.Pi * x(1)),
        4 * x(1) + 0.3 * 4 * math.Pi * math.sin(3 * math.Pi * x(0) + 4 * math.Pi * x(1)))

    // Variable declaration.
    val functionOptimizationLogic = FunctionOptimization(objectiveFunction, gradientFunction)

    // Testing.
    //println(LBFGS.lbfgsMain(2, VectorD(-10, 10), functionOptimizationLogic))
    //println(LBFGS.lbfgsMain(2, VectorD(-5, 5), functionOptimizationLogic))
    //println(LBFGS.lbfgsMain(2, VectorD(5, -5), functionOptimizationLogic))
    println(LBFGS.lbfgsMain(2, VectorD(10, -10), functionOptimizationLogic))
end bohachevsky3FunctionLBFGSTest

@main def camel3FunctionLBFGSTest(): Unit =
    // Function definitions.
    def objectiveFunction(x: VectorD): Double = 2*x(0)~^2 - 1.05*x(0)~^4 + (1/6.0)*x(0)~^6 + x(0)*x(1) + x(1)~^2
    def gradientFunction(x: VectorD): VectorD = VectorD(4 * x(0) - 4.2 * x(0)~^3 + x(0)~^5 + x(1), x(0) + 2 * x(1))

    // Variable declaration.
    val functionOptimizationLogic = FunctionOptimization(objectiveFunction, gradientFunction)

    // Testing.
    //println(LBFGS.lbfgsMain(2, VectorD(-10, 10), functionOptimizationLogic))
    //println(LBFGS.lbfgsMain(2, VectorD(-5, 5), functionOptimizationLogic))
    //println(LBFGS.lbfgsMain(2, VectorD(5, -5), functionOptimizationLogic))
    println(LBFGS.lbfgsMain(2, VectorD(10, -10), functionOptimizationLogic))
end camel3FunctionLBFGSTest

@main def cubeFunctionLBFGSTest(): Unit =
    // Function definitions.
    def objectiveFunction(x: VectorD): Double = 100*(x(1) - x(0)~^3)~^2 + (1-x(0))~^2
    def gradientFunction(x: VectorD): VectorD = VectorD(-200 * (x(1) - x(0)~^3) * (3 * x(0)~^2) - 2 * (1 - x(0)), 200 * (x(1) - x(0)~^3))

    // Variable declaration.
    val functionOptimizationLogic = FunctionOptimization(objectiveFunction, gradientFunction)

    // Testing.
    //println(LBFGS.lbfgsMain(2, VectorD(-10, 10), functionOptimizationLogic))
    //println(LBFGS.lbfgsMain(2, VectorD(-5, 5), functionOptimizationLogic))
    //println(LBFGS.lbfgsMain(2, VectorD(5, -5), functionOptimizationLogic))
    println(LBFGS.lbfgsMain(2, VectorD(10, -10), functionOptimizationLogic))
end cubeFunctionLBFGSTest

@main def freudensteinRothFunctionLBFGSTest(): Unit =
    // Function definitions.
    def objectiveFunction(x: VectorD): Double = (x(0) - 13 + x(1)*((5-x(1))*x(1) -2))~^2 + (x(0) -29 + x(1)*((x(1) + 1)*x(1) -14))~^2
    def gradientFunction(x: VectorD): VectorD = VectorD(2 * (x(0) - 13 + x(1) * ((5 - x(1)) * x(1) - 2)) + 2 * (x(0) - 29 + x(1) * ((x(1) + 1) * x(1) - 14)),
        2 * x(1) * ((5 - x(1)) * x(1) - 2) + 2 * (x(1) * ((x(1) + 1) * x(1) - 14) + (x(0) - 13 + x(1) * ((5 - x(1)) * x(1) - 2)) * ((5 - x(1)) * x(1) - 2)))

    // Variable declaration.
    val functionOptimizationLogic = FunctionOptimization(objectiveFunction, gradientFunction)

    // Testing.
    //println(LBFGS.lbfgsMain(2, VectorD(-10, 10), functionOptimizationLogic))
    //println(LBFGS.lbfgsMain(2, VectorD(-5, 5), functionOptimizationLogic))
    //println(LBFGS.lbfgsMain(2, VectorD(5, -5), functionOptimizationLogic))
    println(LBFGS.lbfgsMain(2, VectorD(10, -10), functionOptimizationLogic))
end freudensteinRothFunctionLBFGSTest