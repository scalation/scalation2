
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

// Package.
package scalation
package optimization
package L_BFGS_C

// General imports.
import scala.math.abs

// Project imports.
import scalation.mathstat.VectorD

// Object.
object Native:
    // Public methods.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Performs the L-BFGS optimization that optimizes variables to minimize a
     *  function value.
     */
    def lbfgsMain(
         n: Int,
         x: VectorD,
         functionLogic: EvaluationLogicNative | OptimizationLogicNative,
         params: LBFGSParameters = LBFGSParameters(),
         instance: Any = None
    ): LBFGSResults =
        checkLBFGSArgumentsForErrors(n, params) match
            case Some(errorReturnCode) => return LBFGSResults(errorReturnCode, x, None)
            case _ =>

        adjustLBFGSArguments(n, params)

        var xNew: VectorD = x

        var k = 1
        var end = 0
        var i, j, ls, bound = 0
        var step = 0.0

        /* Constant parameters and their default values. */
        val m = params.m

        try {
            var xp, g, gp, pg, d = new VectorD(n)
            var pf, s, y = VectorD.nullv
            val lm = Array.ofDim[LBFGSIterationData](m)

            var ys, yy, xnorm, gnorm, beta, fx, rate = 0.0

            val linesearch: LBFGSLineSearch = LBFGSMoreThuente

            /* Construct a callback data. */
            val cd = LBFGSCallbackData(n, instance, functionLogic)

            /* Allocate working space. */
            //        if (param.orthantwise_c != 0.) {
            //            /* Allocate working space for OW-LQN. */
            //            pg = (lbfgsfloatval_t *) vecalloc (n * sizeof(lbfgsfloatval_t));
            //            if (pg == NULL) {
            //                ret = LBFGSERR_OUTOFMEMORY;
            //                goto lbfgs_native_stub_exit;
            //            }
            //        }

            /* Allocate an array for storing previous values of the objective function.
             */
            if 0 < params.past then pf = new VectorD(params.past)

            /* Evaluate the function value and its gradient. */
            val evaluationResults = cd.evaluationLogic.evaluate(cd.instance, x, cd.n, 0)
            fx = evaluationResults.objectiveFunctionValue
            g = evaluationResults.gradientVector

            //        if (0. != params.orthantwise_c) {
            //            /* Compute the L1 norm of the variable and add it to the object value.
            //             */
            //            xnorm = owlqn_x1norm(x, param.orthantwise_start, param.orthantwise_end);
            //            fx += xnorm * param.orthantwise_c;
            //            owlqn_pseudo_gradient(pg, x, g, n, param.orthantwise_c,
            //                param.orthantwise_start, param.orthantwise_end);
            //        }

            /* Store the initial value of the objective function. */
            if pf != VectorD.nullv then pf(0) = fx

            /*
                Compute the direction;
                we assume the initial hessian matrix H_0 as the identity matrix.
             */
            d = -g

//            if params.orthantwiseC == 0 then
//                d = -g
//            else
//                d = -pg
//            end if

            /*
               Make sure that the initial variables are not a minimizer.
             */
            xnorm = x.norm
            gnorm = g.norm

            //        if (param.orthantwise_c == 0.) {
            //            vec2norm(& gnorm, g, n);
            //        } else {
            //            vec2norm(& gnorm, pg, n);
            //        }

            if xnorm < 1.0 then xnorm = 1.0

            if gnorm / xnorm <= params.epsilon then
                return LBFGSResults(LBFGSReturnCode.AlreadyMinimized, xNew, Some(fx))
            //            goto lbfgs_native_stub_exit
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
                linesearch.lineSearch(n, xNew, fx, g, d, step, cd, params) match
                    case lineSearchStep: LBFGSLineSearchStep =>
                        xNew = lineSearchStep.x
                        g = lineSearchStep.g
                        fx = lineSearchStep.fx
                        step = lineSearchStep.step
                        ls = lineSearchStep.numberOfIterations
                    case returnCode: LBFGSReturnCode =>
                        return LBFGSResults(returnCode, xp, Some(fx))

                //            if params.orthantwiseC == 0 then
                //                ls = linesearch.lineSearch(n, x, & fx, g, d, & step, xp, gp, w, & cd, & param);
                //            else
                //                ls = linesearch.lineSearch(n, x, & fx, g, d, & step, xp, pg, w, & cd, & param);
                //                owlqn_pseudo_gradient(pg, x, g, n, param.orthantwise_c,
                //                    param.orthantwise_start,
                //                    param.orthantwise_end);
                //            if (ls < 0) {
                //                /* Revert to the previous point. */
                //                veccpy(x, xp, n);
                //                veccpy(g, gp, n);
                //                ret = ls;
                //                goto lbfgs_native_stub_exit;
                //            }
                //            end if

                /* Compute x and g norms. */
                xnorm = xNew.norm
                gnorm = g.norm

//                if params.orthantwiseC == 0 then
//                    gnorm = g.norm
//                else
//                    gnorm = pg.norm
//                end if
//

                /* Report the progress. */
                functionLogic match
                    case o: OptimizationLogicNative =>
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
                d = -g

//                if params.orthantwiseC == 0 then
//                /* Compute the negative of gradients. */
//                    d = -g
//                else
//                    d = -pg
//                end if

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
                //            if params.orthantwiseC != 0 then
                //                for (i = param.orthantwise_start; i < param.orthantwise_end; ++ i) {
                //                    if (d[i] * pg[i] >= 0) {
                //                        d[i] = 0;
                //                    }
                //                }
                //            end if

                /*
                    Now the search direction d is ready. We try step = 1 first.
                 */
                step = 1.0
            end while

            LBFGSResults(LBFGSReturnCode.UnknownError, xNew, Some(fx))
        }
        catch {
            case e: OutOfMemoryError => LBFGSResults(LBFGSReturnCode.OutOfMemory, x, None)
        }

    // Private methods.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Adjusts the L-BFGS optimization arguments so that the L-BFGS
     *  optimization will work correctly.
     *
     *  @param n        The number of variables.
     *  @param params   [[LBFGSParameters]] class representing the parameters
     *                  chosen to control the L-BFGS optimization.
     */
    private def adjustLBFGSArguments(n: Int, params: LBFGSParameters): Unit =
        if params.orthantwiseEnd < 0 then params.orthantwiseEnd = n

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
        if params.orthantwiseC < 0.0 then return Some(LBFGSReturnCode.InvalidOrthantwise)
        if params.orthantwiseStart < 0 || n < params.orthantwiseStart then
            return Some(LBFGSReturnCode.InvalidOrthantwiseStart)
        end if
        if n < params.orthantwiseEnd then return Some(LBFGSReturnCode.InvalidOrthantwiseEnd)
        if params.orthantwiseC != 0.0 &&
            params.lineSearch != LBFGSLineSearchAlgorithm.BacktrackingDefault &&
            params.lineSearch != LBFGSLineSearchAlgorithm.BacktrackingWolfe
        then
            return Some(LBFGSReturnCode.InvalidLineSearch)
        end if

        None
        
    private def determineLineSearchAlgorithm(): LBFGSLineSearch =
        // Placeholder.
        LBFGSMoreThuente
end Native

// Test functions.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `boothFunctionNativeTest` main function uses the Booth Function to test
 *  the `lbfgsMain` method provided by the [[Native]] object. Multiple tests are
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
 *  > runMain scalation.optimization.L_BFGS_C.boothFunctionNativeTest
 *  }}}
 */
@main def boothFunctionNativeTest(): Unit =
    // Function definitions.
    def objectiveFunction(x: VectorD): Double = (x(0) + 2 * x(1) - 7) ~^ 2 + (2 * x(0) + x(1) - 5) ~^ 2
    def gradientFunction(x: VectorD): VectorD = VectorD(10*x(0) + 8*x(1) - 34, 8*x(0) + 10*x(1) - 38)

    // Variable declaration.
    val functionOptimizationLogic = FunctionOptimizationNative(objectiveFunction, gradientFunction)

    // Testing.
    println(Native.lbfgsMain(2, VectorD(1, 3), functionOptimizationLogic))
    println(Native.lbfgsMain(2, VectorD(2, 3.5), functionOptimizationLogic))
    println(Native.lbfgsMain(2, VectorD(0, 0), functionOptimizationLogic))
    println(Native.lbfgsMain(2, VectorD(-4, 7), functionOptimizationLogic))
end boothFunctionNativeTest
