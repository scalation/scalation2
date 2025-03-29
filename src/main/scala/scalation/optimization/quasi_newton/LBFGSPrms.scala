
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Aug 21 13:53:24 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Parameters that Control the Optimization Process for the L-BFGS Algorithm
 */

package scalation
package optimization
package quasi_newton

//import java.lang.foreign.{MemoryLayout, MemorySegment, StructLayout}
//import java.lang.foreign.ValueLayout.{JAVA_DOUBLE, JAVA_INT}

//import scala.annotation.static
import scala.math.abs

import scalation.mathstat.VectorD

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSPrms` class is used to group together all parameters that control the
 *  L-BFGS optimization process.
 *
 *  @param m               number of past iterations stored in memory to approximate the inverse Hessian
 *                         matrix of the current iteration.  Values less than 3 are not recommended.
 *                         Large values will result in excessive computing time.  The default value is 6.
 *  @param epsilon         epsilon for convergence test.  Determines the accuracy with which the
 *                         solution is to be found.  A minimization terminates when:
 *                         ||g|| < `epsilon` * max(1, ||x||).  In the formula, ||.|| denotes the Euclidean
 *                         L2-norm.  The default value is 1e-5.
 *  @param past            distance for delta-based convergence test.  Determines how many iterations
 *                         to compute the rate of decrease of the objective function.  A value of zero
 *                         implies the delta-based convergence test will not be performed.  The default
 *                         value is 0.
 *  @param delta           delta for convergence test. Determines the minimum rate of decrease of the
 *                         objective function. The optimization stops iterations when the following
 *                         condition is met:  (f'-f)/f < delta.  In the formula, f' is the objective
 *                         value of `past` iterations ago, and f is the objective value of the current
 *                         iteration.  The default value is 1e-5.
 *  @param maxIterations   the maximum number of iterations. The `lbfgsMain` and `lbfgsMainCWrapper`
 *                         methods in `Wrapper` terminate an optimization process with the
 *                         `LBFGSReturnCode.MaximumIteration` return code when the iteration count
 *                         exceeds this parameter.  Setting this parameter to zero continues the
 *                         optimization process until a convergence or error.  The default value is 0.
 *  @param lineSearch      `LBFGSLineSearchAlg` to specify what line search algorithm should be used.
 *                         The default value is `LBFGSLineSearchAlg.Default`.
 *  @param lineSearchPrms  `BFGSLineSearchPrms` to specify the parameters needed during the
 *                         execution of the line search algorithm routine.
 *  @param orthantWise     `Option` of type `OrthantWisePrms` that specifies whether the
 *                         Orthant-Wise Limited-memory Quasi-Newton (OWL-QN) optimization method should
 *                         be used when calculating the value to be optimized. If this parameter is set
 *                         to `Some`, then the `lineSearch` parameter should always be set to
 *                         `LBFGSLineSearchAlg.BacktrackingOrthantWise` or the optimization will
 *                         terminate with the return code `LBFGSReturnCode.InvalidLineSearch`. The same
 *                         will occur when the `lineSearch` parameter is set to
 *                         `LBFGSLineSearchAlg.BacktrackingOrthantWise` and this parameter is set to `None`.
 */
case class LBFGSPrms (m: Int = 6, epsilon: Double = 1e-5, past: Int = 0, delta: Double = 1e-5,
                      maxIterations: Int = 0, lineSearch: LBFGSLineSearchAlg = LBFGSLineSearchAlg.Default,
                      lineSearchPrms: LBFGSLineSearchPrms = LBFGSLineSearchPrms (),
                      orthantWise: Option [OrthantWisePrms] = None)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSPrms` companion object ...
 *
case object LBFGSPrms:

    // This variable is only needed for the LBFGS_FFM object implementation. If
    // you are NOT working with LBFGS_FFM, feel free to disregard it.
    @static
    val memoryLayout: StructLayout = MemoryLayout.structLayout (
        JAVA_INT.withName ("m"),
        MemoryLayout.paddingLayout (32),
        JAVA_DOUBLE.withName ("epsilon"),
        JAVA_INT.withName ("past"),
        MemoryLayout.paddingLayout (32),
        JAVA_DOUBLE.withName ("delta"),
        JAVA_INT.withName ("max_iterations"),
        JAVA_INT.withName ("linesearch"),
        JAVA_INT.withName ("max_linesearch"),
        MemoryLayout.paddingLayout (32),
        JAVA_DOUBLE.withName ("default_step"),
        JAVA_DOUBLE.withName ("min_step"),
        JAVA_DOUBLE.withName ("max_step"),
        JAVA_DOUBLE.withName ("ftol"),
        JAVA_DOUBLE.withName ("wolfe"),
        JAVA_DOUBLE.withName ("gtol"),
        JAVA_DOUBLE.withName ("xtol"),
        JAVA_DOUBLE.withName ("orthantwise_c"),
        JAVA_INT.withName ("orthantwise_start"),
        JAVA_INT.withName ("orthantwise_end")).withName ("lbfgs_parameter_t")

end LBFGSPrms
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSLineSearchPrms` case class is used to group together all
 *  parameters that control the line search routine used by the L-BFGS
 *  optimization process.
 *
 *  @param maxLineSearch    Maximum number of trials for the line search.
 *                          Controls the number of function and gradient
 *                          evaluations per iteration for the line search
 *                          routine. The default value is 40.
 *  @param defaultStep      The default step selected as the initial for the
 *                          line search routine. The default value is 1.0.
 *  @param minStep          Minimum step of the line search routine. Does not
 *                          need to be modified unless the exponents are too
 *                          large for the machine being used, or unless the
 *                          problem is extremely badly scaled (in which case the
 *                          exponents should be increased). The default value is
 *                          1e-20.
 *  @param maxStep          Maximum step of the line search routine. Does not
 *                          need to be modified unless the exponents are too
 *                          large for the machine being used, or unless the
 *                          problem is extremely badly scaled (in which case the
 *                          exponents should be increased). The default value is
 *                          1e20.
 *  @param ftol             Controls the accuracy of the line search routine.
 *                          Should be greater than zero and smaller than 0.5.
 *                          The default value is 1e-4.
 *  @param wolfe            A coefficient for the Wolfe condition. Only used
 *                          when a backtracking line-search algorithm that
 *                          relies on the Wolfe condition is chosen for the
 *                          `LBFGSLineSearchAlg` `lineSearch` param
 *                          (e.g: `LBFGSLineSearchAlg.BacktrackingWolfe`
 *                          or
 *                          `LBFGSLineSearchAlg.BacktrackingStrongWolfe`
 *                          ). Should be greater than the `ftol` parameter and
 *                          smaller than 1.0. The default value is 0.9.
 *  @param gtol             Controls the accuracy of the line search routine. If
 *                          the function and gradient evaluations are
 *                          inexpensive with respect to the cost of the
 *                          iteration (which is sometimes the case when solving
 *                          very large problems), it may be advantageous to set
 *                          this parameter to a small value (e.g: 0.1). This
 *                          parameter should be greater than the `ftol`
 *                          parameter (default value of 1e-4) and smaller than
 *                          1.0. The default value is 0.9.
 *  @param xtol             The machine precision for floating-point values.
 *                          Must be a positive value set by a client program to
 *                          estimate the machine precision. The L-BFGS
 *                          optimization will terminate with the return code
 *                          `LBFGSReturnCode.RoundingError` if the relative
 *                          width of the interval of uncertainty is less than
 *                          this parameter. The default value is 1.0e-16.
 */
case class LBFGSLineSearchPrms (maxLineSearch: Int = 40, defaultStep: Double = 1.0,
                                minStep: Double = 1e-20, maxStep: Double = 1e20,
                                ftol: Double = 1e-4, wolfe: Double = 0.9,
                                gtol: Double = 0.9, xtol: Double = 1.0e-16)

end LBFGSLineSearchPrms


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `OrthantWisePrms` class is used to group together all parameters
 *  that are control the Orthant-Wise method for minimizing the objective
 *  function value during the L-BFGS optimization process.
 *
 *  @param c        Coefficient for the L1 norm of variables. Must be set to a
 *                  positive value to activate the Orthant-Wise Limited-memory
 *                  Quasi-Newton (OWL-QN) method, which minimizes the objective
 *                  function F(x) combined with the L1 norm |x| of the
 *                  variables: F(x) + C|x|. This parameter is the coefficient
 *                  ''C'' for the |x| term. As the L1 norm |x| is not
 *                  differentiable at zero, the code modifies function and
 *                  gradient evaluations from a client program suitably. Thus, a
 *                  client program only has to return the function value F(x)
 *                  and gradients G(x) as usual.
 *  @param start    Start index for computing L1 norm of the variables. This
 *                  parameter, which we shall henceforth call ''b'', must be
 *                  selected such that 0 &le; ''b'' &lt; N. It specifies the
 *                  index number from which the L1 norm of the variables `x`
 *                  will be computed:
 *                  |x| = |x,,''b'',,| + |x,,''b''+1,,| + ... + |x,,N,,|.
 *                  In other words, variables x,,1,,, ..., x,,''b''-1,, are not
 *                  used for computing the L1 norm. Setting ''b'' to a non-zero
 *                  value can protect variables x,,1,,, ..., x,,''b''-1,, from
 *                  being regularized (e.g.: if they represent a bias term of
 *                  logistic regression). The default value is 0.
 *  @param end      End index `Option` for computing L1 norm of the variables.
 *                  This parameter, which we shall henceforth call ''e'', must
 *                  be selected such that 0 &lt; ''e'' &le; N. It specifies the
 *                  index number at which the code stops computing the L1 norm
 *                  of the variables `x`. Setting this parameter to `None` or
 *                  `Some` with a negative value will compute the L1 norm for
 *                  all the variables `x`, which is useful when the number of
 *                  variables `x` (''N'') is not known.
 */
case class OrthantWisePrms (c: Double, start: Int = 0, end: Option[Int] = None):

    def project (d: VectorD, sign: VectorD): VectorD =
        val result = d.copy
        val adjustedEnd = end match
            case None => d.length
            case Some(index) => if index > 0 then index else d.length

        for i <- start until adjustedEnd do
            if d(i) * sign(i) <= 0 then result(i) = 0
        result
    end project

    def pseudoGradient (x: VectorD, g: VectorD): VectorD =
        val n = x.length
        val pg = new VectorD(n)
        val adjustedEnd = end match
            case None => n
            case Some(index) => if index > 0 then index else n

        // Compute the negative of gradients
        for i <- 0 until start do pg(i) = g(i)

        // Compute the psuedo-gradients
        for i <- start until adjustedEnd do
            if x(i) < 0.0 then
                pg(i) = g(i) - c                   // Differentiable
            else if 0.0 < x(i) then
                pg(i) = g(i) + c                   // Differentiable
            else if g(i) < -c then
                pg(i) = g(i) + c                   // Take the right partial derivative
            else if c < g(i) then
                pg(i) = g(i) - c                   // Take the left partial derivative
            else
                pg(i) = 0.0
        end for

        for i <- adjustedEnd until n do pg(i) = g(i)
        pg
    end pseudoGradient

    def x1Norm (x: VectorD): Double =
        var norm = 0.0
        val adjustedEnd = end match
            case None => x.length
            case Some(index) => if index > 0 then index else x.length

        for i <- start until adjustedEnd do norm += abs(x(i))
        norm
    end x1Norm

end OrthantWisePrms

