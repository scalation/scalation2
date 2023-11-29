
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Aug 21 13:53:24 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Group of parameters that control the optimization process of the Limited
 *  memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained
 *  optimization (L-BFGS-B) algorithm.
 */

// Package definition.
package scalation.optimization

// General imports.
import java.lang.foreign.{MemoryLayout, MemorySegment, StructLayout}
import java.lang.foreign.ValueLayout.{JAVA_DOUBLE, JAVA_INT}
import scala.annotation.static

// Case class.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSParameters` class is used to group together all parameters that
 *  control the L-BFGS optimization process.
 *
 *  @param m                Number of past iterations stored in memory to
 *                          approximate the inverse hessian matrix of the
 *                          current iteration. Values less than 3 are not
 *                          recommended. Large values will result in excessive
 *                          computing time. The default value is 6.
 *  @param epsilon          Epsilon for convergence test. Determines the
 *                          accuracy with which the solution is to be found. A
 *                          minimization terminates when:
 *                          ||g|| &lt; `epsilon` * max(1, ||x||).
 *                          In the formula, ||.|| denotes the Euclidean (L2)
 *                          norm. The default value is 1e-5.
 *  @param past             Distance for delta-based convergence test.
 *                          Determines how many iterations to compute the rate
 *                          of decrease of the objective function. A value of
 *                          zero implies the delta-based convergence test will
 *                          not be performed. The default value is 0.
 *  @param delta            Delta for convergence test. Determines the minimum
 *                          rate of decrease of the objective function. The
 *                          optimization stops iterations when the following
 *                          condition is met:
 *                          (f'-f)/f &lt; `delta`.
 *                          In the formula, '' f' '' is the objective value of
 *                          `past` iterations ago, and ''f'' is the objective
 *                          value of the current iteration. The default value is
 *                          1e-5.
 *  @param maxIterations    The maximum number of iterations. The `lbfgsMain`
 *                          and `lbfgsMainCWrapper` methods in `Wrapper`
 *                          terminate an optimization process with the
 *                          [[LBFGSReturnCode.MaximumIteration]] return code
 *                          when the iteration count exceeds this parameter.
 *                          Setting this parameter to zero continues the
 *                          optimization process until a convergence or error.
 *                          The default value is 0.
 *  @param lineSearch       [[LBFGSLineSearchAlgorithm]] to specify what line
 *                          search algorithm should be used. The default value
 *                          is [[LBFGSLineSearchAlgorithm.Default]].
 *  @param maxLineSearch    Maximum number of trials for the line search.
 *                          Controls the number of function and gradient
 *                          evaluations per iteration for the line search
 *                          routine. The default value is 40.
 *  @param defaultStep      The default step selected for the line search
 *                          routine. The default value is 1.0.
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
 *                          [[LBFGSLineSearchAlgorithm]] `lineSearch` param
 *                          (e.g: [[LBFGSLineSearchAlgorithm.BacktrackingWolfe]]
 *                          or
 *                          [[LBFGSLineSearchAlgorithm.BacktrackingStrongWolfe]]
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
 *                          [[LBFGSReturnCode.RoundingError]] if the relative
 *                          width of the interval of uncertainty is less than
 *                          this parameter. The default value is 1.0e-16.
 *  @param orthantWise      [[Option]] of type [[OrthantWiseParameters]] that
 *                          specifies whether the Orthant-Wise Limited-memory
 *                          Quasi-Newton (OWL-QN) optimization method should be
 *                          used when calculating the value to be optimized. If
 *                          this parameter is set to `Some`, then the
 *                          `lineSearch` parameter should always be set to
 *                          [[LBFGSLineSearchAlgorithm.BacktrackingOrthantWise]]
 *                          or the optimization will terminate with the return
 *                          code [[LBFGSReturnCode.InvalidLineSearch]]. The same
 *                          will occur when the `lineSearch` parameter is set to
 *                          [[LBFGSLineSearchAlgorithm.BacktrackingOrthantWise]]
 *                          and this parameter is set to `None`.
 */
case class LBFGSParameters(
    m: Int = 6,
    epsilon: Double = 1e-5,
    past: Int = 0,
    delta: Double = 1e-5,
    maxIterations: Int = 0,
    lineSearch: LBFGSLineSearchAlgorithm = LBFGSLineSearchAlgorithm.Default,
    maxLineSearch: Int = 40,
    defaultStep: Double = 1.0,
    minStep: Double = 1e-20,
    maxStep: Double = 1e20,
    ftol: Double = 1e-4,
    wolfe: Double = 0.9,
    gtol: Double = 0.9,
    xtol: Double = 1.0e-16,
    orthantWise: Option[OrthantWiseParameters] = None
):
    // Public methods.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Copies the data in this `LBFGSParameters` instance to a `destination`
     *  [[MemorySegment]].
     *
     *  It is expected by this method that the `destination` memory segment was
     *  allocated using the `LBFGSParameters.memoryLayout` [[MemoryLayout]].
     *
     *  @param destination  [[MemorySegment]] that will receive the data from
     *                      this `LBFGSParameters` instance. Must have been
     *                      allocated using the [[LBFGSParameters.memoryLayout]]
     *                      [[MemoryLayout]].
     */
    def copyToMemorySegment(destination: MemorySegment): Unit =
        val orthantWiseParams = orthantWise.getOrElse(OrthantWiseParameters(0, 0, Some(-1)))

        destination.set(JAVA_INT, 0, m)
        destination.set(JAVA_DOUBLE, 8, epsilon)
        destination.set(JAVA_INT, 16, past)
        destination.set(JAVA_DOUBLE, 24, delta)
        destination.set(JAVA_INT, 32, maxIterations)
        destination.set(JAVA_INT, 36, lineSearch.number)
        destination.set(JAVA_INT, 40, maxLineSearch)
        destination.set(JAVA_DOUBLE, 48, defaultStep)
        destination.set(JAVA_DOUBLE, 56, minStep)
        destination.set(JAVA_DOUBLE, 64, maxStep)
        destination.set(JAVA_DOUBLE, 72, ftol)
        destination.set(JAVA_DOUBLE, 80, wolfe)
        destination.set(JAVA_DOUBLE, 88, gtol)
        destination.set(JAVA_DOUBLE, 96, xtol)
        destination.set(JAVA_DOUBLE, 104, orthantWiseParams.c)
        destination.set(JAVA_INT, 112, orthantWiseParams.start)
        destination.set(JAVA_INT, 116, orthantWiseParams.end.getOrElse(-1))
end LBFGSParameters

// Companion object.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** See the documentation for the accompanying companion class.
 */
case object LBFGSParameters:
    // Class variables.
    @static
    val memoryLayout: StructLayout = MemoryLayout.structLayout(
        JAVA_INT.withName("m"),
        MemoryLayout.paddingLayout(32),
        JAVA_DOUBLE.withName("epsilon"),
        JAVA_INT.withName("past"),
        MemoryLayout.paddingLayout(32),
        JAVA_DOUBLE.withName("delta"),
        JAVA_INT.withName("max_iterations"),
        JAVA_INT.withName("linesearch"),
        JAVA_INT.withName("max_linesearch"),
        MemoryLayout.paddingLayout(32),
        JAVA_DOUBLE.withName("default_step"),
        JAVA_DOUBLE.withName("min_step"),
        JAVA_DOUBLE.withName("max_step"),
        JAVA_DOUBLE.withName("ftol"),
        JAVA_DOUBLE.withName("wolfe"),
        JAVA_DOUBLE.withName("gtol"),
        JAVA_DOUBLE.withName("xtol"),
        JAVA_DOUBLE.withName("orthantwise_c"),
        JAVA_INT.withName("orthantwise_start"),
        JAVA_INT.withName("orthantwise_end")
    ).withName("lbfgs_parameter_t")
end LBFGSParameters