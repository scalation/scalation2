
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Fri Feb 24 15:22:30 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Limited memory Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained
 *  optimization (L-BFGS-B) algorithm types and classes.
 */

// Package.
package scalation.optimization.L_BFGS_C

// General imports.
import java.lang.foreign.{MemoryLayout, MemorySegment, StructLayout}
import java.lang.foreign.ValueLayout.{JAVA_DOUBLE, JAVA_INT}
import scala.annotation.static

// Object.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Types` object implements a set of types that represent various aspects
 *  of the L-BFGS logic and are necessary in order to model it.
 */
object Types:
    // Enumeration definitions.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `LBFGSLineSearchAlgorithm` enumeration describes possible line
     *  search algorithms to be used in the L-BFGS algorithm when determining
     *  the size of the step to be taken in gradient descent.
     *
     *  @param number   Numerical representation of the algorithm category.
     */
    enum LBFGSLineSearchAlgorithm(val number: Int = 0):
        // Cases.
        /** The default algorithm (MoreThuente method). */
        case Default extends LBFGSLineSearchAlgorithm(0)
    
        /** MoreThuente method proposed by More and Thuente. */
        case MoreThuente extends LBFGSLineSearchAlgorithm(0)
    
        /** Backtracking method with the Armijo condition.
         *  The backtracking method finds the step length such that it satisfies
         *  the sufficient decrease (Armijo) condition:
         *  f(x + a * d) &le; f(x) + `LBFGSParameters.ftol` * a * g(x)^T^ d.
         *
         *  Here, ''x'' is the current point, ''d'' is the current search
         *  direction, and ''a'' is the step length.
         */
        case BacktrackingArmijo extends LBFGSLineSearchAlgorithm(1)
    
        /** The backtracking method with the default (regular Wolfe) condition.
         */
        case BacktrackingDefault extends LBFGSLineSearchAlgorithm(2)
    
        /** Backtracking method with regular Wolfe condition.
         *  The backtracking method finds the step length such that it satisfies
         *  both the Armijo condition (see
         *  [[LBFGSLineSearchAlgorithm.BacktrackingArmijo]]) and the curvature
         *  condition:
         *  g(x + a * d)^T^ d &ge; `LBFGSParameters.wolfe` * g(x)^T^ d.
         *
         *  Here, ''x'' is the current point, ''d'' is the current search
         *  direction, and ''a'' is the step length.
         */
        case BacktrackingWolfe extends LBFGSLineSearchAlgorithm(2)
    
        /** Backtracking method with strong Wolfe condition.
         *  The backtracking method finds the step length such that it satisfies
         *  both the Armijo condition (see
         *  [[LBFGSLineSearchAlgorithm.BacktrackingArmijo]]) and the following
         *  condition:
         *  |g(x + a * d)^T^ d| &le; `LBFGSParameters.wolfe` * |g(x)^T^ d|.
         *
         *  Here, ''x'' is the current point, ''d'' is the current search
         *  direction, and ''a'' is the step length.
         */
        case BacktrackingStrongWolfe extends LBFGSLineSearchAlgorithm(3)
    end LBFGSLineSearchAlgorithm

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `LBFGSReturnCode` enumeration describes possible return codes of the
     *  L-BFGS optimization, including different ways the optimization may
     *  correctly conclude, possible errors with the parameters given and
     *  possible errors during the optimization process.
     *
     *  @param code  Integer value that represents the return code.
     */
    enum LBFGSReturnCode(val code: Int = -1024):
        // Cases.
        /** L-BFGS reaches convergence. */
        case Success extends LBFGSReturnCode(0)
        case Convergence extends LBFGSReturnCode(0)
        case Stop extends LBFGSReturnCode(1)
    
        /** The initial variables already minimize the objective function. */
        case AlreadyMinimized extends LBFGSReturnCode(2)
    
        /** Unknown error. */
        case UnknownError extends LBFGSReturnCode(-1024)
    
        /** Logic error. */
        case LogicError extends LBFGSReturnCode(-1023)
    
        /** Insufficient memory. */
        case OutOfMemory extends LBFGSReturnCode(-1022)
    
        /** The minimization process has been canceled. */
        case Canceled extends LBFGSReturnCode(-1021)
    
        /** Invalid number of variables specified. */
        case InvalidN extends LBFGSReturnCode(-1020)
    
        /** Invalid number of variables (for SSE) specified. */
        case InvalidNSSE extends LBFGSReturnCode(-1019)
    
        /** The array x must be aligned to 16 (for SSE). */
        case InvalidXSSE extends LBFGSReturnCode(-1018)
    
        /** Invalid parameter [[LBFGSParameters.epsilon]] specified. */
        case InvalidEpsilon extends LBFGSReturnCode(-1017)
    
        /** Invalid parameter [[LBFGSParameters.past]] specified. */
        case InvalidTestPeriod extends LBFGSReturnCode(-1016)
    
        /** Invalid parameter [[LBFGSParameters.delta]] specified. */
        case InvalidDelta extends LBFGSReturnCode(-1015)
    
        /** Invalid parameter [[LBFGSParameters.lineSearch]] specified. */
        case InvalidLineSearch extends LBFGSReturnCode(-1014)
    
        /** Invalid parameter [[LBFGSParameters.minStep]] specified. */
        case InvalidMinStep extends LBFGSReturnCode(-1013)
    
        /** Invalid parameter [[LBFGSParameters.maxStep]] specified. */
        case InvalidMaxStep extends LBFGSReturnCode(-1012)
    
        /** Invalid parameter [[LBFGSParameters.ftol]] specified. */
        case InvalidFTOL extends LBFGSReturnCode(-1011)
    
        /** Invalid parameter [[LBFGSParameters.wolfe]] specified. */
        case InvalidWolfe extends LBFGSReturnCode(-1010)
    
        /** Invalid parameter [[LBFGSParameters.gtol]] specified. */
        case InvalidGTOL extends LBFGSReturnCode(-1009)
    
        /** Invalid parameter [[LBFGSParameters.xtol]] specified. */
        case InvalidXTOL extends LBFGSReturnCode(-1008)
    
        /** Invalid parameter [[LBFGSParameters.maxLineSearch]] specified. */
        case InvalidMaxLineSearch extends LBFGSReturnCode(-1007)
    
        /** Invalid parameter [[LBFGSParameters.orthantwiseC]] specified. */
        case InvalidOrthantwise extends LBFGSReturnCode(-1006)
    
        /** Invalid parameter [[LBFGSParameters.orthantwiseStart]] specified. */
        case InvalidOrthantwiseStart extends LBFGSReturnCode(-1005)
    
        /** Invalid parameter [[LBFGSParameters.orthantwiseEnd]] specified. */
        case InvalidOrthantwiseEnd extends LBFGSReturnCode(-1004)
    
        /** The line-search step went out of the interval of uncertainty. */
        case OutOfInterval extends LBFGSReturnCode(-1003)
    
        /** A logic error occurred or the interval of uncertainty became too
         *  small.
         */
        case IncorrectTMinMax extends LBFGSReturnCode(-1002)
    
        /** A rounding error occurred, or no line-search step satisfies
         *  sufficient decrease and curvature conditions.
         */
        case RoundingError extends LBFGSReturnCode(-1001)

        /** The line-search step became smaller than
         *  [[LBFGSParameters.minStep]].
         */
        case MinimumStep extends LBFGSReturnCode(-1000)

        /** The line-search step became larger than [[LBFGSParameters.maxStep]].
         */
        case MaximumStep extends LBFGSReturnCode(-999)
    
        /** The line-search routine reaches the maximum number of evaluations.
         */
        case MaximumLineSearch extends LBFGSReturnCode(-998)
    
        /** The algorithm routine reaches the maximum number of iterations. */
        case MaximumIteration extends LBFGSReturnCode(-997)
    
        /** Relative width of the interval of uncertainty is at most
         *  [[LBFGSParameters.xtol]].
         */
        case WidthTooSmall extends LBFGSReturnCode(-996)
    
        /** A logic error (negative line-search step) occurred. */
        case InvalidParameters extends LBFGSReturnCode(-995)

        /** The current search direction increases the objective function value.
         */
        case IncreaseGradient extends LBFGSReturnCode(-994)
    end LBFGSReturnCode
    
    // Case class definitions.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `LBFGSCallbackData` class is used to group together the
     *  [[OptimizationLogic]] specified for a L-BFGS optimization with values
     *  that are parameters for the methods of the [[OptimizationLogic]]. This
     *  allows the user to pass the optimization logic of the L-BFGS
     *  optimization as a parameter to different methods and classes while
     *  retaining the ability to callback the methods of said logic with the
     *  correct parameters.
     *
     *  @param n                    The number of variables used in the
     *                              optimization.
     *  @param instance             [[MemorySegment]] containing the user data
     *                              provided for a given call of the L-BFGS
     *                              optimization. Must be compatible with the
     *                              `MemoryLayout` format expected from the
     *                              `optimizationLogic` parameter.
     *  @param optimizationLogic    [[OptimizationLogic]] that describes the
     *                              optimization steps for the L-BFGS
     *                              optimization.
     */
    case class LBFGSCallbackData(
        n: Int,
        instance: MemorySegment,
        optimizationLogic: OptimizationLogic
    )

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `LBFGSParameters` class is used to group together all parameters
     *  that control the L-BFGS optimization process.
     *
     *  @param m                Number of past iterations stored in memory to
     *                          approximate the inverse hessian matrix of the
     *                          current iteration. Values less than 3 are not
     *                          recommended. Large values will result in
     *                          excessive computing time. The default value is
     *                          6.
     *  @param epsilon          Epsilon for convergence test. Determines the
     *                          accuracy with which the solution is to be found.
     *                          A minimization terminates when:
     *                          ||g|| &lt; `epsilon` * max(1, ||x||).
     *                          In the formula, ||.|| denotes the Euclidean
     *                          (L2) norm. The default value is 1e-5.
     *  @param past             Distance for delta-based convergence test.
     *                          Determines how many iterations to compute the
     *                          rate of decrease of the objective function. A
     *                          value of zero implies the delta-based
     *                          convergence test will not be performed. The
     *                          default value is 0.
     *  @param delta            Delta for convergence test. Determines the
     *                          minimum rate of decrease of the objective
     *                          function. The optimization stops iterations when
     *                          the following condition is met:
     *                          (f'-f)/f &lt; `delta`.
     *                          In the formula, '' f' '' is the objective value
     *                          of `past` iterations ago, and ''f'' is the
     *                          objective value of the current iteration. The
     *                          default value is 1e-5.
     *  @param maxIterations    The maximum number of iterations. The
     *                          `lbfgsMain` and `lbfgsMainCWrapper` methods in
     *                          [[Wrapper]] terminate an optimization process
     *                          with the [[LBFGSReturnCode.MaximumIteration]]
     *                          status when the iteration count exceeds this
     *                          parameter. Setting this parameter to zero
     *                          continues the optimization process until a
     *                          convergence or error. The default value is 0.
     *  @param lineSearch       [[LBFGSLineSearchAlgorithm]] to specify what
     *                          line search algorithm should be used. The
     *                          default value is
     *                          [[LBFGSLineSearchAlgorithm.Default]].
     *  @param maxLineSearch    Maximum number of trials for the line search.
     *                          Controls the number of function and gradient
     *                          evaluations per iteration for the line search
     *                          routine. The default value is 40.
     *  @param minStep          Minimum step of the line search routine. Does
     *                          not need to be modified unless the exponents are
     *                          too large for the machine being used, or unless
     *                          the problem is extremely badly scaled (in which
     *                          case the exponents should be increased). The
     *                          default value is 1e-20.
     *  @param maxStep          Maximum step of the line search routine. Does
     *                          not need to be modified unless the exponents are
     *                          too large for the machine being used, or unless
     *                          the problem is extremely badly scaled (in which
     *                          case the exponents should be increased). The
     *                          default value is 1e20.
     *  @param ftol             Controls the accuracy of the line search
     *                          routine. Should be greater than zero and smaller
     *                          than 0.5. The default value is 1e-4.
     *  @param wolfe            A coefficient for the Wolfe condition. Only used
     *                          when a backtracking line-search algorithm that
     *                          relies on the Wolfe condition is chosen for the
     *                          [[LBFGSLineSearchAlgorithm]] `lineSearch` param
     *                          (e.g: `BacktrackingWolfe` or
     *                          `BacktrackingStrongWolfe`). Should be greater
     *                          than the `ftol` parameter and smaller than 1.0.
     *                          The default value is 0.9.
     *  @param gtol             Controls the accuracy of the line search
     *                          routine. If the function and gradient
     *                          evaluations are inexpensive with respect to the
     *                          cost of the iteration (which is sometimes the
     *                          case when solving very large problems), it may
     *                          be advantageous to set this parameter to a small
     *                          value (e.g: 0.1). This parameter should be
     *                          greater than the `ftol` parameter (default value
     *                          of 1e-4) and smaller than 1.0. The default value
     *                          is 0.9.
     *  @param xtol             The machine precision for floating-point values.
     *                          Must be a positive value set by a client program
     *                          to estimate the machine precision. The L-BFGS
     *                          optimization will terminate with the status
     *                          [[LBFGSReturnCode.RoundingError]] if the
     *                          relative width of the interval of uncertainty is
     *                          less than this parameter. The default value is
     *                          1.0e-16
     *  @param orthantwiseC     Coefficient for the L1 norm of variables. Should
     *                          be set to zero for standard minimization
     *                          problems. Setting it to a positive value
     *                          activates the Orthant-Wise Limited-memory
     *                          Quasi-Newton (OWL-QN) method, which minimizes
     *                          the objective function F(x) combined with the
     *                          L1 norm |x| of the variables: F(x) + C|x|. This
     *                          parameter is the coefficient ''C'' for the |x|
     *                          term. As the L1 norm |x| is not differentiable
     *                          at zero, the library modifies function and
     *                          gradient evaluations from a client program
     *                          suitably. Thus, a client program only has to
     *                          return the function value F(x) and gradients
     *                          G(x) as usual. The default value is 0.
     *  @param orthantwiseStart Start index for computing L1 norm of the
     *                          variables. Only used for the OWL-QN method (
     *                          i.e.: when `orthantwiseC` != 0). This parameter,
     *                          which we shall henceforth call ''b'', must be
     *                          selected such that 0 &le; ''b'' &lt; N. It
     *                          specifies the index number from which the L1
     *                          norm of the variables `x` will be computed:
     *                          |x| = |x,,''b'',,| + |x,,''b''+1,,| + ... +
     *                          |x,,N,,|. In other words, variables x,,1,,, ...,
     *                          x,,''b''-1,, are not used for computing the L1
     *                          norm. Setting ''b'' to a non-zero value can
     *                          protect variables x,,1,,, ..., x,,''b''-1,, from
     *                          being regularized (e.g.: if they represent a
     *                          bias term of logistic regression). The default
     *                          value is 0.
     *  @param orthantwiseEnd   End index for computing L1 norm of the
     *                          variables. Only used for the OWL-QN method
     *                          (i.e.: when `orthantwiseC` != 0). This
     *                          parameter, which we shall henceforth call ''e'',
     *                          must be selected such that 0 &lt; ''e'' &le; N.
     *                          It specifies the index number at which the
     *                          library stops computing the L1 norm of the
     *                          variables `x`.
     */
    case class LBFGSParameters(
        m: Int = 6,
        epsilon: Double = 1e-5,
        past: Int = 0,
        delta: Double = 1e-5,
        maxIterations: Int = 0,
        lineSearch: LBFGSLineSearchAlgorithm = LBFGSLineSearchAlgorithm.Default,
        maxLineSearch: Int = 40,
        minStep: Double = 1e-20,
        maxStep: Double = 1e20,
        ftol: Double = 1e-4,
        wolfe: Double = 0.9,
        gtol: Double = 0.9,
        xtol: Double = 1.0e-16,
        orthantwiseC: Double = 0.0,
        orthantwiseStart: Int = 0,
        var orthantwiseEnd: Int = -1
    ):

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Copies the data in this `LBFGSParameters` instance to a
         *  `destination` [[MemorySegment]].
         *
         *  It is expected by this method that the `destination` memory segment
         *  was allocated using the `LBFGSParameters.memoryLayout`
         *  [[MemoryLayout]].
         *
         *  @param destination  [[MemorySegment]] that will receive the data
         *                      from this `LBFGSParameters` instance. Must have
         *                      been allocated using the
         *                      [[LBFGSParameters.memoryLayout]]
         *                      [[MemoryLayout]].
         */
        def copyToMemorySegment(destination: MemorySegment): Unit =
            destination.set(JAVA_INT, 0, m)
            destination.set(JAVA_DOUBLE, 8, epsilon)
            destination.set(JAVA_INT, 16, past)
            destination.set(JAVA_DOUBLE, 24, delta)
            destination.set(JAVA_INT, 32, maxIterations)
            destination.set(JAVA_INT, 36, lineSearch.number)
            destination.set(JAVA_INT, 40, maxLineSearch)
            destination.set(JAVA_DOUBLE, 48, minStep)
            destination.set(JAVA_DOUBLE, 56, maxStep)
            destination.set(JAVA_DOUBLE, 64, ftol)
            destination.set(JAVA_DOUBLE, 72, wolfe)
            destination.set(JAVA_DOUBLE, 80, gtol)
            destination.set(JAVA_DOUBLE, 88, xtol)
            destination.set(JAVA_DOUBLE, 96, orthantwiseC)
            destination.set(JAVA_INT, 104, orthantwiseStart)
            destination.set(JAVA_INT, 108, orthantwiseEnd)
  
    // Companion object definitions.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** See the documentation for the accompanying companion class.
     */
    case object LBFGSParameters:
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

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** See the documentation for the accompanying companion class.
     */
    object LBFGSReturnCode:
        // Public method definitions.
        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Returns the `LBFGSReturnCode` corresponding to a numerical code.
         *
         *  @param code             Numerical value used to determine a
         *                          `LBFGSReturnCode` return code.
         *  @return LBFGSReturnCode `LBFGSReturnCode` whose numerical value is
         *                          equal to the `code` parameter.
         */
        def fromCode(code: Int): LBFGSReturnCode =
            LBFGSReturnCode.values.find(_.code == code).get
