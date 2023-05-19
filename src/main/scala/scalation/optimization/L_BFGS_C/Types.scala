
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
object Types:
  // Enumeration definitions.
  enum LBFGSLineSearchAlgorithm(val number: Int = 0):
    // Cases.
    /** The default algorithm (MoreThuente method). */
    case Default extends LBFGSLineSearchAlgorithm(0)

    /** MoreThuente method proposed by More and Thuente. */
    case MoreThuente extends LBFGSLineSearchAlgorithm(0)

    /** Backtracking method with the Armijo condition.
     * The backtracking method finds the step length such that it satisfies
     * the sufficient decrease (Armijo) condition,
     *    - f(x + a * d) &le; f(x) + lbfgs_parameter_t.ftol * a * g(x)&#94;T d,
     *
     * where x is the current point, d is the current search direction, and
     * a is the step length.
     */
    case BacktrackingArmijo extends LBFGSLineSearchAlgorithm(1)

    /** The backtracking method with the default (regular Wolfe) condition. */
    case BacktrackingDefault extends LBFGSLineSearchAlgorithm(2)

    /** Backtracking method with regular Wolfe condition.
     * The backtracking method finds the step length such that it satisfies
     * both the Armijo condition (LBFGSLineSearchAlgorithm.BacktrackingArmijo)
     * and the curvature condition,
     *    - g(x + a * d)&#94;T d &ge; lbfgs_parameter_t.wolfe * g(x)&#94;T d,
     *
     * where x is the current point, d is the current search direction, and
     * a is the step length.
     */
    case BacktrackingWolfe extends LBFGSLineSearchAlgorithm(2)

    /** Backtracking method with strong Wolfe condition.
     * The backtracking method finds the step length such that it satisfies
     * both the Armijo condition (LBFGSLineSearchAlgorithm.BacktrackingArmijo)
     * and the following condition,
     *    - |g(x + a * d)&#94;T d| &le; lbfgs_parameter_t.wolfe * |g(x)&#94;T d|,
     *
     * where x is the current point, d is the current search direction, and
     * a is the step length.
     */
    case BacktrackingStrongWolfe extends LBFGSLineSearchAlgorithm(3)
  end LBFGSLineSearchAlgorithm
  
  enum LBFGSReturnCode(val code: Int = -1024):
    // Cases.
    // L-BFGS reaches convergence.
    case Success extends LBFGSReturnCode(0)
    case Convergence extends LBFGSReturnCode(0)
    case Stop extends LBFGSReturnCode(1)

    // The initial variables already minimize the objective function.
    case AlreadyMinimized extends LBFGSReturnCode(2)

    // Unknown error.
    case UnknownError extends LBFGSReturnCode(-1024)

    // Logic error.
    case LogicError extends LBFGSReturnCode(-1023)

    // Insufficient memory.
    case OutOfMemory extends LBFGSReturnCode(-1022)

    // The minimization process has been canceled.
    case Canceled extends LBFGSReturnCode(-1021)

    // Invalid number of variables specified.
    case InvalidN extends LBFGSReturnCode(-1020)

    // Invalid number of variables (for SSE) specified.
    case InvalidNSSE extends LBFGSReturnCode(-1019)

    // The array x must be aligned to 16 (for SSE).
    case InvalidXSSE extends LBFGSReturnCode(-1018)

    // Invalid parameter lbfgs_parameter_t.epsilon specified.
    case InvalidEpsilon extends LBFGSReturnCode(-1017)

    // Invalid parameter lbfgs_parameter_t.past specified.
    case InvalidTestPeriod extends LBFGSReturnCode(-1016)

    // Invalid parameter lbfgs_parameter_t.delta specified.
    case InvalidDelta extends LBFGSReturnCode(-1015)

    // Invalid parameter lbfgs_parameter_t.linesearch specified.
    case InvalidLineSearch extends LBFGSReturnCode(-1014)

    // Invalid parameter lbfgs_parameter_t.max_step specified.
    case InvalidMinStep extends LBFGSReturnCode(-1013)

    // Invalid parameter lbfgs_parameter_t.max_step specified.
    case InvalidMaxStep extends LBFGSReturnCode(-1012)

    // Invalid parameter lbfgs_parameter_t.ftol specified.
    case InvalidFTOL extends LBFGSReturnCode(-1011)

    // Invalid parameter lbfgs_parameter_t.wolfe specified.
    case InvalidWolfe extends LBFGSReturnCode(-1010)

    // Invalid parameter lbfgs_parameter_t.gtol specified.
    case InvalidGTOL extends LBFGSReturnCode(-1009)

    // Invalid parameter lbfgs_parameter_t.xtol specified.
    case InvalidXTOL extends LBFGSReturnCode(-1008)

    // Invalid parameter lbfgs_parameter_t.max_linesearch specified.
    case InvalidMaxLineSearch extends LBFGSReturnCode(-1007)

    // Invalid parameter lbfgs_parameter_t.orthantwise_c specified.
    case InvalidOrthantwise extends LBFGSReturnCode(-1006)

    // Invalid parameter lbfgs_parameter_t.orthantwise_start specified.
    case InvalidOrthantwiseStart extends LBFGSReturnCode(-1005)

    // Invalid parameter lbfgs_parameter_t.orthantwise_end specified.
    case InvalidOrthantwiseEnd extends LBFGSReturnCode(-1004)

    // The line-search step went out of the interval of uncertainty.
    case OutOfInterval extends LBFGSReturnCode(-1003)

    // A logic error occurred or the interval of uncertainty became too small.
    case IncorrectTMinMax extends LBFGSReturnCode(-1002)

    // A rounding error occurred, or no line-search step satisfies sufficient decrease and curvature conditions.
    case RoundingError extends LBFGSReturnCode(-1001)

    // The line-search step became smaller than lbfgs_parameter_t.min_step.
    case MinimumStep extends LBFGSReturnCode(-1000)

    // The line-search step became larger than lbfgs_parameter_t.max_step.
    case MaximumStep extends LBFGSReturnCode(-999)

    // The line-search routine reaches the maximum number of evaluations.
    case MaximumLineSearch extends LBFGSReturnCode(-998)

    // The algorithm routine reaches the maximum number of iterations.
    case MaximumIteration extends LBFGSReturnCode(-997)

    // Relative width of the interval of uncertainty is at most lbfgs_parameter_t.xtol.
    case WidthTooSmall extends LBFGSReturnCode(-996)

    // A logic error (negative line-search step) occurred.
    case InvalidParameters extends LBFGSReturnCode(-995)

    // The current search direction increases the objective function value.
    case IncreaseGradient extends LBFGSReturnCode(-994)
  end LBFGSReturnCode
  
  // Case class definitions.
  case class LBFGSCallbackData(
    n: Int,
    instance: MemorySegment,
    optimizationLogic: OptimizationLogic
  )

  case class LBFGSParameters(
    m: Int = 6,
    epsilon: Double = 1e-5,
    past: Int = 0,
    delta: Double = 1e-5,
    maxIterations: Int = 0,
    lineSearch: LBFGSLineSearchAlgorithm = LBFGSLineSearchAlgorithm.Default,
    maxLinesearch: Int = 40,
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
    def copyToMemorySegment(destination: MemorySegment): Unit =
      destination.set(JAVA_INT, 0, m)
      destination.set(JAVA_DOUBLE, 8, epsilon)
      destination.set(JAVA_INT, 16, past)
      destination.set(JAVA_DOUBLE, 24, delta)
      destination.set(JAVA_INT, 32, maxIterations)
      destination.set(JAVA_INT, 36, lineSearch.number)
      destination.set(JAVA_INT, 40, maxLinesearch)
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

  object LBFGSReturnCode:
    // Public method definitions.
    def fromCode(code: Int): LBFGSReturnCode =
      LBFGSReturnCode.values.find(_.code == code).get
