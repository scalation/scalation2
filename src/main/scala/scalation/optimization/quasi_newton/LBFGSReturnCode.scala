
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Aug 21 13:48:43 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Set of return codes that can be returned in an call to the Limited memory
 *  Broyden–Fletcher–Goldfarb–Shanno (BFGS) for unconstrained optimization
 *  (L-BFGS) algorithm.
 */

// Package definition.
package scalation
package optimization
package quasi_newton

// Enumeration.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSReturnCode` enumeration describes possible return codes of the
 *  L-BFGS optimization, including different ways the optimization may correctly
 *  conclude, possible errors with the parameters given and possible errors
 *  during the optimization process.
 *
 *  @param code Integer value that represents the return code.
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

    /** A logic error occurred or the interval of uncertainty became too small.
     */
    case IncorrectTMinMax extends LBFGSReturnCode(-1002)

    /** A rounding error occurred, or no line-search step satisfies sufficient
     *  decrease and curvature conditions.
     */
    case RoundingError extends LBFGSReturnCode(-1001)

    /** The line-search step became smaller than [[LBFGSParameters.minStep]]. */
    case MinimumStep extends LBFGSReturnCode(-1000)

    /** The line-search step became larger than [[LBFGSParameters.maxStep]]. */
    case MaximumStep extends LBFGSReturnCode(-999)

    /** The line-search routine reaches the maximum number of evaluations. */
    case MaximumLineSearch extends LBFGSReturnCode(-998)

    /** The algorithm routine reaches the maximum number of iterations. */
    case MaximumIteration extends LBFGSReturnCode(-997)

    /** Relative width of the interval of uncertainty is at most
     *  [[LBFGSParameters.xtol]].
     */
    case WidthTooSmall extends LBFGSReturnCode(-996)

    /** A logic error (negative line-search step) occurred. */
    case InvalidParameters extends LBFGSReturnCode(-995)

    /** The current search direction increases the objective function value. */
    case IncreaseGradient extends LBFGSReturnCode(-994)
    
    /** Invalid parameter `momentum` specified for [[dmLBFGS]]. */
    case InvalidMomentum extends LBFGSReturnCode(-993)

    // Public methods.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a boolean indicating whether this return code is an error code.
     *
     *  @return Boolean Indicates whether this return code is an error code.
     */
    def isErrorCode: Boolean =
        this match
            case LBFGSReturnCode.Success |
                 LBFGSReturnCode.Convergence |
                 LBFGSReturnCode.Stop |
                 LBFGSReturnCode.AlreadyMinimized => false
            case _ => true
end LBFGSReturnCode

// Companion object.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** See the documentation for the accompanying companion enum.
 */
object LBFGSReturnCode:
    // Public methods.
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the `LBFGSReturnCode` corresponding to a numerical code.
     *
     *  @param code             Numerical value used to determine a
     *                          `LBFGSReturnCode` return code.
     *  @return LBFGSReturnCode `LBFGSReturnCode` whose numerical value is equal
     *                          to the `code` parameter.
     */
    def fromCode(code: Int): LBFGSReturnCode =
        LBFGSReturnCode.values.find(_.code == code).get
end LBFGSReturnCode
