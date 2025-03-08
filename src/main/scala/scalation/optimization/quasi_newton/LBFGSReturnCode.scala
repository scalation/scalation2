
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Mon Aug 21 13:48:43 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Set of Return Codes that Can Be Returned the L-BFGS Algorithm
 */

package scalation
package optimization
package quasi_newton

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSReturnCode` enumeration describes possible return codes of the
 *  L-BFGS optimization, including different ways the optimization may correctly
 *  conclude, possible errors with the parameters given and possible errors
 *  during the optimization process.
 *
 *  @param code  integer value that represents the return code.
 */
enum LBFGSReturnCode (val code: Int = -1024):

    /** L-BFGS reaches convergence.
     */
    case Success extends LBFGSReturnCode (0)
    case Convergence extends LBFGSReturnCode (0)
    case Stop extends LBFGSReturnCode (1)

    /** The initial variables already minimize the objective function.
     */
    case AlreadyMinimized extends LBFGSReturnCode (2)

    /** Unknown error.
     */
    case UnknownError extends LBFGSReturnCode (-1024)

    /** Logic error.
     */
    case LogicError extends LBFGSReturnCode (-1023)

    /** Insufficient memory.
     */
    case OutOfMemory extends LBFGSReturnCode (-1022)

    /** The minimization process has been canceled.
     */
    case Canceled extends LBFGSReturnCode (-1021)

    /** Invalid number of variables specified.
     */
    case InvalidN extends LBFGSReturnCode (-1020)

    /** Invalid number of variables (for SSE) specified.
     */
    case InvalidNSSE extends LBFGSReturnCode (-1019)

    /** The array x must be aligned to 16 (for SSE).
     */
    case InvalidXSSE extends LBFGSReturnCode (-1018)

    /** Invalid parameter `LBFGSPrms.epsilon` specified.
     */
    case InvalidEpsilon extends LBFGSReturnCode (-1017)

    /** Invalid parameter `LBFGSPrms.past` specified.
     */
    case InvalidTestPeriod extends LBFGSReturnCode (-1016)

    /** Invalid parameter `LBFGSPrms.delta` specified.
     */
    case InvalidDelta extends LBFGSReturnCode (-1015)

    /** Invalid parameter `LBFGSPrms.lineSearch` specified.
     */
    case InvalidLineSearch extends LBFGSReturnCode (-1014)

    /** Invalid parameter `LBFGSPrms.minStep` specified.
     */
    case InvalidMinStep extends LBFGSReturnCode (-1013)

    /** Invalid parameter `LBFGSPrms.maxStep` specified.
     */
    case InvalidMaxStep extends LBFGSReturnCode (-1012)

    /** Invalid parameter `LBFGSPrms.ftol` specified.
     */
    case InvalidFTOL extends LBFGSReturnCode (-1011)

    /** Invalid parameter `LBFGSPrms.wolfe` specified.
     */
    case InvalidWolfe extends LBFGSReturnCode (-1010)

    /** Invalid parameter `LBFGSPrms.gtol` specified.
     */
    case InvalidGTOL extends LBFGSReturnCode (-1009)

    /** Invalid parameter `LBFGSPrms.xtol` specified.
     */
    case InvalidXTOL extends LBFGSReturnCode (-1008)

    /** Invalid parameter `LBFGSPrms.maxLineSearch` specified.
     */
    case InvalidMaxLineSearch extends LBFGSReturnCode (-1007)

    /** Invalid parameter `LBFGSPrms.orthantwiseC` specified.
     */
    case InvalidOrthantwise extends LBFGSReturnCode (-1006)

    /** Invalid parameter `LBFGSPrms.orthantwiseStart` specified.
     */
    case InvalidOrthantwiseStart extends LBFGSReturnCode (-1005)

    /** Invalid parameter `LBFGSPrms.orthantwiseEnd` specified.
     */
    case InvalidOrthantwiseEnd extends LBFGSReturnCode (-1004)

    /** The line-search step went out of the interval of uncertainty.
     */
    case OutOfInterval extends LBFGSReturnCode (-1003)

    /** A logic error occurred or the interval of uncertainty became too small.
     */
    case IncorrectTMinMax extends LBFGSReturnCode (-1002)

    /** A rounding error occurred, or no line-search step satisfies sufficient
     *  decrease and curvature conditions.
     */
    case RoundingError extends LBFGSReturnCode (-1001)

    /** The line-search step became smaller than `LBFGSPrms.minStep`.
     */
    case MinimumStep extends LBFGSReturnCode (-1000)

    /** The line-search step became larger than `LBFGSPrms.maxStep`.
     */
    case MaximumStep extends LBFGSReturnCode (-999)

    /** The line-search routine reaches the maximum number of evaluations.
     */
    case MaximumLineSearch extends LBFGSReturnCode (-998)

    /** The algorithm routine reaches the maximum number of iterations.
     */
    case MaximumIteration extends LBFGSReturnCode (-997)

    /** Relative width of the interval of uncertainty is at most `LBFGSPrms.xtol`.
     */
    case WidthTooSmall extends LBFGSReturnCode (-996)

    /** A logic error (negative line-search step) occurred.
     */
    case InvalidPrms extends LBFGSReturnCode (-995)

    /** The current search direction increases the objective function value.
     */
    case IncreaseGradient extends LBFGSReturnCode (-994)
    
    /** Invalid parameter `momentum` specified for `dmLBFGS`.
     */
    case InvalidMomentum extends LBFGSReturnCode (-993)

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
    end isErrorCode

end LBFGSReturnCode


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LBFGSReturnCode companion object ...
 */
object LBFGSReturnCode:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the `LBFGSReturnCode` corresponding to a numerical code.
     *
     *  @param code             Numerical value used to determine a
     *                          `LBFGSReturnCode` return code.
     *  @return LBFGSReturnCode `LBFGSReturnCode` whose numerical value is equal
     *                          to the `code` parameter.
     */
    def fromCode (code: Int): LBFGSReturnCode =
        LBFGSReturnCode.values.find (_.code == code).get
    end fromCode

end LBFGSReturnCode

