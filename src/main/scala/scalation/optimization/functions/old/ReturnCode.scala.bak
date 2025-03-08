
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @date    Mon Aug 21 13:48:43 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Return Codes for Quasi-Newton Optimizers
 */

package scalation
package optimization
package functions

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ReturnCode` enumeration describes possible return codes of the
 *  L-BFGS optimization, including different ways the optimization may correctly
 *  conclude, possible errors with the parameters given and possible errors
 *  during the optimization process.
 *
 *  @param code Integer value that represents the return code.
 */
enum ReturnCode (val code: Int = -1024):

    /** L-BFGS reaches convergence. */
    case Success extends ReturnCode(0)
    case Convergence extends ReturnCode(0)
    case Stop extends ReturnCode(1)

    /** The initial variables already minimize the objective function. */
    case AlreadyMinimized extends ReturnCode(2)

    /** Unknown error. */
    case UnknownError extends ReturnCode(-1024)

    /** Logic error. */
    case LogicError extends ReturnCode(-1023)

    /** Insufficient memory. */
    case OutOfMemory extends ReturnCode(-1022)

    /** The minimization process has been canceled. */
    case Canceled extends ReturnCode(-1021)

    /** Invalid number of variables specified. */
    case InvalidN extends ReturnCode(-1020)

    /** Invalid number of variables (for SSE) specified. */
    case InvalidNSSE extends ReturnCode(-1019)

    /** The array x must be aligned to 16 (for SSE). */
    case InvalidXSSE extends ReturnCode(-1018)

    /** Invalid parameter LBFGSParameters.epsilon specified. */
    case InvalidEpsilon extends ReturnCode(-1017)

    /** Invalid parameter LBFGSParameters.past specified. */
    case InvalidTestPeriod extends ReturnCode(-1016)

    /** Invalid parameter LBFGSParameters.delta specified. */
    case InvalidDelta extends ReturnCode(-1015)

    /** Invalid parameter LBFGSParameters.lineSearch specified. */
    case InvalidLineSearch extends ReturnCode(-1014)

    /** Invalid parameter LBFGSParameters.minStep specified. */
    case InvalidMinStep extends ReturnCode(-1013)

    /** Invalid parameter LBFGSParameters.maxStep specified. */
    case InvalidMaxStep extends ReturnCode(-1012)

    /** Invalid parameter LBFGSParameters.ftol specified. */
    case InvalidFTOL extends ReturnCode(-1011)

    /** Invalid parameter BFGSParameters.wolfe specified. */
    case InvalidWolfe extends ReturnCode(-1010)

    /** Invalid parameter LBFGSParameters.gtol specified. */
    case InvalidGTOL extends ReturnCode(-1009)

    /** Invalid parameter LBFGSParameters.xtol specified. */
    case InvalidXTOL extends ReturnCode(-1008)

    /** Invalid parameter LBFGSParameters.maxLineSearch]] specified. */
    case InvalidMaxLineSearch extends ReturnCode(-1007)

    /** Invalid parameter LBFGSParameters.orthantwiseC specified. */
    case InvalidOrthantwise extends ReturnCode(-1006)

    /** Invalid parameter LBFGSParameters.orthantwiseStart specified. */
    case InvalidOrthantwiseStart extends ReturnCode(-1005)

    /** Invalid parameter LBFGSParameters.orthantwiseEnd specified. */
    case InvalidOrthantwiseEnd extends ReturnCode(-1004)

    /** The line-search step went out of the interval of uncertainty. */
    case OutOfInterval extends ReturnCode(-1003)

    /** A logic error occurred or the interval of uncertainty became too small. */
    case IncorrectTMinMax extends ReturnCode(-1002)

    /** A rounding error occurred, or no line-search step satisfies sufficient
     *  decrease and curvature conditions.
     */
    case RoundingError extends ReturnCode(-1001)

    /** The line-search step became smaller than LBFGSParameters.minStep. */
    case MinimumStep extends ReturnCode(-1000)

    /** The line-search step became larger than LBFGSParameters.maxStep. */
    case MaximumStep extends ReturnCode(-999)

    /** The line-search routine reaches the maximum number of evaluations. */
    case MaximumLineSearch extends ReturnCode(-998)

    /** The algorithm routine reaches the maximum number of iterations. */
    case MaximumIteration extends ReturnCode(-997)

    /** Relative width of the interval of uncertainty is at most LBFGSParameters.xtol. */
    case WidthTooSmall extends ReturnCode(-996)

    /** A logic error (negative line-search step) occurred. */
    case InvalidParameters extends ReturnCode(-995)

    /** The current search direction increases the objective function value. */
    case IncreaseGradient extends ReturnCode(-994)
    
    /** Invalid parameter `momentum` specified for DMLBFGS. */
    case InvalidMomentum extends ReturnCode(-993)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a boolean indicating whether this return code is an error code.
     *
     *  @return Boolean Indicates whether this return code is an error code.
     */
    def isErrorCode: Boolean =
        this match
            case ReturnCode.Success |
                 ReturnCode.Convergence |
                 ReturnCode.Stop |
                 ReturnCode.AlreadyMinimized => false
            case _ => true
    end isErrorCode

end ReturnCode


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ReturnCode` object provides a method for convert an code code to a ReturnCode enum. 
 */
object ReturnCode:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns the `ReturnCode` corresponding to a numerical code.
     *
     *  @param code        Numerical value used to determine a `ReturnCode` return code.
     *  @return ReturnCode `ReturnCode` whose numerical value is equal to the `code` parameter.
     */
    def fromCode (code: Int): ReturnCode =
        ReturnCode.values.find (_.code == code).get
    end fromCode

end ReturnCode

