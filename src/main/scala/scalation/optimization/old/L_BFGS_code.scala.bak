
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Jul 12 16:13:47 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   BFGS Error Codes - Translated from liblbfgs
 *
 *  @see github.com/clementfarabet/lbfgs/blob/master/lbfgs.h
 *  @see github.com/chokkan/liblbfgs/blob/master/lib/lbfgs.c
 */

package scalation
package optimization

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Define the values/codes used by lbfgs ().
 *  Roughly speaking, a negative value indicates an error.
 */
enum BFGS_code (code_ : Int = -1):

    def code: Int = code_

    // L-BFGS reaches convergence.
    case LBFGS_SUCCESS     extends BFGS_code (0)
    case LBFGS_CONVERGENCE extends BFGS_code (0)
    case LBFGS_STOP        extends BFGS_code (0)

    // The initial variables already minimize the objective function.
    case LBFGS_ALREADY_MINIMIZED extends BFGS_code ()

    // Unknown error.
    case LBFGSERR_UNKNOWNERROR extends BFGS_code (-1024)

    // Logic error.
    case LBFGSERR_LOGICERROR extends BFGS_code ()

    // Insufficient memory.
    case LBFGSERR_OUTOFMEMORY extends BFGS_code ()

    // The minimization process has been canceled.
    case LBFGSERR_CANCELED extends BFGS_code ()

    // Invalid number of variables specified.
    case LBFGSERR_INVALID_N extends BFGS_code ()

    // Invalid number of variables (for SSE) specified.
    case LBFGSERR_INVALID_N_SSE extends BFGS_code ()

    // The array x must be aligned to 16 (for SSE).
    case LBFGSERR_INVALID_X_SSE extends BFGS_code ()

    // Invalid parameter lbfgs_parameter_t::epsilon specified.
    case LBFGSERR_INVALID_EPSILON extends BFGS_code ()

    // Invalid parameter lbfgs_parameter_t::past specified.
    case LBFGSERR_INVALID_TESTPERIOD extends BFGS_code ()

    // Invalid parameter lbfgs_parameter_t::delta specified.
    case LBFGSERR_INVALID_DELTA extends BFGS_code ()

    // Invalid parameter lbfgs_parameter_t::linesearch specified.
    case LBFGSERR_INVALID_LINESEARCH extends BFGS_code ()

    // Invalid parameter lbfgs_parameter_t::max_step specified.
    case LBFGSERR_INVALID_MINSTEP extends BFGS_code ()

    // Invalid parameter lbfgs_parameter_t::max_step specified.
    case LBFGSERR_INVALID_MAXSTEP extends BFGS_code ()

    // Invalid parameter lbfgs_parameter_t::ftol specified.
    case LBFGSERR_INVALID_FTOL extends BFGS_code ()

    // Invalid parameter lbfgs_parameter_t::wolfe specified.
    case LBFGSERR_INVALID_WOLFE extends BFGS_code ()

    // Invalid parameter lbfgs_parameter_t::gtol specified.
    case LBFGSERR_INVALID_GTOL extends BFGS_code ()

    // Invalid parameter lbfgs_parameter_t::xtol specified.
    case LBFGSERR_INVALID_XTOL extends BFGS_code ()

    // Invalid parameter lbfgs_parameter_t::max_linesearch specified.
    case LBFGSERR_INVALID_MAXLINESEARCH extends BFGS_code ()

    // Invalid parameter lbfgs_parameter_t::orthantwise_c specified.
    case LBFGSERR_INVALID_ORTHANTWISE extends BFGS_code ()

    // Invalid parameter lbfgs_parameter_t::orthantwise_start specified.
    case LBFGSERR_INVALID_ORTHANTWISE_START extends BFGS_code ()

    // Invalid parameter lbfgs_parameter_t::orthantwise_end specified.
    case LBFGSERR_INVALID_ORTHANTWISE_END extends BFGS_code ()

    // The line-search step went out of the interval of uncertainty.
    case LBFGSERR_OUTOFINTERVAL extends BFGS_code ()

    // A logic error occurred or the interval of uncertainty became too small.
    case LBFGSERR_INCORRECT_TMINMAX extends BFGS_code ()

    // A rounding error occurred, or no line-search step satisfies sufficient decrease and curvature conditions.
    case LBFGSERR_ROUNDING_ERROR extends BFGS_code ()

    // The line-search step became smaller than lbfgs_parameter_t::min_step.
    case LBFGSERR_MINIMUMSTEP extends BFGS_code ()

    // The line-search step became larger than lbfgs_parameter_t::max_step.
    case LBFGSERR_MAXIMUMSTEP extends BFGS_code ()

    // The line-search routine reaches the maximum number of evaluations.
    case LBFGSERR_MAXIMUMLINESEARCH extends BFGS_code ()

    // The algorithm routine reaches the maximum number of iterations.
    case LBFGSERR_MAXIMUMITERATION extends BFGS_code ()

    // Relative width of the interval of uncertainty is at most lbfgs_parameter_t::xtol.
    case LBFGSERR_WIDTHTOOSMALL extends BFGS_code ()

    // A logic error (negative line-search step) occurred.
    case LBFGSERR_INVALIDPARAMETERS extends BFGS_code ()

    // The current search direction increases the objective function value.
    case LBFGSERR_INCREASEGRADIENT extends BFGS_code ()

end BFGS_code

import BFGS_code._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Return a description of the lbfgs standard error code.
 *  @pram err  the error code
 */
def lbfgs_strerror (err: BFGS_code): String =
    err match
        case LBFGS_SUCCESS =>                       // also handles LBFGS_CONVERGENCE.
            "Success: reached convergence (gtol)."

        case LBFGS_STOP =>
            "Success: met stopping criteria (ftol)."

        case LBFGS_ALREADY_MINIMIZED =>
            "The initial variables already minimize the objective function."

        case LBFGSERR_UNKNOWNERROR =>
            "Unknown error."

        case LBFGSERR_LOGICERROR =>
            "Logic error."

        case LBFGSERR_OUTOFMEMORY =>
            "Insufficient memory."

        case LBFGSERR_CANCELED =>
            "The minimization process has been canceled."

        case LBFGSERR_INVALID_N =>
            "Invalid number of variables specified."

        case LBFGSERR_INVALID_N_SSE =>
            "Invalid number of variables (for SSE) specified."

        case LBFGSERR_INVALID_X_SSE =>
            "The array x must be aligned to 16 (for SSE)."

        case LBFGSERR_INVALID_EPSILON =>
            "Invalid parameter lbfgs_parameter_t::epsilon specified."

        case LBFGSERR_INVALID_TESTPERIOD =>
            "Invalid parameter lbfgs_parameter_t::past specified."

        case LBFGSERR_INVALID_DELTA =>
            "Invalid parameter lbfgs_parameter_t::delta specified."

        case LBFGSERR_INVALID_LINESEARCH =>
            "Invalid parameter lbfgs_parameter_t::linesearch specified."

        case LBFGSERR_INVALID_MINSTEP =>
            "Invalid parameter lbfgs_parameter_t::max_step specified."

        case LBFGSERR_INVALID_MAXSTEP =>
            "Invalid parameter lbfgs_parameter_t::max_step specified."

        case LBFGSERR_INVALID_FTOL =>
            "Invalid parameter lbfgs_parameter_t::ftol specified."

        case LBFGSERR_INVALID_WOLFE =>
            "Invalid parameter lbfgs_parameter_t::wolfe specified."

        case LBFGSERR_INVALID_GTOL =>
            "Invalid parameter lbfgs_parameter_t::gtol specified."

        case LBFGSERR_INVALID_XTOL =>
            "Invalid parameter lbfgs_parameter_t::xtol specified."

        case LBFGSERR_INVALID_MAXLINESEARCH =>
            "Invalid parameter lbfgs_parameter_t::max_linesearch specified."

        case LBFGSERR_INVALID_ORTHANTWISE =>
            "Invalid parameter lbfgs_parameter_t::orthantwise_c specified."

        case LBFGSERR_INVALID_ORTHANTWISE_START =>
            "Invalid parameter lbfgs_parameter_t::orthantwise_start specified."

        case LBFGSERR_INVALID_ORTHANTWISE_END =>
            "Invalid parameter lbfgs_parameter_t::orthantwise_end specified."

        case LBFGSERR_OUTOFINTERVAL =>
            "The line-search step went out of the interval of uncertainty."

        case LBFGSERR_INCORRECT_TMINMAX =>
            "A logic error occurred, or the interval of uncertainty became too small."

        case LBFGSERR_ROUNDING_ERROR =>
            "A rounding error occurred, or no line-search step satisfies sufficient decrease and curvature conditions."

        case LBFGSERR_MINIMUMSTEP =>
            "The line-search step became smaller than lbfgs_parameter_t::min_step."

        case LBFGSERR_MAXIMUMSTEP =>
            "The line-search step became larger than lbfgs_parameter_t::max_step."

        case LBFGSERR_MAXIMUMLINESEARCH =>
            "The line-search routine reaches the maximum number of evaluations."

        case LBFGSERR_MAXIMUMITERATION =>
            "The algorithm routine reaches the maximum number of iterations."

        case LBFGSERR_WIDTHTOOSMALL =>
            "Relative width of the interval of uncertainty is at most lbfgs_parameter_t::xtol."

        case LBFGSERR_INVALIDPARAMETERS =>
            "A logic error (negative line-search step) occurred."

        case LBFGSERR_INCREASEGRADIENT =>
            "The current search direction increases the objective function value."

        case _ =>
            "(unknown)"
    end match
end lbfgs_strerror

