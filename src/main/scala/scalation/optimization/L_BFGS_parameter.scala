
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Jul 12 16:13:47 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   BFGS Hyper-Parameters
 *
 *  @see github.com/clementfarabet/lbfgs/blob/master/lbfgs.h
 *  @see github.com/chokkan/liblbfgs/blob/master/lib/lbfgs.c
 */

package scalation
package optimization

import scalation.mathstat.VectorD

import BFGS_code._
import BFGS_LSA._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BFGS_parameter` case class provides hyper-parameters BFGS optimization.
 *------------------------------------------------------------------------------
 * @param m  The number of corrections to approximate the inverse hessian matrix.
 *        The L-BFGS routine stores the computation results of previous \ref m
 *        iterations to approximate the inverse hessian matrix of the current
 *        iteration. This parameter controls the size of the limited memories
 *        (corrections). The default value is \c 6. Values less than \c 3 are
 *        not recommended. Large values will result in excessive computing time.
 *------------------------------------------------------------------------------
 * @param epsilon  Epsilon for convergence test.
 *        This parameter determines the accuracy with which the solution is to
 *        be found. A minimization terminates when
 *        ||g|| < \ref epsilon * max(1, ||x||),
 *        where ||.|| denotes the Euclidean (L2) norm. The default value is \c 1e-5.
 *------------------------------------------------------------------------------
 * @param past  Distance for delta-based convergence test.
 *        This parameter determines the distance, in iterations, to compute
 *        the rate of decrease of the objective function. If the value of this
 *        parameter is zero, the library does not perform the delta-based
 *        convergence test. The default value is \c 0.
 *------------------------------------------------------------------------------
 * @param delta  Delta for convergence test.
 *        This parameter determines the minimum rate of decrease of the objective function.
 *        The library stops iterations when the following condition is met:
 *        (f' - f) / f < \ref delta,
 *        where f' is the objective value of \ref past iterations ago, and f is
 *        the objective value of the current iteration.  The default value is \c 0.
 *------------------------------------------------------------------------------
 * @param max_iterations  The maximum number of iterations.
 *        The lbfgs() function terminates an optimization process with
 *        ::LBFGSERR_MAXIMUMITERATION status code when the iteration count
 *        exceedes this parameter. Setting this parameter to zero continues an
 *        optimization process until a convergence or error. The default value is \c 0.
 *------------------------------------------------------------------------------
 * @param linesearch  The line search algorithm.
 *        This parameter specifies a line search algorithm to be used by the  L-BFGS routine.
 *------------------------------------------------------------------------------
 * @param max_linesearch  The maximum number of trials for the line search.
 *        This parameter controls the number of function and gradients evaluations
 *        per iteration for the line search routine. The default value is \c 20.
 *------------------------------------------------------------------------------
 * @param min_step  The minimum step of the line search routine.
 *        The default value is \c 1e-20. This value need not be modified unless
 *        the exponents are too large for the machine being used, or unless the
 *        problem is extremely badly scaled (in which case the exponents should be increased).
 *------------------------------------------------------------------------------
 * @param max_step  The maximum step of the line search.
 *        The default value is \c 1e+20. This value need not be modified unless
 *        the exponents are too large for the machine being used, or unless the
 *        problem is extremely badly scaled (in which case the exponents should be increased).
 *------------------------------------------------------------------------------
 * @param ftol  A parameter to control the accuracy of the line search routine.
 *        The default value is \c 1e-4. This parameter should be greater
 *        than zero and smaller than \c 0.5.
 *------------------------------------------------------------------------------
 * @param wolfe  A coefficient for the Wolfe condition.
 *        This parameter is valid only when the backtracking line-search
 *        algorithm is used with the Wolfe condition,
 *        ::LBFGS_LINESEARCH_BACKTRACKING_STRONG_WOLFE or
 *        ::LBFGS_LINESEARCH_BACKTRACKING_WOLFE .
 *        The default value is \c 0.9. This parameter should be greater
 *        the \ref ftol parameter and smaller than \c 1.0.
 *------------------------------------------------------------------------------
 * @param gtol  A parameter to control the accuracy of the line search routine.
 *        The default value is \c 0.9. If the function and gradient
 *        evaluations are inexpensive with respect to the cost of the
 *        iteration (which is sometimes the case when solving very large
 *        problems) it may be advantageous to set this parameter to a small
 *        value. A typical small value is \c 0.1. This parameter shuold be
 *        greater than the \ref ftol parameter (\c 1e-4) and smaller than \c 1.0.
 *------------------------------------------------------------------------------
 * @param xtol  The machine precision for floating-point values.
 *        This parameter must be a positive value set by a client program to
 *        estimate the machine precision. The line search routine will terminate
 *        with the status code (::LBFGSERR_ROUNDING_ERROR) if the relative width
 *        of the interval of uncertainty is less than this parameter.
 *------------------------------------------------------------------------------
 * @param orthantwise_c  Coeefficient for the L1 norm of variables.
 *        This parameter should be set to zero for standard minimization
 *        problems. Setting this parameter to a positive value activates
 *        Orthant-Wise Limited-memory Quasi-Newton (OWL-QN) method, which
 *        minimizes the objective function F(x) combined with the L1 norm |x|
 *        of the variables, {F(x) + C |x|}. This parameter is the coeefficient
 *        for the |x|, i.e., C. As the L1 norm |x| is not differentiable at
 *        zero, the library modifies function and gradient evaluations from
 *        a client program suitably; a client program thus have only to return
 *        the function value F(x) and gradients G(x) as usual. The default value is zero.
 *------------------------------------------------------------------------------
 * @param orthantwise_start  Start index for computing L1 norm of the variables.
 *        This parameter is valid only for OWL-QN method
 *        (i.e., \ref orthantwise_c != 0). This parameter b (0 <= b < N)
 *        specifies the index number from which the library computes the
 *        L1 norm of the variables x,
 *            |x| := |x_{b}| + |x_{b+1}| + ... + |x_{N}| .
 *        In other words, variables x_1, ..., x_{b-1} are not used for
 *        computing the L1 norm. Setting b (0 < b < N), one can protect
 *        variables, x_1, ..., x_{b-1} (e.g., a bias term of logistic
 *        regression) from being regularized. The default value is zero.
 *------------------------------------------------------------------------------
 * @param orthantwise_end  End index for computing L1 norm of the variables.
 *        This parameter is valid only for OWL-QN method
 *        (i.e., \ref orthantwise_c != 0). This parameter e (0 < e <= N)
 *        specifies the index number at which the library stops computing the
 *        L1 norm of the variables x.
 */
case class BFGS_parameter (m:              Int      = 6,
                           epsilon:        Double   = 1e-6,
                           past:           Int      = 0,
                           delta:          Double   = 1e-6,
                           max_iterations: Int      = 1000,
                           linesearch:     BFGS_LSA = LBFGS_LINESEARCH_BACKTRACKING_WOLFE,
                           max_linesearch: Int      = 40,
                           min_step:       Double   = 1e-20,
                           max_step:       Double   = 1e20,
                           ftol:           Double   = 1e-6,
                           wolfe:          Double   = 0.9,
                           gtol:           Double   = 0.9,
                           xtol:           Double   = 1e-16,
                           orthantwise_c:  Double   = 0.0,
                           orthantwise_start: Int   = 1,
                       var orthantwise_end: Int     = -1):

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return default parameter values as a vector.
     */
    def default_param = VectorD (6.0, 1e-5, 0.0, 1e-5, 0.0, LBFGS_LINESEARCH_DEFAULT.ordinal,
                                 40.0, 1e-20, 1e20, 1e-4, 0.9, 0.9, 1.0e-16, 0.0, 0.0, -1.0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check the input parameters for errors.
     *  @param n  the dimension of the search space
     */
    def check_parameters (n: Int): Int =
        if n <= 0 then return LBFGSERR_INVALID_N.code
        if epsilon < 0.0 then return LBFGSERR_INVALID_EPSILON.code
        if past < 0 then return LBFGSERR_INVALID_TESTPERIOD.code
        if delta < 0.0 then return LBFGSERR_INVALID_DELTA.code
        if min_step < 0.0 then return LBFGSERR_INVALID_MINSTEP.code
        if max_step < min_step then return LBFGSERR_INVALID_MAXSTEP.code
        if ftol < 0.0 then return LBFGSERR_INVALID_FTOL.code
        if linesearch == LBFGS_LINESEARCH_BACKTRACKING_WOLFE ||
           linesearch == LBFGS_LINESEARCH_BACKTRACKING_STRONG_WOLFE then
            if wolfe <= ftol || 1.0 <= wolfe then return LBFGSERR_INVALID_WOLFE.code
        end if
        if gtol < 0.0 then return LBFGSERR_INVALID_GTOL.code
        if xtol < 0.0 then return LBFGSERR_INVALID_XTOL.code
        if max_linesearch <= 0 then return LBFGSERR_INVALID_MAXLINESEARCH.code
        if orthantwise_c < 0.0 then return LBFGSERR_INVALID_ORTHANTWISE.code
        if orthantwise_start < 0 || n < orthantwise_start then
            return LBFGSERR_INVALID_ORTHANTWISE_START.code
        end if
        if orthantwise_end < 0 then orthantwise_end = n
        if n < orthantwise_end then return LBFGSERR_INVALID_ORTHANTWISE_END.code
        0                                                   // return success
    end check_parameters

end BFGS_parameter


