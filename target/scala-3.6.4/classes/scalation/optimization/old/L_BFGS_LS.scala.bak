
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Jul 12 16:13:47 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   BFGS Line Search Algorithms
 *
 *  @see github.com/clementfarabet/lbfgs/blob/master/lbfgs.h
 *  @see github.com/chokkan/liblbfgs/blob/master/lib/lbfgs.c
 */

package scalation
package optimization

import scala.math.{abs, max, min}

import scalation.mathstat.VectorD

import BFGS_code._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Return true if variables x and y have different signs.
 *  in C: #define fsigndiff(x, y) (*(x) * (*(y) / fabs(*(y))) < 0.)
 *  @param x  the first variable (double)
 *  @param y  the second variable (double)
 */
inline def fsigndiff (x: Double, y: Double): Boolean = x * (y / abs (y)) < 0.0

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Line search algorithms enumeration.
 *  @param num_  the number corresponding to the line search algorithm
 */
enum BFGS_LSA (num_ : Int):

    def num: Int = num_

    /** The default algorithm (MoreThuente method). */
    case LBFGS_LINESEARCH_DEFAULT extends BFGS_LSA (0)

    /** MoreThuente method proposd by More and Thuente. */
    case LBFGS_LINESEARCH_MORETHUENTE extends BFGS_LSA (0)

    /** Backtracking method with the Armijo condition.
     *  The backtracking method finds the step length such that it satisfies
     *  the sufficient decrease (Armijo) condition,
     *    - f(x + a * d) <= f(x) + lbfgs_parameter_t::ftol * a * g(x)^T d,
     *
     *  where x is the current point, d is the current search direction, and
     *  a is the step length.
     */
    case LBFGS_LINESEARCH_BACKTRACKING_ARMIJO extends BFGS_LSA (1)

    /** The backtracking method with the defualt (regular Wolfe) condition. */
    case LBFGS_LINESEARCH_BACKTRACKING extends BFGS_LSA (2)

    /** Backtracking method with regular Wolfe condition.
     *  The backtracking method finds the step length such that it satisfies
     *  both the Armijo condition (LBFGS_LINESEARCH_BACKTRACKING_ARMIJO)
     *  and the curvature condition,
     *    - g(x + a * d)^T d >= lbfgs_parameter_t::wolfe * g(x)^T d,
     *
     *  where x is the current point, d is the current search direction, and
     *  a is the step length.
     */
    case LBFGS_LINESEARCH_BACKTRACKING_WOLFE extends BFGS_LSA (2)

    /** Backtracking method with strong Wolfe condition.
     *  The backtracking method finds the step length such that it satisfies
     *  both the Armijo condition (LBFGS_LINESEARCH_BACKTRACKING_ARMIJO)
     *  and the following condition,
     *    - |g(x + a * d)^T d| <= lbfgs_parameter_t::wolfe * |g(x)^T d|,
     *
     *  where x is the current point, d is the current search direction, and
     *  a is the step length.
     */
    case LBFGS_LINESEARCH_BACKTRACKING_STRONG_WOLFE extends BFGS_LSA (3)

end BFGS_LSA

import BFGS_LSA._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BFGS_LS` object provides multiple line search algorithms.
 */
object BFGS_LS:

    private val debug = debugf ("BFGS_LS", true)                   // debug function
    private val flaw  = flawf ("BFGS_LS")                          // flaw function

    private var param: BFGS_parameter = BFGS_parameter ()          // the (hyper) parameters
    private var ff:    VectorD => Double  = null                   // the objective function
    private var gf:    VectorD => VectorD = null                   // the gradient function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the (hyper) parameter values (to use other than default values).
     *  @param param_  the (hyper) parameters
     */
    def set_param (param_ : BFGS_parameter): Unit = { param = param_ }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the objective function.  Must be set before calling a line search algorithm.
     *  @param ff  the objective function
     */
    def set_ff (ff_ : VectorD => Double): Unit = { ff = ff_ }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the gradient functions.  Must be set before calling a line search algorithm.
     *  @param gf  the gradient function
     */
    def set_gf (gf_ : VectorD => VectorD): Unit = { gf = gf_ }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return/select a linesearch algorithm based on the given (hyper) parameter values.
     */
    def select_linesearch =                     // Linesearch_type =
        param.linesearch match
        case LBFGS_LINESEARCH_MORETHUENTE =>
            line_search_morethuente
        case _ =>
            line_search_backtracking
        end match
/*******
        if param.orthantwise_c != 0.0 then
            param.linesearch match
            case LBFGS_LINESEARCH_BACKTRACKING =>
                line_search_backtracking_owlqn _

            case _ =>
                flaw ("select_linesearch", "LBFGSERR_INVALID_LINESEARCH")
                null                     // Only backtracking method is available.
            end match
        end if
*******/
    end select_linesearch

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Line search with backtracking returning the new location for x and the
     *  status code.
     *  @see typedef int (*line_search_proc) in github.com/debbiemarkslab/plmc/blob/master/src/lib/lbfgs.c
     *  Note: the cs and param arguments are removed, handled by other means.
     *  @param x      the current location/point vector (copy in, out)
     *  @param f_     the objective function value f(x) (copy in, out)
     *  @param g_     the gradient vector at x (copy in, out)
     *  @param s      the search direction
     *  @param step_  the current step size (copy in, out)
     */
    def line_search_backtracking (x:    VectorD,
                                  f_ :    Double,
                                  g_ :    VectorD,
                                  s:      VectorD,
                                  step_ : Double): (Double, Double, Int) =
        // Copy in parameters
        var f    = f_ 
        var g    = g_ 
        var step = step_

        debug ("line_search_backtracking", s"linesearch from x = $x, f = $f, g = $g, step = $step")

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Copy out parameters, otherwise must be explicitly returned (f, step).
         *  Must be called before all returns.
         */
        inline def copy_out (): Unit = g_ set g

        var width = 0.9                                             // initial width (step size multiplier)
        val dec   = 0.5                                             // amount to decrease width
        val inc   = 2.1                                             // amount to increase width
    
        // Check the input parameters for errors.
        if step <= 0.0 then
            flaw ("line_search_backtracking", s"step = $step must be nonegative")
            copy_out ()
            return (f, step, LBFGSERR_INVALIDPARAMETERS.code)
    
        // Compute the initial gradient in the search direction.
        // Make sure that s points to a descent direction.
        val dginit = g dot s                                        // vecdot (&dginit, g, s, n)
        if dginit > 0.0 then
            flaw ("line_search_backtracking", s"dginit = $dginit must be <= 0.0")
            copy_out ()
            return (f, step, LBFGSERR_INCREASEGRADIENT.code)
    
        // The initial value of the objective function.
        val finit  = f
        val dgtest = param.ftol * dginit
    
        var count = 0                                               // iteration count

        while true do
            val xp = x.copy                                         // veccpy (x, xp, n)
            x += s * step                                           // vecadd (x, s, step, n)
    
            // Evaluate the function and gradient values.
            f = ff (x)                                              // f = cd.proc_evaluate (cd.instance, x, g, cd.n, step)
            g = gf (x)
            count += 1
            debug ("line_search_backtracking", s"count = $count, x = $x, f = $f")
    
    
            if f > finit + step * dgtest then
                width = dec
            else if param.linesearch == LBFGS_LINESEARCH_BACKTRACKING_ARMIJO then
                // The sufficient decrease condition (Armijo condition).
                debug ("line_search_backtracking", "return due to Armijo sufficient decrease condition")
                copy_out ()
                return (f, step, count)                             // exit with the Armijo condition.
            end if 
    
            // Check the Wolfe condition.
            val dg = g dot s                                        // vecdot (&dg, g, s, n)
            if dg < param.wolfe * dginit then
                width = inc
            else if param.linesearch == LBFGS_LINESEARCH_BACKTRACKING_WOLFE then
                debug ("line_search_backtracking", "return due to regular Wolfe condition")
                copy_out ()
                return (f, step, count)                             // exit with the regular Wolfe condition.
            end if 
    
            // Check the strong Wolfe condition.
            if dg > -param.wolfe * dginit then
                width = dec
            else
                debug ("line_search_backtracking", "return due to strong Wolfe condition")
                copy_out ()
                return (f, step, count)                             // exit with the strong Wolfe condition.
            end if
    
            if step < param.min_step then                           // the step is the minimum value.
                debug ("line_search_backtracking", "return due to step size too small")
                copy_out ()
                return (f, step, LBFGSERR_MINIMUMSTEP.code)
    
            if step > param.max_step then                           // the step is the maximum value.
                debug ("line_search_backtracking", "return due to step size too big")
                copy_out ()
                return (f, step, LBFGSERR_MAXIMUMSTEP.code)
    
            if param.max_linesearch <= count then                   // maximum number of iteration.
                debug ("line_search_backtracking", "return due to iteration limit")
                copy_out ()
                return (f, step, LBFGSERR_MAXIMUMLINESEARCH.code)
    
            step *= width                                           // decrease the step size by width factor
        end while

        copy_out ()
        (f, step, count)                                            // return count
    end line_search_backtracking

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Improve the current point x by moving in the search direction s.
     *  using the More-Thuente Line Search Algorithm.
     *  @see www.ii.uib.no/~lennart/drgrad/More1994.pdf
     *  @param x      the current location/point vector
     *  @param f_     the value of the objective function
     *  @param g_     the gradient vector
     *  @param s      the search direction
     *  @param step_  the current step size
     */
    def line_search_morethuente (x:      VectorD,
                                 f_ :    Double,
                                 g_ :    VectorD,
                                 s:      VectorD,
                                 step_ : Double): (Double, Double, Int) =
        var f    = f_ 
        var g    = g_ 
        var step = step_
    
        debug ("line_search_morethuente", s"linesearch from x = $x, f = $f, g = $g, step = $step")

        var count = 0                                               // iteration count
        var uinfo = 0                                               // 
        var stmin, stmax = 0.0                                      // minimum and maximum steps
    
        // Check the input parameters for errors.
        if step <= 0.0 then
            return (f, step, LBFGSERR_INVALIDPARAMETERS.code)
    
        // Compute the initial gradient in the search direction.
        // Make sure that s points to a descent direction.
        val dginit = g dot s                                        // vecdot (&dginit, g, s, n)
        if 0 < dginit then
            return (f, step, LBFGSERR_INCREASEGRADIENT.code)
    
        // Initialize local variables.
        var brackt = false                                          // whether the interval is bracketed yet
        var stage1 = true                                           // whether in stage 1 of the 2-stage algorithm
        val finit  = f                                              // initial functional value
        val dgtest = param.ftol * dginit                            //
        var width  = param.max_step - param.min_step                // initial interval width
        var prev_width = 2.0 * width                                // previous width
    
        // Variables stx, fx, dgx contain the values of the step, function, and directional derivative at the best step.
        // Variables sty, fy, dgy contain the value of the step, function, and derivative at the other endpoint of the interval of uncertainty.
        // Variables step, f, dg contain the values of the step, function, and derivative at the current step.

        var stx = 0.0                                               // best step size
        var sty = 0.0                                               // step to end of interval
        var fx  = finit                                             // functional value at best step
        var fy  = finit                                             // functional value at end step
        var dgx = dginit                                            // directional derivative best step
        var dgy = dginit                                            // directional derivative end step
    
        while true do
            // Set the minimum and maximum steps to correspond to the present interval of uncertainty.
            if brackt then
                stmin = min (stx, sty)
                stmax = max (stx, sty)
            else
                stmin = stx
                stmax = step + 4.0 * (step - stx)
            end if
    
            // Clip the step in the range of [stepmin, stepmax].
            if step < param.min_step then step = param.min_step
            if param.max_step < step then step = param.max_step
    
            // If an unusual termination is to occur then let step be the lowest point obtained so far.
            if (brackt && ((step <= stmin || stmax <= step) || param.max_linesearch <= count + 1 || uinfo != 0)) ||
               (brackt && (stmax - stmin <= param.xtol * stmax)) then
                step = stx
            end if
    
            // Compute the current value of x: x <- x + (step) * s.
            val xp = x.copy                                         // veccpy (x, xp, n) - previous point
            x += s * step                                           // vecadd (x, s, step, n) - new point
    
            // Evaluate the function and gradient values.
            f = ff (x)                                              // f = cd.proc_evaluate(cd.instance, x, g, cd.n, step)
            g = gf (x)
            val dg = g dot s                                        // vecdot (&dg, g, s, n) - directional derivative
    
            val ftest1 = finit + step * dgtest
            count += 1
    
            // Test for errors and convergence.

            if brackt && ((step <= stmin || stmax <= step) || uinfo != 0) then
                return (f, step, LBFGSERR_ROUNDING_ERROR.code)            // rounding errors prevent further progress
    
            if step == param.max_step && f <= ftest1 && dg <= dgtest then
                return (f, step, LBFGSERR_MAXIMUMSTEP.code)               // the step is the maximum value.
    
            if step == param.min_step && (ftest1 < f || dgtest <= dg) then
                return (f, step, LBFGSERR_MINIMUMSTEP.code)               // the step is the minimum value
    
            if brackt && (stmax - stmin) <= param.xtol * stmax then
                return (f, step, LBFGSERR_WIDTHTOOSMALL.code)             // relative width of interval of uncertainty is at most xtol
    
            if param.max_linesearch <= count then
                return (f, step, LBFGSERR_MAXIMUMLINESEARCH.code)         // maximum number of iteration
    
            if f <= ftest1 && abs (dg) <= param.gtol * (-dginit) then
                return (f, step, count)                                   // sufficient decrease cond. and directional deriv cond. hold.
    
            // In first stage we seek a step for which the modified function has a nonpositive value and nonnegative derivative.

            if stage1 && f <= ftest1 && min (param.ftol, param.gtol) * dginit <= dg then
                stage1 = false
    
            // A modified function is used to predict the step only if we have not obtained a step for which the modified
            // function has a nonpositive function value and nonnegative derivative, and if a lower function value has been
            // obtained but the decrease is not sufficient.

            if stage1 && ftest1 < f && f <= fx then                 // define the modified function and derivative values.
                val fm   = f - step * dgtest                        // modified functional value at current step
                val fxm  = fx - stx * dgtest                        // modified functional value at best step
                val fym  = fy - sty * dgtest                        // modified functional value at end step
                val dgm  = dg - dgtest                              // modified directional derivative at current step
                val dgxm = dgx - dgtest                             // modified directional derivative at best step
                val dgym = dgy - dgtest                             // modified directional derivative at end step
    
                // Call update_trial_interval() to update the interval of uncertainty and to compute new step.
                uinfo = update_trial_interval (stx, fxm, dgxm, sty, fym, dgym, step, fm, dgm, stmin, stmax, brackt)
    
                // Reset the function and gradient values for f.
                fx  = fxm + stx * dgtest
                fy  = fym + sty * dgtest
                dgx = dgxm + dgtest
                dgy = dgym + dgtest
            else
                // Call update_trial_interval() to update the interval of uncertainty and to compute new step.
                uinfo = update_trial_interval (stx, fx, dgx, sty, fy, dgy, step, f, dg, stmin, stmax, brackt)
            end if
    
            // Force a sufficient decrease in the interval of uncertainty.
            if brackt then
                if 0.66 * prev_width <= abs(sty - stx) then step = stx + 0.5 * (sty - stx)
                prev_width = width
                width = abs(sty - stx)
            end if
        end while
    
        (f, step, LBFGSERR_LOGICERROR.code)
    end line_search_morethuente
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Improve the current point x by moving in the search direction s.
     *  Line search algorithm for problems with L1 regularization, including optimizers
     *  such as Orthant-Wise Quasi-Newton optimizers.
     *  @param n      the dimension of search space
     *  @param x_     the current location/point vector
     *  @param f_     the value of the objective function
     *  @param g_     the gradient vector
     *  @param s      the search direction
     *  @param step_  the current step size
     *  @param xp_    the previous location/point
     *  @param gp     the previous gradient vector
     *  @param wp     the orthant for the point (updated in return)
     */
    def line_search_backtracking_owlqn (n:      Int,
                                        x_ :    VectorD,
                                        f_ :    Double,
                                        g_ :    VectorD,
                                        s:      VectorD,
                                        step_ : Double,
                                        xp_ :   VectorD,
                                        gp:     VectorD,
                                        wp:     VectorD): (Double, Double, Int) =
        debug ("line_search_backtracking_owlqn", s"linesearch from x_ = $x_")
        var x  = x_
        var f  = f_
        var g  = g_
        var step = step_
        var xp = xp_

        var i, count = 0
        var width = 0.5
        var norm = 0.0
        var dgtest = 0.0
        var finit = f

        // Check the input parameters for errors.
        if step <= 0.0 then
            return (f, step, LBFGSERR_INVALIDPARAMETERS.code)

        // Choose the orthant for the new point.
        for i <- 0 until n do wp(i) = if xp(i) == 0.0 then -gp(i) else xp(i)

        while true do                                          // Update the current point.
            xp = x.copy                                        // veccpy (x, xp, n)
            x += s * step                                      // vecadd (x, s, step, n)

            // The current point is projected onto the orthant.
            owlqn_project(x, wp, param.orthantwise_start, param.orthantwise_end)

            // Evaluate the function and gradient values.
            f = ff (x)                                         // f = cd.proc_evaluate(cd.instance, x, g, cd.n, step)
            g = gf (x)

            // Compute the L1 norm of the variables and add it to the object value.
            norm = owlqn_x1norm(x, param.orthantwise_start, param.orthantwise_end)
            f += norm * param.orthantwise_c
   
            count += 1
   
            dgtest = 0.0
            for i <- 0 until n do dgtest += (x(i) - xp(i)) * gp(i)

            if f <= finit + param.ftol * dgtest then           // The sufficient decrease condition.
                return (f, step, count)
   
            if step < param.min_step then                      // The step is the minimum value.
                return (f, step, LBFGSERR_MINIMUMSTEP.code)

            if step > param.max_step then                      // The step is the maximum value.
                return (f, step, LBFGSERR_MAXIMUMSTEP.code)

            if param.max_linesearch <= count then              // Maximum number of iteration.
                return (f, step, LBFGSERR_MAXIMUMLINESEARCH.code)

            step *= width
        end while
        (f, step, 0)                                                 // return success
    end line_search_backtracking_owlqn

end BFGS_LS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Update a safeguarded trial value and interval for line search.
 *------------------------------------------------------------------------------
 *  The parameter x represents the step with the least function value.
 *  The parameter t represents the current step. This function assumes
 *  that the derivative at the point of x in the direction of the step.
 *  If the bracket is set to true, the minimizer has been bracketed in
 *  an interval of uncertainty with endpoints between x and y.
 *------------------------------------------------------------------------------
 *  @see Jorge J. More and David J. Thuente. Line search algorithm with
 *       guaranteed sufficient decrease. ACM Transactions on Mathematical
 *       Software (TOMS), Vol 20, No 3, pp. 286-307, 1994.
 *------------------------------------------------------------------------------
 *  @param  x       The pointer to the value of one endpoint.
 *  @param  fx      The pointer to the value of f(x).
 *  @param  dx      The pointer to the value of f'(x).
 *  @param  y       The pointer to the value of another endpoint.
 *  @param  fy      The pointer to the value of f(y).
 *  @param  dy      The pointer to the value of f'(y).
 *  @param  t       The pointer to the value of the trial value, t.
 *  @param  ft      The pointer to the value of f(t).
 *  @param  dt      The pointer to the value of f'(t).
 *  @param  tmin    The minimum value for the trial value, t.
 *  @param  tmax    The maximum value for the trial value, t.
 *  @param  brackt  The pointer to the predicate if the trial value is bracketed.
 *  @return int     Status value. Zero indicates a normal termination.
 */
def update_trial_interval (x_  : Double,
                           fx_ : Double,
                           dx_ : Double,
                           y_  : Double,
                           fy_ : Double,
                           dy_ : Double,
                           t_  : Double,
                           ft:   Double,
                           dt:   Double,
                           tmin: Double,
                           tmax: Double,
                           brackt_ : Boolean): Int =
    var x  = x_
    var fx = fx_
    var dx = dx_
    var y  = y_
    var fy = fy_
    var dy = dy_
    var t  = t_
    var brackt = brackt_

    var bound = false
    var dsign = fsigndiff (dt, dx)
    var mc    = 0.0                                                 // minimizer of an interpolated cubic
    var mq    = 0.0                                                 // minimizer of an interpolated quadratic
    var newt  = 0.0                                                 // new trial value

    // Check the input parameters for errors.
    if brackt then
        if t <= min (x, y) || max (x, y) <= t then                  // trival value t is out of the interval
            return LBFGSERR_OUTOFINTERVAL.code

        if 0.0 <= dx * (t - x) then                                 // function must decrease from x
            return LBFGSERR_INCREASEGRADIENT.code

        if tmax < tmin then                                         // incorrect tmin and tmax specified
            return LBFGSERR_INCORRECT_TMINMAX.code
    end if

    // Trial value selection.
    if fx < ft then
        // Case 1: a higher function value.  The minimum is brackt.
        // If the cubic minimizer is closer to x than the quadratic one,
        // the cubic one is taken, else the average of the minimizers is taken.

        brackt = true
        bound  = true
        mc     = cubic_minimizer (mc, x, fx, dx, t, ft, dt)
        mq     = quad_minimizer (mq, x, fx, dx, t, ft)
        newt   = if abs (mc - x) < abs (mq - x) then mc else mc + 0.5 * (mq - mc)

    else if dsign then
        // Case 2: a lower function value and derivatives of opposite sign.
        // The minimum is brackt. If the cubic minimizer is closer to x than
        // the quadratic (secant) one, the cubic one is taken, else the quadratic one is taken.

        brackt = true
        bound  = false
        mc     = cubic_minimizer (mc, x, fx, dx, t, ft, dt)
        mq     = quad_minimizer2 (mq, x, dx, t, dt)
        newt   = if abs (mc - t) > abs (mq - t) then mc else mq

    else if abs (dt) < abs (dx) then
        // Case 3: a lower function value, derivatives of the same sign, and the magnitude of
        // the derivative decreases.  The cubic minimizer is only used if the cubic tends to
        // infinity in the direction of the minimizer or if the minimum of the cubic is beyond t.
        // Otherwise the cubic minimizer is defined to be either tmin or tmax. The quadratic (secant)
        // minimizer is also computed and if the minimum is brackt then the the minimizer closest
        // to x is taken, else the one farthest away is taken.

        bound = true
        mc    = cubic_minimizer2 (mc, x, fx, dx, t, ft, dt, tmin, tmax)
        mq    = quad_minimizer2 (mq, x, dx, t, dt)
        newt  =
        if brackt then
            if abs (t - mc) < abs (t - mq) then mc else mq
        else
            if abs (t - mc) > abs (t - mq) then mc else mq

    else
        // Case 4: a lower function value, derivatives of the same sign, and the magnitude of
        // the derivative does not decrease. If the minimum is not brackt, the step is either
        // tmin or tmax, else the cubic minimizer is taken.

        bound = false
        newt  = if brackt then cubic_minimizer (newt, t, ft, dt, y, fy, dy)
                else if x < t then tmax
                else tmin
    end if

    // Update interval of uncertainty:  update does not depend on new step or case analysis above.
    // Case a: if f(x) < f(t),  x <- x, y <- t
    // Case b: if f(t) <= f(x) && f'(t) * f'(x) > 0,  x <- t, y <- y
    // Case c: if f(t) <= f(x) && f'(t) * f'(x) < 0,  x <- t, y <- x

    if fx < ft then                                                 // Case a
        y = t; fy = ft; dy = dt
    else                                                            // Case c
        if dsign then y = x; fy = fx; dy = dx
                                                                    // Cases b and c
        x = t; fx = ft; dx = dt
    end if

    // Clip the new trial value in [tmin, tmax].
    if tmax < newt then newt = tmax
    if newt < tmin then newt = tmin

    // Redefine the new trial value if it is close to the upper bound of the interval.
    if brackt && bound then
        mq   = x + 0.66 * (y - x)
        if x < y then if mq < newt then newt = mq
        else if newt < mq then newt = mq
    end if

    // Return the new trial value.
    t = newt
    0                                                               // return success
end update_trial_interval


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Return the 1-norm of vector x.
 *  @param x      the vector whose 1-norm is sought
 *  @param start  the start index
 *  @param n      the end index
 */
def owlqn_x1norm (x: VectorD, start: Int, n: Int): Double =
    var norm = 0.0
    for i <- start until n do norm += abs (x(i))
    norm
end owlqn_x1norm


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Project ...
 *  @param d      the distance vector
 *  @param sign   the sign vector
 *  @param start  the start index
 *  @param end_   the end index
 */
def owlqn_project (d: VectorD, sign: VectorD, start: Int, end_ : Int): Unit =
    for i <- start until end_ do
        if d(i) * sign(i) <= 0.0 then d(i) = 0.0
    end for
end owlqn_project


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bFGS_LSTest` main function tests the `BFGS_LS` object.
 *  > runMain scalation.optimization.bFGS_LSTest
 */
@main def bFGS_LSTest (): Unit =

    println ("\nMinimize: (x_0 - 2)^2 + (x_1 - 3)^2 + 1")

    def ff(x: VectorD): Double  = (x(0) - 2)~^2 + (x(1) - 3)~^2 + 1
    def gf(x: VectorD): VectorD = VectorD (2 * x(0) - 4, 2 * x(1) - 6)

    BFGS_LS.set_ff (ff)
    BFGS_LS.set_gf (gf)

    val x    = VectorD (0, 0)               // the current location/point vector
    val f    = ff(x)                        // the objective function value f(x)
    val g    = gf(x)                        // the gradient vector at x
//  val s    = -g                           // the search direction (e.g., opposite g)
    val s    = VectorD (1, 1)               // custom search direction
    val step = 0.2                          // the initial step size

    banner (s"x = $x, f = $f, g = $g, s = $s, step = $step")

    val code = BFGS_LS.line_search_backtracking (x, f, g, s, step)
//  val code = BFGS_LS.line_search_morethuente (x, f, g, s, step)

    println (s"optimal solution x = $x with an objective value ff(x) = ${ff(x)}, with status code $code")

end bFGS_LSTest

