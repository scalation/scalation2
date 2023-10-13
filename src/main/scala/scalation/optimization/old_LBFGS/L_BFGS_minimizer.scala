
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Jul 12 16:13:47 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   BFGS Line Search Minimizers
 *
 *  @see github.com/clementfarabet/lbfgs/blob/master/lbfgs.h
 *  @see github.com/chokkan/liblbfgs/blob/master/lib/lbfgs.c
 */

package scalation
package optimization

import scala.math.{abs, max, sqrt}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Find a minimizer of an interpolated cubic function.
 *  @param  cm      The minimizer of the interpolated cubic.
 *  @param  u       The value of one point, u.
 *  @param  fu      The value of f(u).
 *  @param  du      The value of f'(u).
 *  @param  v       The value of another point, v.
 *  @param  fv      The value of f(v).
 *  @param  du      The value of f'(v).
 */
def cubic_minimizer (cm: Double, u: Double, fu: Double, du: Double,
                     v: Double, fv: Double, dv: Double): Double =
    val d = v - u
    val theta = (fu - fv) * 3 / d + du + dv
    var p = abs (theta)
    var q = abs (du)
    var r = abs (dv)
    val s = max3 (p, q, r)
    // gamma = s*sqrt((theta/s)**2 - (du/s) * (dv/s))
    val a = theta / s
    var gamma = s * sqrt (a * a - (du / s) * (dv / s))
    if v < u then gamma = -gamma
    p = gamma - du + theta
    q = gamma - du + gamma + dv
    r = p / q
    u + r * d                                         // retrun updated cm
end cubic_minimizer


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Find a minimizer of an interpolated cubic function.
 *  @param  cm      The minimizer of the interpolated cubic.
 *  @param  u       The value of one point, u.
 *  @param  fu      The value of f(u).
 *  @param  du      The value of f'(u).
 *  @param  v       The value of another point, v.
 *  @param  fv      The value of f(v).
 *  @param  dv      The value of f'(v).
 *  @param  xmin    The minimum value.
 *  @param  xmax    The maximum value.
 */
def cubic_minimizer2 (cm: Double, u: Double, fu: Double, du: Double,
                      v: Double, fv: Double, dv: Double, xmin: Double, xmax: Double): Double =
    val d = v - u
    val theta = (fu - fv) * 3 / d + du + dv
    var p = abs (theta)
    var q = abs (du)
    var r = abs (dv)
    val s = max3 (p, q, r)
    // gamma = s*sqrt((theta/s)**2 - (du/s) * (dv/s))
    val a = theta / s
    var gamma = s * sqrt (max (0, a * a - (du / s) * (dv / s)))
    if u < v then gamma = -gamma
    p = gamma - dv + theta
    q = gamma - dv + gamma + du
    r = p / q
    if r < 0.0 && gamma != 0.0 then v - r * d           // return updated cm
    else if d > 0 then xmax
    else xmin
end cubic_minimizer2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Find a minimizer of an interpolated quadratic function.
 *  @param  qm      The minimizer of the interpolated quadratic.
 *  @param  u       The value of one point, u.
 *  @param  fu      The value of f(u).
 *  @param  du      The value of f'(u).
 *  @param  v       The value of another point, v.
 *  @param  fv      The value of f(v).
 */
def quad_minimizer (qm: Double, u: Double, fu: Double, du: Double, v: Double, fv: Double): Double =
    val a = v - u
    u + du / ((fu - fv) / a + du) / 2 * a               // return update qm
end quad_minimizer

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Find a minimizer of an interpolated quadratic function.
 *  @param  qm      The minimizer of the interpolated quadratic.
 *  @param  u       The value of one point, u.
 *  @param  du      The value of f'(u).
 *  @param  v       The value of another point, v.
 *  @param  dv      The value of f'(v).
 */
def quad_minimizer2 (qm: Double, u: Double, du: Double, v: Double, dv: Double): Double =
    val a = u - v
    v + dv / (dv - du) * a                                // return update qm
end quad_minimizer2

