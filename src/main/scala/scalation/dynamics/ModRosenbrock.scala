
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Nov  6 15:54:34 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   (2,3)-Order Modified Rosenbrock Stiff ODE Integrator or ode23s
 */

package scalation
package dynamics

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ModRosenbroc` object provides a numerical ODE solver for stiff systems.
 *  Given an unknown, time-dependent function y(t) governed by an Ordinary
 *  Differential Equation (ODE) of the form
 *      d/dt y(t) = f(t, y)
 *  compute y(t) using a (2,3)-order Modified Rosenbrock ODE Integrator (Modified Rosenbrock)
 *  or ode.23s
 *  Note: the integrateV method for a system of separable ODEs is mixed in from the
 *  `Integrator` trait.
 *  @see http://adorio-research.org/wordpress/?p=6565
 */
object ModRosenbrock
       extends Integrator:

    private val flaw = flawf ("ModRosenbrock")                    // flaw function

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute y(t) governed by a differential equation using numerical integration
     *  of the derivative function f(t, y) using a (2,3)-order Modified Rosenbrock method
     *  to return the value of y(t) at time t.
     *  @param f     the derivative function f(t, y)
     *  @param y0    value of the y-function at time t0, y0 = y(t0)
     *  @param t     the time value at which to compute y(t)
     *  @param t0    the initial time
     *  @param step  the middle step size
     */
    def integrate (f: Derivative, y0: Double, t: Double,
                   t0: Double = 0.0, step: Double = defaultStepSize): Double =
        0.0                                                                       // FIX - implement
    end integrate
 
   //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute y(t), a vector, governed by a system of differential equations using
     *  numerical integration of the derivative function f(t, y) using a (2,3)-order
     *  Modified Rosenbrock method to return the value of y(t) at time t.
     *  @param f     the array of derivative functions [f(t, y)] where y is a vector
     *  @param y0    the value of the y-function at time t0, y0 = y(t0)
     *  @param t     the time value at which to compute y(t)
     *  @param t0    the initial time
     *  @param step  the step size
     */
    def integrateVV (f: Array [DerivativeV], y0: VectorD, t: Double,
                     t0: Double = 0.0, step: Double = defaultStepSize): VectorD =
        null                                                                      // FIX - implement
    end integrateVV

end ModRosenbrock

import ModRosenbrock._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `modRosenbrockTest` main function is used to test the `ModRosenbrock` object.
 *  This test is for non-stiff equations.  Compare ode23s with ode45.
 *  > runMain scalation.dynamics.modRosenbrockTest
 */
@main def modRosenbrockTest (): Unit =

    def derv1 (t: Double, y: Double) = t + y
    val y0   = 1.24
    val t    = 1.0
    val hmin = 0.01
    val hmax = 1.0

    //def integrate (f: Derivative, y0: Double, t: Double, hmin: Double, hmax: Double,
    //               t0: Double = 0., tol: Double = 1E-5, maxSteps: Int = 1000): Double =
    //println ("\n==> at t = " + t + " y = " + integrate2 (derv1, y0, t, hmin, hmax))

    // @see http://www.mathworks.com/help/techdoc/ref/ode23.html (Example 1)

    def dx_dt (t: Double, p: VectorD) =  p(1) * p(2)
    def dy_dt (t: Double, p: VectorD) = -p(0) * p(2)
    def dz_dt (t: Double, p: VectorD) = -.51 * p(0) * p(1)
    val odes = Array [DerivativeV] (dx_dt, dy_dt, dz_dt)

    var ti  = .2
    var p   = VectorD (0.0, 1.0, 1.0)
    val p_r = new MatrixD (61, 3); for (k <- 0 until p.dim) p_r(0, k) = p(k)
    var tt  = new VectorD (61); tt(0) = 0.0
    for i <- 1 to 60 do
        tt(i) = ti * i
        p = integrateVV (odes, p, ti)
        println ("\n==> at tt = " + tt(i) + " p = " + p)
        for (k <- 0 until p.dim) p_r(i, k) = p(k)
//      p_r(i) = p
    end for

    new Plot (tt, p_r(?, 0), p_r(?, 1), "Plot p(0), p(1) vs. t")

end modRosenbrockTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `modRosenbrockTest2` main function is used to test the `ModRosenbrock` object.
 *  This test is for stiff equations.  Compare ode23s with ode45.
 *  > runMain scalation.dynamics.modRosenbrockTest2
 */
@main def modRosenbrockTest2 (): Unit =

    println ("modRosenbrockTest2 not yet implemented")             // FIX - implement

end modRosenbrockTest2

