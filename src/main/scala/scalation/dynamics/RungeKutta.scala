
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Oct 25 18:41:26 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   4th-Order Runge-Kutta Integrator (RK4) or ode44
 */

package scalation
package dynamics

import scala.math.{abs, E, round}

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RungeKutta` object provides an implementation of a classical numerical
 *  ODE solver.  Given an unknown, time-dependent function y(t) governed by an
 *  Ordinary Differential Equation (ODE) of the form:
 *      d/dt y(t) = f(t, y)
 *  Compute y(t) using a 4th-order Runge-Kutta Integrator (RK4).  Note: the
 *  'integrateV' method for a system of separable ODEs is mixed in from the
 *  `Integrator` trait.
 */
object RungeKutta
       extends Integrator:

    private val flaw  = flawf ("RungeKutta")                   // flaw function
    private val sixth = 1.0 / 6.0                              // one sixth
    private val ovf   = Double.MaxValue / 10.0                 // too big, may overflow

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute y(t) governed by a differential equation using numerical integration
     *  of the derivative function f(t, y) using a 4th-order Runge-Kutta method to
     *  return the value of y(t) at time t.
     *  @param f     the derivative function f(t, y) where y is a scalar
     *  @param y0    the value of the y-function at time t0, y0 = y(t0)
     *  @param t     the time value at which to compute y(t)
     *  @param t0    the initial time
     *  @param step  the step size
     */
    def integrate (f: Derivative, y0: Double, t: Double,
                   t0: Double = 0.0, step: Double = defaultStepSize): Double =
        val t_t0  = t - t0                                     // time interval
        val steps = (round (t_t0 / step)).toInt                // number of steps
        var h     = t_t0 / steps.toDouble                      // adjusted step size
        var ti    = t0                                         // initialize ith time ti to t0
        var y     = y0                                         // initialize y = f(t) to y0

        var a = 0.0; var b = 0.0; var c = 0.0; var d = 0.0

        for i <- 1 to steps do
            if ti > t then { h -= ti - t; ti = t }             // don't go past t

            a = h * f(ti, y)
            b = h * f(ti + 0.5*h, y + 0.5*a)
            c = h * f(ti + 0.5*h, y + 0.5*b)
            d = h * f(ti + h, y + c)
            y += (a + 2.0*b + 2.0*c + d) * sixth

            if abs (y) > ovf then flaw ("integrate", "probable overflow since y = " + y)
            if i % 1000 == 0 then println ("integrate: iteration " + i + " ti = " + ti + " y = " + y)
            ti += h                                            // take the next step
        end for
        y                           // the value of the function at time t, y = f(t)
    end integrate

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute y(t), a vector, governed by a system of differential equations using
     *  numerical integration of the derivative function f(t, y) using a 4th-order
     *  Runge-Kutta method to return the value of y(t) at time t.
     *  @param f     the array of derivative functions [f(t, y)] where y is a vector
     *  @param y0    the value of the y-function at time t0, y0 = y(t0)
     *  @param t     the time value at which to compute y(t)
     *  @param t0    the initial time
     *  @param step  the step size
     */
    def integrateVV (f: Array [DerivativeV], y0: VectorD, t: Double,
                     t0: Double = 0.0, step: Double = defaultStepSize): VectorD =
        val t_t0       = t - t0                                  // time interval
        val steps: Int = (round (t_t0 / step)).toInt             // number of steps
        var h          = t_t0 / steps.toDouble                   // adjusted step size
        var ti         = t0                                      // initialize ith time ti to t0
        val y          = y0                                      // initialize y = f(t) to y0

        val a = new VectorD (y.dim)
        val b = new VectorD (y.dim)
        val c = new VectorD (y.dim)
        val d = new VectorD (y.dim)

        for i <- 1 to steps do
            if ti > t then { h -= ti - t; ti = t }                  // don't go past t

            for j <- y.indices do a(j) = h * f(j)(ti, y)
            for j <- y.indices do b(j) = h * f(j)(ti + 0.5*h, y + a(j)*0.5)
            for j <- y.indices do c(j) = h * f(j)(ti + 0.5*h, y + b(j)*0.5)
            for j <- y.indices do d(j) = h * f(j)(ti + h, y + c(j))
            for j <- y.indices do y(j) += (a(j) + 2.0*b(j) + 2.0*c(j) + d(j)) * sixth

            if abs (y(0)) > ovf then flaw ("integrateVV", "probable overflow since y = " + y)
            if i % 1000 == 0 then println ("integrateVV: iteration " + i + " ti = " + ti + " y = " + y)
            ti += h                                              // take the next step
        end for
        y                  // the value of the function at time t, y = f(t)
    end integrateVV

end RungeKutta

import RungeKutta._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `rungeKuttaTest` mian function is used to test the `RungeKutta` object.
 *  > runMain scalation.dynamics.rungeKuttaTest
 */
@main def rungeKuttaTest (): Unit =

    val y0 = 1.0
    val t  = 2.0

    def derv1 (t: Double, y: Double) = 2.0 * t       // solution to differential equation is t^2
    println ("\n==> at t = " + t + " y = " + integrate (derv1, y0, t))
    println ("\n==> t^2 + c = " + 5)

    def derv2 (t: Double, y: Double) = y             // solution to differential equation is e^t
    println ("\n==> at t = " + t + " y = " + integrate (derv2, y0, t))
    println ("\n==> e = " + E + " e^2 = " + E * E)

    def derv3 (t: Double, y: Double) = t + y         // solution to differential equation is ?
    println ("\n==> at t = " + t + " y = " + integrate (derv3, y0, t))

    println ("\n==> at t = " + t + " y = " + 
             integrateV (Array (derv1, derv2), VectorD (1.0, 2.0), t))

    // @see http://www.mathworks.com/help/techdoc/ref/ode23.html (Example 1)

    def dx_dt (t: Double, p: VectorD) =  p(1) * p(2)
    def dy_dt (t: Double, p: VectorD) = -p(0) * p(2)
    def dz_dt (t: Double, p: VectorD) = -.51 * p(0) * p(1)
    val odes = Array [DerivativeV] (dx_dt, dy_dt, dz_dt)

    var ti  = .2
    var p   = VectorD (0.0, 1.0, 1.0)
    val p_r = new MatrixD (61, 3); for (k <- 0 until p.dim) p_r(0, k) = p(k)
    var tt  = VectorD (61); tt(0) = 0.0
    for i <- 1 to 60 do
        tt(i) = ti * i
        p = integrateVV (odes, p, ti)
        println ("\n==> at tt = " + tt(i) + " p = " + p)
        for k <- p.indices do p_r(i, k) = p(k)
//      p_r(i) = p
    end for

    new Plot (tt, p_r(?, 0), p_r(?, 1), "Plot p(0), p(1) vs. t")

end rungeKuttaTest

