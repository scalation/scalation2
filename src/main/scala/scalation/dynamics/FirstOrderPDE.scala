
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Thu Mar 28 13:43:50 EDT 2013
 *  @see     LICENSE (MIT style license file)
 *  @see     www.ita.uni-heidelberg.de/~dullemond/lectures/.../Chapter_3.pdf
 *  @see     www2.hawaii.edu/~norbert/CompPhys/chapter14.pdf
 *
 *  @note    First Order Partial Differential Equation (PDE) Solver
 *           Explicit Finite Difference
 */

package scalation
package dynamics

import scala.math.{abs, ceil, exp}

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FirstOrderPDE` class is used to solve First Order Partial Differential
 *  Equations like the Advection Equation.  Let 'u(x, t)' = concentration in a fluid
 *  with velocity 'v' at position '0 <= x <= xm' and time 't' > 0.  Numerically solve the
 *
 *  Advection Equation:      u_t + v(x, t) * u_x = 0
 *  with initial conditions  u(x, 0) = ic(x)
 *      boundary conditions  (u(0, t), u(xm, t)) = bc
 *
 *  @param v   the velocity field function v(x, t)
 *  @param dt  delta 't'
 *  @param dx  delta 'x'
 *  @param xm  the length of the column
 *  @param ic  the initial conditions as a function of position 'x'
 *  @param bc  the boundary conditions as a 2-tuple for end-points 0 and 'xm'
 */
class FirstOrderPDE (v: (Double, Double) => Double, dt: Double, dx: Double, xm: Double,
                     ic: FunctionS2S, bc: (Double, Double)):

    private val nx = (ceil ((xm) / dx)).toInt + 1               // number of x values in grid
    private val r  = dt / dx                                    // multiplier in recurrence equation
    private var u  = new VectorD (nx)                           // old concentration vectors
    private var uu = new VectorD (nx)                           // new concentration vectors

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for the concentration of the column at time t, returning the vector of
     *  concentration representing the concentration profile of column over its length.
     *  This method uses an Explicit Finite Difference technique to solve the PDE.
     *  L-W is the Lax-Wendroff scheme which has second-order accuracy.
     *  @see   math.nju.edu.cn/~qzh/numPDE.pdf
     *  @param te  the time the solution is desired (t-end)
     */
    def solve (te: Double): VectorD =
 
        val nt = (ceil (te / dt)).toInt + 1                     // number of t values in grid
        for i <- 0 until nx do u(i) = ic (i*dx)                 // apply initial conditions
        u(0) = bc._1                                            // apply boundary conditions, left only

        println ("at t =  " + 0.0 + ": \tu  = " + u)

        for j <- 1 until nt do                                  // iterative over t = j*dt
            val t = j*dt                                        // current time t
            for i <- 1 until nx- 1 do                           // iterative over x = i*dx
                val x = i*dx                                    // current position x
                val a = r * v(x, t)
                                                                // explicit recurrence equation
//              uu(i) = u(i) - a * (u(i) - u(i-1))
//              uu(i) = .5 * ((1. - a) * u(i+1) + (1. + a) * u(i-1))
//              uu(i) = u(i) - .5 * a * u(i+1) + .5 * a * u(i-1)
                uu(i) = u(i) - .5 * a * (u(i+1) - u(i-1)) + .5 * a*a * (u(i+1) - 2.0*u(i) + u(i-1))  // L-W

            end for

            println (s"at t = ${t}: \tuu = $uu")
            uu(0)  = bc._1                                     // apply left boundary condition
            val ut = uu; uu = u; u = ut                        // swap new and old references
        end for
        u                                                      // return the concentration vector
    end solve

end FirstOrderPDE


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `firstOrderPDETest` main function is used to test the `FirstOrderPDE` class.
 *  Numerically solve the Advection Equation:  'du/dt + v(x, t) * du/dx = 0'
 *  > runMain scalation.dynamics.firstOrderPDETest
 */
@main def firstOrderPDETest (): Unit =

    val dt = 1.0                                               // delta t in sec
    val dx = 1.0                                               // delta x in cm
    val xm = 100.0                                             // length of column in cm

    def ic (x: Double): Double = if x < 30.0 then 1.0 else 0.0    // initial conditions
    val bc = (1.0, 0.0)                                        // boundary conditions - only a left bc

    def v (x: Double, t: Double): Double = 1.0 + 0.0 * (x+t)     // the velocity field

    val pde = new FirstOrderPDE (v, dt, dx, xm, ic, bc)

    banner ("Explicit Finite Difference")
    println ("solution = " + pde.solve (30.0))                 // solve for t = 1, 2, ... sec

end firstOrderPDETest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `firstOrderPDETest2` main function is used to test the `FirstOrderPDE` class.
 *  Numerically solve the Advection Equation:  'du/dt + v(x, t) * du/dx = 0'
 *  > runMain scalation.dynamics.firstOrderPDETest2
 */
@main def firstOrderPDETest2 (): Unit =

    val dt = .2                                                // delta t in sec
    val dx = .2                                                // delta x in cm
    val xm = 3.0                                               // length/height of column in cm
    val d  = 0.5                                               // decay width
    val x0 = 0.0

    def ic (x: Double): Double =                               // initial conditions
        xm / (1.0 + exp ((x - x0) / d))                        // logistic function

    for i <- 0 to 15 do printf ("x = %4.1f, \t%6.3f\n", i*dx, ic (i*dx)) 

    val bc = (ic (0.0), 0.0)                                   // boundary conditions - only a left bc

    def v (x: Double, t: Double): Double = 1.0 + 0.0 * (x+t)   // the velocity field

    val pde = new FirstOrderPDE (v, dt, dx, xm, ic, bc)

    banner ("Explicit Finite Difference")
    println ("solution = " + pde.solve (4.0))                  // solve for t = 2., .4 ... sec

end firstOrderPDETest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `firstOrderPDETest3` main function is used to test the `FirstOrderPDE` class.
 *  Numerically solve the Advection Equation:  'du/dt + v(x, t) * du/dx = 0'
 *  @see www.public.asu.edu/~hhuang38/pde_slides_numerical.pdf
 *  > runMain scalation.dynamics.firstOrderPDETest3
 */
@main def firstOrderPDETest3 (): Unit =

    val EPSILON = 1E-9                                         // a value close to zero

    val dt = .1                                                // delta t in sec
    val dx = .2                                                // delta x in cm
    val xm = 2.0                                               // length/height of column in cm

    def ic (x: Double): Double =                               // initial conditions
        if abs (x - .8) < EPSILON then 1.0 else 0.0

    val bc = (ic (0.0), 0.0)                                   // boundary conditions - only a left bc

    def v (x: Double, t: Double): Double = -1.0 + 0.0 * (x+t)     // the velocity field

    val pde = new FirstOrderPDE (v, dt, dx, xm, ic, bc)

    banner ("Explicit Finite Difference")
    println ("solution = " + pde.solve (2.0))                  // solve for t = .1, .2, ... sec

end firstOrderPDETest3

