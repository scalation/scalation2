
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Thu Mar  7 14:20:25 EST 2013
 *  @see     LICENSE (MIT style license file)
 *  @see     gwu.geverstine.com/pdenum.pdf
 *
 *  @note    Parabolic Partial Differential Equation (PDE) Solvers
 *           Explicit Finite Difference and Implicit Crank-Nicholson
 */

package scalation
package dynamics

import scala.math.ceil

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ParabolicPDE' class is used to solve parabolic partial differential
 *  equations like the Heat Equation.  Let 'u(x, t)' = temperature of a rod at
 *  position '0 <= x <= xm' and time 't' > 0.  Numerically solve the
 *
 *  Heat Equation:           u_t = k * u_xx
 *  with initial conditions  u(x, 0) = ic(x)
 *      boundary conditions  (u(0, t), u(xm, t)) = bc
 *
 *  @param k   the thermal conductivity
 *  @param dt  delta 't'
 *  @param dx  delta 'x'
 *  @param xm  the length of the rod
 *  @param ic  the initial conditions as a function of position 'x'
 *  @param bc  the boundary conditions as a 2-tuple for end-points 0 and 'xm'
 */
class ParabolicPDE (k: Double, dt: Double, dx: Double, xm: Double,
                    ic: FunctionS2S, bc: (Double, Double)):

    private val flaw = flawf ("ParabolicPDE")                // flaw function
    private val nx   = (ceil ((xm) / dx)).toInt + 1          // number of x values in grid
    private val r    = k * dt / (dx * dx)                    // multiplier in recurrence equation

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for the temperature of the rod at time 't', returning the vector of
     *  temperatures representing the temperature profile of the rod over its length.
     *  This method uses an Explicit Finite Difference technique to solve the PDE.
     *  @param t  the time the solution is desired
     */
    def solve (t: Double): VectorD =
        if r >= .5 then flaw ("solve", "multiplier r = " + r + " must be < .5 for stability")

        val u  = new MatrixD (nx, 2)                         // old/new temperatures as column vectors
        var j1 = 0                                           // current time index
        var j2 = 1                                           // next time index
        val nt = (ceil (t / dt)).toInt + 1                   // number of t values in grid
        for i <- 1 until nx-2 do u(i, 0) = ic (i*dx)         // apply initial conditions
        u(0, 0) = bc._1; u(nx-1, 0) = bc._2                  // apply boundary conditions
        u(0, 1) = bc._1; u(nx-1, 1) = bc._2                  // apply boundary conditions (to both)

        println (s"at t = 0.0: \tu = ${u(?, j1)}")

        for j <- 1 until nt do                               // iterative over t = j*dt
            for i <- 1 until nx-1 do                         // iterative over x = i*dx
                                                             // explicit recurrence equation
                u(i, j2) = u(i, j1) + r * (u(i-1, j1) - 2.0*u(i, j1) + u(i+1, j1))

            println (s"at t = ${j*dt}: \tu = ${u(?, j2)}")
            j2 = j1; j1 = (j1 + 1) % 2                       // toggle indices (j2 <-> j1)
        end for
        u(?, j1)                                             // return solution column vector at j1
    end solve

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for the temperature of the rod at time 't', returning the vector of
     *  temperatures representing the temperature profile of the rod over its length.
     *  This method uses the Implicit Crank-Nicolson technique to solve the PDE,
     *  which provides greater stability and accuracy.
     *  Implicit recurrence equation:
     *      -r*u(i-1, j2) + 2.*(1.+r)*u(i, j2) - r*u(i+1, j2) =
     *       r*u(i-1, j1) + 2.*(1.-r)*u(i, j1) + r*u(i+1, j1)
     *  This equation is solved simultaneously:  solve for 'u' in 'mat * u = vec'
     *  @see people.sc.fsu.edu/~jpeterson/5-CrankNicolson.pdf
     *  @param t  the time the solution is desired
     */
    def solveCN (t: Double): VectorD =
        val u  = new VectorD (nx)                            // temperatures as a vector
        val nt = (ceil (t / dt)).toInt + 1                   // number of t values in grid
        for i <- 1 until nx-2 do u(i) = ic (i*dx)            // apply initial conditions
        u(0) = bc._1; u(nx-1) = bc._2                        // apply boundary conditions

        val mx  = nx - 2                                     // all x-values, except first and last
        val mat = formMatrix (r, mx)                         // coefficients for next time point
        val vec = new VectorD (mx)                           // to hold values from current time point

        println (s"at t = 0.0: \tu = $u")

        for j <- 1 until nt do                               // iterative over time t = j*dt
            vec(0)    = 2.0*(1.0-r)*u(1) + r*u(2) + 2.0*r*bc._1
            vec(mx-1) = r*u(mx-1) + 2.0*(1.0-r)*u(mx) + 2.0*r*bc._2

            for i <- 1 until mx-1 do                         // iterative over position x = i*dx
                vec(i) = r*u(i) + 2.0*(1.0-r)*u(i+1) + r*u(i+2)

//          println (s"mat = $mat \nvec = $vec")
            val (a, b, c) = (mat.getDiag (-1), mat.getDiag (), mat.getDiag (1))
            val uu = vec.copy
            u(1 until nx-1) = solve (uu, a, b, c)            // solve for u in mat * u = vec
            println (s"at t = ${j*dt}: \tu = $u")
        end for
        u                                                    // return solution vector u
    end solveCN

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Form the tridiagonal matrix that is used in the equation 'mat * u = vec'.
     *  @param r   the multiplier in the recurrence equation
     *  @param mx  the number of positions 'x' excluding the first and last
     */
    private def formMatrix (r: Double, mx: Int): MatrixD =
        val mat = new MatrixD (mx, mx)
        for i <- 0 until mx do
            if i > 0 then mat(i, i-1) = -r                    // sub-diagonal
            mat(i, i) = 2.0*(1.0+r)                           // diagonal
            if i < mx-1 then mat(i, i+1) = -r                 // super-diagonal
        end for
        mat
    end formMatrix

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve the tridiagonal matrix equation 'Ax = d' problem where x is a
     *  tridiagonal matrix, b is a conatnt vector and u is the solution vector.
     *  Apply Thomas Algorithm for tridiagonal matrices 
     *  @see https://en.wikipedia.org/wiki/Tridiagonal_matrix_algorithm
     *  @param x  the initial input d, becomes the solution vector
     *  @param a  the subdiagonal,   indexed from 1, ..., n-1 x.getDiag (-1)
     *  @param b  the main diagonal, indexed from 0, ..., n-1 x.getDiag (0)
     *  @param c  the superdiagonal, indexed from 0, ..., n-2 x.getDiag (1)                                 // superdiagonal, indexed from 0, ..., n-2
     */
    private def solve (x: VectorD, a: VectorD, b: VectorD, c: VectorD): VectorD =
        val n = x.dim
        val scr = new VectorD (n)                             // a scratch/working vector
        scr(0) = c(0) / b(0)
        x(0)   = x(0) / b(0)

        for ix <- 1 until n do
            if ix < n-1 then scr(ix) = c(ix) / (b(ix) - a(ix) * scr(ix - 1))
            x(ix) = (x(ix) - a(ix) * x(ix - 1)) / (b(ix) - a(ix) * scr(ix - 1))

        for ix <- n - 2 to 0 by -1 do x(ix) -= scr(ix) * x(ix + 1)
        x
    end solve

end ParabolicPDE


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `parabolicPDETest` main function is used to test the `ParabolicPDE` class.
 *  Numerically solve the Heat Equation:  'du/dt = k * d^2u/dx^2'.
 *  @see personales.unican.es/gutierjm/cursos/cornell/9_PDEs.pdf
 *  > runMain scalation.dynamanics.parabolicPDETest
 */
@main def parabolicPDETest (): Unit =

    val k  = 0.82                                             // thermal conductivity in cal/s*cm*0C
    val dt = 2.0                                              // delta t in sec
    val dx = 2.5                                              // delta x in cm
    val xm = 10.0                                             // length of rod in cm

    def ic (x: Double): Double = 0.0 * x                      // initial conditions
    val bc = (100.0, 50.0)                                    // boundary conditions

    val pde = new ParabolicPDE (k, dt, dx, xm, ic, bc)

    banner ("Explicit Finite Difference")
    println ("solution = " + pde.solve (60.0))                  // solve for t = 2, 4, ... sec

    banner ("Implicit Crank-Nicholson")
    println ("solution = " + pde.solveCN (60.0))        // solve for t = 2, 4, ... sec

end parabolicPDETest

