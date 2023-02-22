
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Mustafa Nural
 *  @version 2.0
 *  @date    Mon Apr 24 21:28:06 EDT 2017
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Alternating Direction Method of Multipliers (ADMM) for Lasso Regression
 *
 *  @see www.simonlucey.com/lasso-using-admm/
 *  @see statweb.stanford.edu/~candes/math301/Lectures/Consensus.pdf
 *  @see web.stanford.edu/~boyd/papers/admm_distr_stats.html
 *  Adjusted from Boyd implementation
 */

package scalation
package optimization

import scala.collection.mutable
import scala.math.{abs, max, sqrt}
import scala.util.control.Breaks._

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LassoAdmm` class performs LASSO regression using Alternating Direction
 *  Method of Multipliers (ADMM).  Minimize the following objective function to
 *  find an optimal solutions for x.
 *
 *      argmin_x (1/2)||Ax − b||_2^2 + λ||x||_1
 *
 *      A = data matrix
 *      b = response vector
 *      λ = weighting on the l_1 penalty
 *      x = solution (coefficient vector)
 *
 *  @see euler.stat.yale.edu/~tba3/stat612/lectures/lec23/lecture23.pdf
 *  @see https://web.stanford.edu/~boyd/papers/admm_distr_stats.html
 */
object LassoAdmm:

    private val debug   = debugf ("LassoAdmm", false)         // debug flag
    private val maxIter = 5000                                // maximum number of iterations

    val ρ               = 1                                   // augmented lagrangian parameter
    private val α       = 1.5                                 // relaxation parameter

    private val ABSTOL  = 1e-4                                // Absolute tolerance
    private val RELTOL  = 1e-2                                // Relative tolerance

    private var warmStartMap = new mutable.HashMap [MatrixD, (VectorD, VectorD)]

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the warm start map.
     */
    def reset: Unit = 
        warmStartMap = new mutable.HashMap [MatrixD, (VectorD, VectorD)]
    end reset

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for x using ADMM.
     *  @param a the data matrix
     *  @param b the response vector
     *  @param λ the regularization l_1 penalty weight
     */
    def solve (a: MatrixD, b: VectorD, λ: Double = 0.01): VectorD =
        val at  = a.transpose
        val ata = at * a
        for i <- ata.indices do ata(i, i) += ρ                // ata_ρI
        val ata_ρI_inv = Fac_Inverse.inverse (ata)            // ata.inverse

        solveCached (ata_ρI_inv, at * b, λ)
    end solve

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Solve for x using ADMM using cached factorizations for efficiency.
     *  @param ata_ρI_inv   cached (a.t * a + ρI)^-1
     *  @param atb          cached a.t * b
     *  @param λ            the regularization l_1 penalty weight
     */
    def solveCached (ata_ρI_inv: MatrixD, atb: VectorD, λ: Double): VectorD =
        val n = ata_ρI_inv.dim                          // # rows, # columns in data matrix

        var x: VectorD     = null                       // the solution (coefficient vector)
        var x_hat: VectorD = null                       // the solution (coefficient vector)
        var z: VectorD     = null                       // the ? vector
        var l: VectorD     = null                       // the Lagrangian vector

        if warmStartMap.contains (ata_ρI_inv) then
            z = warmStartMap (ata_ρI_inv)._1
            l = warmStartMap (ata_ρI_inv)._2
        else
            z = new VectorD (n)
            l = new VectorD (n)
        end if

        var z_old: VectorD = null

        breakable {
            for k <- 0 until maxIter do
                z_old = z

                x     = ata_ρI_inv * (atb + (z - l) * ρ )    // solve sub-problem for x
                x_hat = x * α + z_old * (1 - α)
                z     = fast_sthresh (x_hat + l, λ / ρ)
                l    += x_hat - z

                val r_norm = (x - z).norm
                val s_norm = ((z - z_old) * -ρ).norm

                val eps_pri  = sqrt (n) * ABSTOL + RELTOL * max (x.norm, -z.norm)
                val eps_dual = sqrt (n) * ABSTOL + RELTOL * (l * ρ).norm

                // @see https://web.stanford.edu/~boyd/papers/admm/lasso/lasso.html
                // break loop if no progress
                if r_norm < eps_pri && s_norm < eps_dual then break ()
                debug ("solve", s"on iteration $k: x = $x")
            end for
        } // breakable

        warmStartMap.put (ata_ρI_inv, (z, l))
        x
    end solveCached

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the fast soft thresholding function.
     *  @param v   the vector to threshold
     *  @param thr the threshold
     */
    def fast_sthresh (v: VectorD, thr: Double): VectorD =
        VectorD (for i <- v.indices yield sign (max (abs (v(i)) - thr, 0.0), v(i)))
    end fast_sthresh

end LassoAdmm


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LassoAdmmTest` main function tests `LassoAdmm` class using the following
 *  regression equation.
 *      y  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  @see statmaster.sdu.dk/courses/st111/module03/index.html
 *  > runMain scalation.optimization.LassoAdmmTest
 */
@main def lassoAdmmTest (): Unit =

    val a = MatrixD ((5, 3), 1.0, 36.0,  66.0,               // 5-by-3 data matrix
                             1.0, 37.0,  68.0,
                             1.0, 47.0,  64.0,
                             1.0, 32.0,  53.0,
                             1.0,  1.0, 101.0)
    val b = VectorD (745.0, 895.0, 442.0, 440.0, 1598.0)         // response vector

    val x    = LassoAdmm.solve (a, b)                            // optimal coefficient vector
    val e    = b - a * x                                         // error vector
    val sse  = e dot e                                           // sum of squared errors
    val sst  = (b dot b) - b.sum~^2.0 / b.dim.toDouble           // total sum of squares
    val ssr  = sst - sse                                         // regression sum of squares
    val rSquared = ssr / sst                                     // coefficient of determination

    println (s"x        = $x")
    println (s"e        = $x")
    println (s"sse      = $sse")
    println (s"rSquared = $rSquared")

end lassoAdmmTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LassoAdmmTest2` main function tests `LassoAdmm` class using the following
 *  regression equation.
 *      y  =  b dot x  =  b_0 + b_1*x_1 + b_2*x_2.
 *  @see www.cs.jhu.edu/~svitlana/papers/non_refereed/optimization_1.pdf
 *  > runMain scalation.optimization.lassoAdmmTest2
 */
@main def lassoAdmmTest2 (): Unit =

    val MAX_ITER = 30
    val eta      = 0.1                                   // learning rate
    val p0       = 0.25; var p = p0                      // initial penalty (p = p0)
    var l        = 0.0                                   // initial value for Lagrange multiplier

    // function to optimize
    def f(x: VectorD): Double = (x(0) - 4)~^2 + (x(1) - 2)~^2

    // equality constraint to maintain
    def h(x: VectorD): Double = x(0) - x(1)

    // augmented Lagrangian
    def lg (x: VectorD): Double = f(x) + (p/2) * h(x)~^2 - l * h(x)

    // gradient of Augmented Lagrangian
    def grad (x: VectorD): VectorD = 
        VectorD (2 * (x(0) - 4) + p * (x(0) - x(1)) - l,
                 2 * (x(1) - 2) - p * (x(0) - x(1)) + l)
    end grad

    val x   = new VectorD (2)                            // vector to optimize
    val z   = new MatrixD (MAX_ITER, 2)                  // store x's trajectory

    for k <- 1 to MAX_ITER do
        l -= p * h(x)                                    // comment out for Penalty Method
        x -= grad (x) * eta
        z(k-1) = x.copy
        println (s"$k: x = $x, f(x) = ${f(x)}, lg(x) = ${lg(x)}, p = $p, l = $l")
        p += p0
    end for

    new Plot (z(?, 0), z(?, 1))

end lassoAdmmTest2

