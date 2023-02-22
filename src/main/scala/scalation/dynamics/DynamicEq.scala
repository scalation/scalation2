
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Dec 23 14:12:56 EST 2020
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Simulate a System of Dynamic Equations over Time
 */

package scalation
package dynamics

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `DynamicEq` class may be used for determining trajectories x_t from a
 *  system of dynamic equations.
 *      x_t = f(t)
 *  f(t) is the vector function of time.
 *  @param f  the vector-valued function of time
 *  @param d  the number of dimensions
 */
class DynamicEq (f: Double => VectorD, d: Int = 2):

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Record the trajectory for an object (e.g., golf ball) over n time points.
     *  @param t0  the start time of the simulation
     *  @param t1  the end time of the simulation
     *  @param n   the number of points to plot
     */
    def trajectory (t0: Double, t1: Double, n: Int): MatrixD =
        val traj = new MatrixD (n, d)
        val dt   = (t1 - t0) / n.toDouble
        var t    = t0
        for i <- 0 until n do
            traj(i) = f(t)
            t += dt
        end for
        traj
    end trajectory

end DynamicEq


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `dynamicEqTest` function to test the `DynamicEq` class using example at
 *  in Introduction to Computational Data Science using ScalaTion, section 15.1.
 *  > runMain scalation.dynamics.dynamicEqTest
 */
@main def dynamicEqTest (): Unit =
    val g   = 9.807                                                                // gravitational constant
    val mps = 60.0                                                                 // meters per second (mps)
    def f (t: Double): VectorD = VectorD (-0.5 * g * t~^2 + mps * t,               // position
                                          -g * t + mps)                            // velocity

    val dyn = new DynamicEq (f)
    val n   = 100
    val te  = 12.0
    val t   = VectorD.range (0, n) * (te / n.toDouble)
    val trj = dyn.trajectory (0.0, te, n)
    new Plot (t, trj(?, 0), trj(?, 1), "Plot (x_t0, x_t1) vs. t")

end dynamicEqTest

