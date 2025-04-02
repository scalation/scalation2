
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 2.0
 *  @date    Sun Sep 13 20:37:41 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model: Kalman Filter
 *
 *  @see web.mit.edu/kirtley/kirtley/binlustuff/literature/control/Kalman%20filter.pdf
 *  @see en.wikipedia.org/wiki/Kalman_filter
 */

package scalation
package modeling
package forecasting_old

import scalation.mathstat._
import scalation.random.NormalVec

// FIX: needs more thorough testing and estimation for matrices

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KalmanFilter` class is used to fit state-space models.
 *     x_t = F x_t-1 + G u_t + w_t   (State Equation)
 *     z_t = H x_t + v_t             (Observation/Measurement Equation)
 *  @param x0  the initial state vector
 *  @param ff  the state transition matrix (F)
 *  @param hh  the observation matrix (H)
 *  @param qq  the process noise covariance matrix (Q)
 *  @param rr  the observation noise covariance matrix (R)
 *  @param gg  the optional control-input matrix (G)
 *  @param u   the optional control vector
 */
class KalmanFilter (x0: VectorD, ff: MatrixD, hh: MatrixD, qq: MatrixD, rr: MatrixD,
                    gg: MatrixD = null, u: VectorD = null):

    private val MAX_ITER = 20                             // maximum number of iterations
    private val doPlot   = true                           // flag for drawing plot
    private val n        = ff.dim                         // dimension of the state vector
    private val _0       = VectorD (n)                    // vector of 0's
    private val ii       = MatrixD.eye (n, n)             // identity matrix
    private val fft      = ff.transpose                   // transpose of ff
    private val hht      = hh.transpose                   // transpose of hh
    private var x        = x0                             // the state estimate
    private var pp       = new MatrixD (n, n)             // the covariance estimate

    val traj = if doPlot then new MatrixD (MAX_ITER, n+1) else new MatrixD (0, 0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Predict the state of the process at the next time point.
     */
    def predict (): Unit =
        x  = ff * x                                       // new predicted state
        if u != null && gg != null then x += gg * u       // if using control
        pp = ff * pp * fft + qq                           // new predicted covariance
    end predict

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the state and covariance estimates with the current and possibly
     *  noisy measurements
     *  @param z  current measurement/observation of the state
     */
    def update (z: VectorD): Unit =
        val y  = z - hh * x                               // measurement residual
        val ss = hh * pp * hht + rr                       // residual covariance
        val kk = pp * hht * ss.inverse                    // optimal Kalman gain
        x  = x + kk * y                                   // updated state estimate
        pp = (ii - kk * hh) * pp                          // updated covariance estimate
    end update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Iteratively solve for x using predict and update phases.
     *  @param dt  the time increment (delta t)
     *  @param u   the control vector
     */
    def solve (dt: Double, u: VectorD = null): VectorD =
        var t  = 0.0                                      // initial time

        for k <- 0 until MAX_ITER do

            t += dt                                       // advance time
            if doPlot then traj(k) = x :+ t               // add current time t, state x to trajectory

            // predict
            predict ()                                    // estimate new state x and covariance pp

            // update
            val v  = NormalVec (_0, rr).gen               // observation noise - FIX - should work in trait
            val z  = hh * x + v                           // new observation

            update (z)
        end for
        x
    end solve

end KalmanFilter


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `kalmanFilterTest` main function is used to test the `KalmanFilter` class.
 *  @see en.wikipedia.org/wiki/Kalman_filter
 *  > runMain scalation.modeling.forecasting.kalmanFilterTest
 */
@main def kalmanFilterTest (): Unit =

    banner ("KalmanFilterTest")

    val dt    = 0.1                                       // time increment (delta t)
    val var_a = 0.5                                       // variance of uncontrolled acceleration a
    val var_z = 0.5                                       // variance from observation noise

    val ff = MatrixD ((2, 2), 1.0, dt,                    // transition matrix
                              0.0, 1.0)

    val hh = MatrixD ((1, 2), 1.0, 0.0)

    val qq = MatrixD ((2, 2), dt~^4/4, dt~^3/2,
                              dt~^3/2, dt~^2) * var_a

    val rr = MatrixD ((1, 1), var_z)

    val x0 = VectorD (0.0, 0.0)

    val kf = new KalmanFilter (x0, ff, hh, qq, rr)

    println ("solve = " + kf.solve (dt))
    println ("traj  = " + kf.traj)

    new Plot (kf.traj(?, 2), kf.traj(?, 0), kf.traj(?, 1))

end kalmanFilterTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `kalmanFilterTest2` main function is used to test the `KalmanFilter` class.
 *  @see https://faculty.washington.edu/ezivot/econ584/notes/statespacemodels.pdf
 *  > runMain scalation.modeling.forecasting.kalmanFilterTest2
 *
@main def kalmanFilterTest2 (): Unit =

    banner ("KalmanFilterTest: AR(2)")

    val dt    = 0.1                                       // time increment (delta t)
    val var_a = 0.5                                       // variance of uncontrolled acceleration a
    val var_z = 0.5                                       // variance from observation noise

    val ff = MatrixD ((2, 2), phi1, phi2,                 // transition matrix
                              1.0,  0.0)

    val hh = MatrixD ((1, 2), 1.0, 0.0)

    val qq = MatrixD ((2, 2), dt~^4/4, dt~^3/2,
                              dt~^3/2, dt~^2) * var_a

    val rr = MatrixD ((1, 1), var_z)

    val x0 = VectorD (0.0, 0.0)

    val kf = new KalmanFilter (x0, ff, hh, qq, rr)

    println ("solve = " + kf.solve (dt))
    println ("traj  = " + kf.traj)

    new Plot (kf.traj(?, 2), kf.traj(?, 0), kf.traj(?, 1))

end kalmanFilterTest2
 */

