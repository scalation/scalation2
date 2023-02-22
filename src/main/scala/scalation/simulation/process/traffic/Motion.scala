
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Casey Bowman
 *  @version 2.0
 *  @date    Fri Feb 19 08:58:42 EST 2021
 *  @see     LICENSE (MIT style license file).
 */

package scalation
package simulation
package process
package traffic

import scala.math.min

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Motion` object provides several options for car following models for
 *  vehicles to use to move along roads.
 */
object Motion:

    val FREERANGE = 50.0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the velocity of the vehicle based on Gipps' model.
     *  @param an  the max acceleration of driver n
     *  @param bn  the max deceleration of driver n (negative #)
     *  @param sp  the size of the predecessor's vehicle
     *  @param Vn  the desired velocity of driver n
     *  @param xn  the current position of driver n
     *  @param vn  the current velocity of driver n
     *  @param xp  the current position of the predecessor
     *  @param vp  the current velocity of the predecessor
     *  @param τ   the reaction time of driver n
     */
    def gipps (an: Double, bn: Double, sp: Double, Vn: Double, xn: Double,
               vn: Double, xp: Double, vp: Double, τ: Double): Double =
        val free = vn * 2.5 * an * τ * (1.0 - vn / Vn) * Math.sqrt (0.025 + vn / Vn)
        val cong = bn * τ + Math.sqrt (bn * bn * τ * τ - bn * (2 * (xp - sp - xn) - vn * τ - vp * vp / bn))
        min (free, cong)
    end gipps

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the velocity of the vehicle based on Gipps' model for a vehicle and its predecessor.
     *  @param cn  the current vehicle
     *  @param cp  the predecessor of the current vehicle
     */
    def gipps (cn: Vehicle, cp: Vehicle): Double =
        if cp == null then
            gipps (cn.amax, cn.bmax, cn.len, cn.vmax, cn.t_disp, cn.velocity, cn.t_disp + 1000, cn.vmax, cn.τ)
        else 
            gipps (cn.amax, cn.bmax, cp.len, cn.vmax, cn.t_disp, cn.velocity, cp.t_disp, cp.velocity, cn.τ)
    end gipps

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the acceleration of the vehicle based on the Intelligent Driver Model.
     *  @param an ...
     */
    def iDM (an: Double, bn: Double, sp: Double, Vn: Double, xn: Double, vn: Double,
             xp: Double, vp: Double, T: Double, s0: Double, δ: Double): Double =
        val Δx = xp - xn - sp
        val Δv = vn - vp
        val ss = s0 + vn * T + (vn * Δv) / (2.0 * Math.sqrt (an * bn))
        an * (1.0 - (vn / Vn) ~^ δ - (ss / Δx) ~^ 2.0)
    end iDM

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the acceleration of the vehicle based on the Intelligent Driver Model
     *  when there is no predecessor.
     */
    def iDMFree (an: Double, vn: Double, Vn: Double, δ: Double = 4.0): Double =
        an * (1.0 - (vn / Vn) ~^ δ)
    end iDMFree

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the acceleration of the vehicle based on the Intelligent Driver Model
     *  for a vehicle and its predecessor.
     *  @param cn  the current vehicle
     *  @param cp  the predecessor of the current vehicle
     */
    def iDM (cn: Vehicle, cp: Vehicle, δ: Double = 4.0): Double =
        if cp == null then
            iDMFree (cn.amax, cn.velocity, cn.vmax, δ)
        else if cp.t_disp - cn.t_disp > FREERANGE then
            iDMFree (cn.amax, cn.velocity, cn.vmax, δ)
        else 
            iDM (cn.amax, -cn.bmax, cp.len, cn.vmax, cn.t_disp, cn.velocity, cp.t_disp, cp.velocity, cn.T, cn.s, δ)
    end iDM

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return ...
     *  @param cn  the current vehicle
     *  @param cp  the predecessor of the current vehicle
     */
    def basic (cn: Vehicle, cp: Vehicle): Double =
        if cp == null then
            1.0 - (cn.velocity / cn.vmax)
        else 
            val dx = cp.t_disp - cn.t_disp
            val dv = cp.velocity - cn.velocity
            if dx > FREERANGE then 1.0 - (cn.velocity / cn.vmax) else 0.0
        end if
    end basic

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Butcher's Method (fifth order) for numerically solving an ordinary differential equation.
     *  @param Ft    the "original" function value at time t
     *  @param ft    the "derivative" function value at time t
     *  @param ft_τ  the "derivative" function value at time t - τ
     *  @param τ     the time difference (reaction time)
     *
     *  FIX - integrate into Dynamics package
     */
    def butcher (Ft: Double, ft: Double, ft_τ: Double, τ: Double): Double =
        val _1_by_9 = 1.0 / 9.0
        val k1 = ft_τ
        val k3 = ft_τ + 0.25 * (ft - ft_τ)
        val k4 = ft_τ + 0.50 * (ft - ft_τ)
        val k5 = ft_τ + 0.75 * (ft - ft_τ)
        val k6 = ft
        Ft + _1_by_9 * (7 * k1 + 32 * k3 + 12 * k4 + 32 * k5 + 7 * k6) * τ
    end butcher

end Motion

