

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 2.0
 *  @date    Tue Mar 12 21:43:42 EDT 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Records the Flow of Actors/Vehicles (Counts and Speed)
 */

package scalation
package simulation
package process

import scala.math.floor

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Recorder` trait allows Nodes to easily record the flow of actors/entities
 *  (e.g., vehicles) in terms of counts and optionally average speed.
 *  @param nt  the number of time intervals
 */
trait Recorder (nt: Int = 200):

    private val timeConv = 86400.0 / nt                                 // 50 * 60 * 24 = 86400 seconds per day

    protected val r_counts = Array.ofDim [Int] (nt)                     // record counts in time interval
    protected val r_speeds = Array.ofDim [Double] (nt)                  // record average speed in time interval

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Record the entity and optionally its speed.
     *  @param ctime  the clock time the entity entered the component (e.g., Sink)
     *  @param speed  the speed at which entity entered the component (e.g., Sink)
     */
    def record (actor: SimActor, ctime: Double): Unit =
        val i = floor (ctime / timeConv).toInt
        val cnt = r_counts(i) + 1
        r_counts(i) = cnt
        if actor.isInstanceOf [Vehicle] then
            val speed = actor.asInstanceOf [Vehicle].velocity
            r_speeds(i)  = (r_speeds(i) * (cnt - 1) + speed) / cnt
    end record

end Recorder

