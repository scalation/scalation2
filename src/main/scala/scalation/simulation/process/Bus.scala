
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Oct 20 14:54:48 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   A Bus Is Used to Transport Several SimActors Together
 */

package scalation
package simulation
package process

import scala.util.control.Breaks.{break, breakable}

import scalation.random.Variate

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Bus` class is used to collect multiple actors together for transporting.
 *  The act method must be specified in each subclass where the bus route is specified.
 *  @param name      the name of this bus
 *  @param director  the director controlling the model
 *  @param lTime     the loading/unloading time
 *  @param cap       the capacity of this bus
 */
abstract class Bus (name: String, director: Model, lTime: Variate, cap: Int)
     extends SimActor ("b", director):

    private val rider   = Array.ofDim [SimActor] (cap)           // seats on this bus 
    private var nRiders = 0                                      // current number of riders

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load actors/riders waiting for this bus in queue q.
     *  @param que  the wait queue where actors are waiting for the bus
     */
    def load (que: WaitQueue): Unit =
        breakable {
            for i <- 0 to cap if rider(i) == null do             // find next open seat
                if que.isEmpty then break ()                     // break when queue empties
                rider(i) = que.dequeue ()                        // rider from queue takes seat i
                nRiders += 1                                     // increment the number of riders
            end for
        } // breakable
    end load

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Unload actors/riders from this bus onto transport t.
     *  @param tran      the transport for actors departing this bus
     */
    def unload (tran: Transport): Unit =
        for i <- 0 until nRiders do
            val r_i = rider(i)                                   // consider the i-th rider
            if r_i.nextTransport == tran then                    // rider i wants to exit
                r_i.schedule (i)                                 // FIX - use longer delay
                rider(i) = null                                  // open seat i
                nRiders -= 1                                     // decrement the number of riders
            end if
        end for
    end unload

end Bus

