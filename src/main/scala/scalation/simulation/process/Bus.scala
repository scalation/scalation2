
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Oct 20 14:54:48 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    A Bus Is Used to Transport Several SimActors Together
 */

package scalation
package simulation
package process

import scala.runtime.ScalaRunTime.stringOf
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
         extends SimActor ("bus_$name", director):

    private val debug = debugf ("Resource", true)                  // debug function

    private val rider   = Array.ofDim [SimActor] (cap)             // seats on this bus 
    private var nRiders = 0                                        // current number of riders

    debug ("init", s"name = bus_$name, located at ${stringOf (at)}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Load actors/riders waiting for this bus in queue que.
     *  @param que  the wait queue where actors are waiting for the bus
     */
    def load (que: WaitQueue): Unit =
        val delay = lTime.gen                                      // common loading delay for all riders
        schedule (delay)                                           // bus delay for loading riders
        breakable {
            for i <- 0 to cap if rider(i) == null do               // find next open seat
                if que.isEmpty then break ()                       // break when queue empties
                rider(i) = que.dequeue ()                          // rider from queue takes seat i
                nRiders += 1                                       // increment the number of riders
            end for
        } // breakable
    end load

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Unload actors/riders from this bus onto transport tran.
     *  @param tran  the transport for actors departing this bus
     */
    def unload (tran: Transport): Unit =
        val delay = lTime.gen                                      // common unloading delay for all riders
        schedule (delay)                                           // bus delay for unloading riders
        for i <- 0 until nRiders do
            val r_i = rider(i)                                     // consider the i-th rider
            if r_i.nextTransport == tran then                      // rider i wants to exit the bus
                r_i.schedule (i)                                   // assitional delay for each rider
                rider(i) = null                                    // open seat i
                nRiders -= 1                                       // decrement the number of riders
        end for
    end unload

end Bus

