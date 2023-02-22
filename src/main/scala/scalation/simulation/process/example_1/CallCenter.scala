
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Sep 26 15:00:24 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Example Model: Call Center for Process-Interaction Simulation
 */

package scalation
package simulation
package process
package example_1                                     // One-Shot

import scalation.random.{Exponential, Uniform}
import scalation.random.RandomSeeds.N_STREAMS

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runCallCenter` function is used to launch the `CallCenterModel` class.
 *  > runMain scalation.simulation.process.example_1.runCallCenter
 */
@main def runCallCenter (): Unit = new CallCenterModel ()


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CallCenterModel` class defines a simple process-interaction model of a call
 *  center where service is provided by one or more tele-service representatives.
 *  Caveat: must add 'from' and 'to' components before transport!!
 *  @param name       the name of the simulation model
 *  @param reps       the number of independent replications to run
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed vs. animation speed
 *  @param nStop      the number arrivals before stopping
 *  @param stream     the base random number stream (0 to 999)
 */
class CallCenterModel (name: String = "CallCenter", reps: Int = 1, animating: Boolean = true,
                       aniRatio: Double = 8.0, nStop: Int = 100, stream: Int = 0)
      extends Model (name, reps, animating, aniRatio):

    //--------------------------------------------------
    // Intialize Model Constants

    val lambda  = 6.0                                 // customer arrival rate (per hour)
    val mu      = 7.5                                 // customer service rate (per hour)
    val nPhones = 1                                   // the number of Phones (servers)

    //--------------------------------------------------
    // Create Random Variables (RVs)

    val iArrivalRV = Exponential (HOUR / lambda, stream)
    val serviceRV  = Exponential (HOUR / mu, (stream + 1) % N_STREAMS)
    val moveRV     = Uniform (0.4 * MINUTE, 0.6 * MINUTE, (stream + 2) % N_STREAMS)

    //--------------------------------------------------
    // Create Model Components

    val entry    = Source ("entry", this, () => Call (), 0, nStop, iArrivalRV, (200, 290))
    val phone    = Resource ("phone", null, nPhones, serviceRV, (350, 285))
    val hangUp   = Sink ("hangUp", (500, 290))
    val drop     = Sink ("drop", (360, 440))
    val toPhone  = Transport ("toPhone", entry, phone, moveRV)
    val toHangUp = Transport ("toHangUp", phone, hangUp, moveRV)
    val toDrop   = Transport ("toDrop", phone, drop, moveRV)

    addComponent (entry, phone, hangUp, drop, toPhone, toHangUp, toDrop)

    //--------------------------------------------------
    // Specify Scripts for each Type of Simulation Actor

    case class Call () extends SimActor ("c", this):

        def act (): Unit =
            toPhone.jump ()
            if phone.busy then
                toDrop.jump ()
                drop.leave ()
            else
                phone.utilize ()
                phone.release ()
                toHangUp.jump ()
                hangUp.leave ()
            end if
        end act

    end Call

    simulate ()
    waitFinished ()
    Model.shutdown ()

end CallCenterModel

