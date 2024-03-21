
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Sep 26 15:00:24 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Example Model: Call Center for Event Scheduling Simulation
 */

package scalation
package simulation
package event
package example_1

import scalation.mathstat.Statistic
import scalation.random.Exponential
import scalation.random.RandomSeeds.N_STREAMS

import queueingnet.MMck_Queue

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runCallCenter` function is used to launch the `CallCenterModel` class. 
 *  > runMain scalation.simulation.event.example_1.runCallCenter
 */
@main def runCallCenter (): Unit = new CallCenterModel ()


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CallCenterModel` class defines a simple Event Graph model of a
 *  Call Center where service is provided by one tele-service representative and
 *  models an M/M/1/1 queue.
 *  @param name    the name of the simulation model
 *  @param reps    the number of independent replications to run
 *  @param nStop   the number arrivals before stopping
 *  @param stream  the base random number stream (0 to 999)
 */
class CallCenterModel (name: String = "CallCenter", reps: Int = 1, nStop: Int = 100, stream: Int = 0)
      extends Model (name, reps):                     // animation on

    //--------------------------------------------------
    // Initialize Model Constants

    val lambda = 6.0                                  // call arrival rate (per hour)
    val mu     = 7.5                                  // call service rate (per hour)

    //--------------------------------------------------
    // Create Random Variables (RVs)

    val iArrivalRV = Exponential (HOUR / lambda, stream)
    val serviceRV  = Exponential (HOUR / mu, (stream + 1) % N_STREAMS)

    //--------------------------------------------------
    // Create State Variables

    var nArr  = 0.0                                   // number of calls that have arrived
    var nIn   = 0.0                                   // number of calls in progress
    var nOut  = 0.0                                   // number of calls that finished and hung up
    var nLost = 0.0                                   // number of calls dropped

    val t_ia_stat = new Statistic ("t_ia")            // time between Arrivals statistics
    val t_s_stat  = new Statistic ("t_s")             // time in Service statistics
    addStats (t_ia_stat, t_s_stat)

    //--------------------------------------------------
    // Specify Logic for each Type of Simulation Event

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Arrival` is a subclass of `Event` for handling arrival events Make-a-Call.
     *  @param call   the entity that arrives, in this case a phone call
     *  @param delay  the time delay for this event's occurrence
     */
    case class Arrival (call: Entity, delay: Double)
         extends Event (call, this, delay, t_ia_stat):

        def occur (): Unit =
            if nArr < nStop - 1 then
                val toArrive = Entity (iArrivalRV.gen, serviceRV.gen, CallCenterModel.this)
                schedule (Arrival (toArrive, toArrive.iArrivalT))
            end if
            if nIn == 0 then
                schedule (Departure (call, call.serviceT))
            end if
            nArr += 1                                            // update the current state
            if nIn == 1 then nLost += 1 else nIn = 1
        end occur

    end Arrival

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Departure is a subclass of `Event` for handling departure events 'HangUp'.
     *  @param call   the entity that arrives, in this case a phone call
     *  @param delay  the time delay for this event's occurrence
     */
    case class Departure (call: Entity, delay: Double)
         extends Event (call, this, delay, t_s_stat):

        def occur (): Unit =
            leave (call)                                         // collects time in sYstem statistics
            nIn   = 0                                            // update the current state
            nOut += 1
        end occur

    end Departure

    //--------------------------------------------------
    // Start the simulation after scheduling the first priming event

    val firstArrival = Entity (iArrivalRV.gen, serviceRV.gen, this)
    schedule (Arrival (firstArrival, firstArrival.iArrivalT))    // first priming event
    simulate ()                                                  // start simulating

    report (("nArr", nArr), ("nIn", nIn), ("nLost", nLost), ("nOut", nOut))
    reportStats ()

    //--------------------------------------------------
    // Verify the results using an M/M/1/1 Queueing Model

    println ("\nVerification ...")
    val mm11 = new MMck_Queue (lambda/HOUR, mu/HOUR)
    mm11.view ()
    mm11.report ()

end CallCenterModel

