
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Sep 26 15:00:24 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Example Model: Machine Shop for Event-Scheduling Simulation
 *
 *  @see     simulation/process/example_1/Machine.scala
 */

// Partial Implementation

package scalation
package simulation
package event
package example_1

import scalation.mathstat.Statistic
import scalation.random.Exponential
import scalation.random.RandomSeeds.N_STREAMS

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runMachine` function is used to launch the `MachineModel` class.
 *  > runMain scalation.simulation.event.example_1.runMachine
 */
@main def runMachine (): Unit = new MachineModel () 


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MachineModel` class defines a simple Event-Scheduling model of a simple
 *  two-stage manufacturing process (machine shop).
 *  Tandem M/M/1/4 Queues (queue only capacity = 4-1 = 3).
 *  @param name    the name of the simulation model
 *  @param reps    the number of independent replications to run
 *  @param nStop   the number arrivals before stopping
 *  @param stream  the base random number stream (0 to 999)
 */
class MachineModel (name: String = "Machine", reps: Int = 1, nStop: Int = 100, stream: Int = 0)
      extends Model (name, reps):

    //--------------------------------------------------
    // Initialize Model Constants

    val lambda = 10.0                                 // part arrival rate (per hr)
    val mu1    = 12.0                                 // machine 1 part service rate (per hr)
    val mu2    = 15.0                                 // machine 2 part service rate (per hr)
    val nUnits = 1                                    // the number of machines at each stage/station
    val qCap   = 3                                    // the capacity of each queue for holding parts

    //--------------------------------------------------
    // Create Random Variables (RVs)

    val iArrivalRV = Exponential (HOUR / lambda, stream)
    val service1RV = Exponential (HOUR / mu1, (stream + 1) % N_STREAMS)
    val service2RV = Exponential (HOUR / mu2, (stream + 2) % N_STREAMS)

    //--------------------------------------------------
    // Create State Variables

    var nArr      = 0                                 // number of parts that have arrived at the machine shop
    var nIn1      = 0                                 // number of parts at machine 1 
    var nIn2      = 0                                 // number of parts at machine 2
    var nOut      = 0                                 // number of parts that finished & shipped
    var nScrap1   = 0                                 // number of scrapped raw parts
    var nScrap2   = 0                                 // number of scrapped partially finished parts

    val t_ia_stat = new Statistic ("t_ia")            // time between Arrivals statistics
    val t_s1_stat = new Statistic ("t_s")             // time in Service for machine 1 statistics
    val t_s2_stat = new Statistic ("t_s")             // time in Service for machine 2 statistics
    val machine1Q = WaitQueue (this, "Q1", qCap)      // waiting queue for machine 1 that collects stats
    val machine2Q = WaitQueue (this, "Q2", qCap)      // waiting queue for machine 2 that collects stats
    addStats (t_ia_stat, t_s1_stat, t_s2_stat)

    //--------------------------------------------------
    // Specify Logic for each Type of Simulation Event

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Arrival` is a subclass of `Event` for handling arrival events.
     *  The 'occur' method triggers future events and updates the current state.
     *  @param part   the entity that arrives at machine 1, in this case a part to be machined
     *  @param delay  the time delay for this event's occurrence
     */
    case class Arrival (part: Entity, delay: Double)
         extends Event (part, this, delay, t_ia_stat):

        def occur (): Unit =
            var scrapped = false
            if nArr < nStop - 1 then
                val toArrive = Entity (iArrivalRV.gen, service1RV.gen, MachineModel.this)
                schedule (Arrival (toArrive, toArrive.iArrivalT))
            end if
            if nIn1 == 0 then                                     // machine 1 is available
                schedule (FinishMachine1 (part, part.serviceT))
            else   
                if machine1Q.isFull then scrapped = true          // no room => move part to scrap
                else machine1Q.enqueue (part)                     // wait for machine 1
            end if
            nArr += 1                                             // update the current state
            if scrapped then nScrap1 += 1 else nIn1 += 1
        end occur

    end Arrival

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `FinishMachine1` is a subclass of `Event` for handling departure events.
     *  The 'occur' method triggers future events and updates the current state.
     *  @param part   the entity that departs, in this case a part to be machined
     *  @param delay  the time delay for this event's occurrence
     */
    case class FinishMachine1 (part: Entity, delay: Double)
         extends Event (part, this, delay, t_s1_stat):

        def occur (): Unit = ???

    end FinishMachine1

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `FinishMachine2` is a subclass of `Event` for handling departure events.
     *  The 'occur' method triggers future events and updates the current state.
     *  @param part   the entity that departs, in this case a part to be machined
     *  @param delay  the time delay for this event's occurrence
     */
    case class FinishMachine2 (part: Entity, delay: Double)
         extends Event (part, this, delay, t_s2_stat):

        def occur (): Unit = ???

    end FinishMachine2

    //--------------------------------------------------
    // Start the simulation after scheduling the first priming event

    val firstArrival = Entity (iArrivalRV.gen, service1RV.gen, this)
    schedule (Arrival (firstArrival, firstArrival.iArrivalT))     // first priming event
    simulate ()                                                   // start simulating

    report (("nArr", nArr), ("nIn1", nIn1), ("nIn2", nIn2), ("nOut", nOut),
            ("nScrap1", nScrap1), ("nScrap2", nScrap2))
    reportStats ()
    machine1Q.summary (nOut)
    machine2Q.summary (nOut)

end MachineModel

