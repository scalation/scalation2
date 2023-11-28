
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Sep 26 15:00:24 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Example Model: Bank for Event Graph Simualtion
 */

package scalation
package simulation
package event
package example_1

import scalation.mathstat.Statistic
import scalation.random.Exponential
import scalation.random.RandomSeeds.N_STREAMS

import queueingnet.MM1_Queue

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runBank3` function is used to launch the `BankModel3` class.
 *  It is the same as `runBank`, except that causal links are added to enable
 *  the model to be animated as an Event Graph.
 *  > runMain scalation.simulation.event.example_1.runBank3
 */
@main def runBank3 (): Unit = new BankModel3 ()


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BankModel3` class defines a simple Event Graph model of a Bank where
 *  service is provided by one teller and models an M/M/1 queue.
 *  @param name    the name of the simulation model
 *  @param reps    the number of independent replications to run
 *  @param nStop   the number arrivals before stopping
 *  @param stream  the base random number stream (0 to 999)
 */
class BankModel3 (name: String = "Bank3", reps: Int = 1, nStop: Int = 100, stream: Int = 0)
//    extends Model (name, reps, false):              // animation off
      extends Model (name, reps, true):               // animation on

    //--------------------------------------------------
    // Initialize Model Constants

    val lambda = 6.0                                  // customer arrival rate (per hr)
    val mu     = 7.5                                  // customer service rate (per hr)

    //--------------------------------------------------
    // Create Random Variables (RVs)

    val iArrivalRV = Exponential (HOUR / lambda, stream)
    val serviceRV  = Exponential (HOUR / mu, (stream + 1) % N_STREAMS)

    //--------------------------------------------------
    // Create State Variables

    var nArr   = 0.0                                  // number of customers that have arrived
    var nIn    = 0.0                                  // number of customers in the bank
    var nOut   = 0.0                                  // number of customers that finished & left

    val t_ia_stat = new Statistic ("t_ia")            // time between Arrivals statistics
    val t_s_stat  = new Statistic ("t_s")             // time in Service statistics
    val waitQueue = WaitQueue (this)                  // waiting queue that collects stats
    addStats (t_ia_stat, t_s_stat)

    //--------------------------------------------------
    // Create Nodes in the Event Graph (event prototypes)

    val aLoc   = Array (150.0, 200.0, 50.0, 50.0)     // Arrival event node location
    val dLoc   = Array (450.0, 200.0, 50.0, 50.0)     // Departure event node location
    val aProto = new EventNode (this, aLoc)           // prototype for all Arrival events
    val dProto = new EventNode (this, dLoc)           // prototype for all Departure events

    //--------------------------------------------------
    // Create Edges in the Event Graph (causal links between events)

    val aLink = Array (CausalLink ("l_A2A", this, () => nArr < nStop-1, aProto),
                       CausalLink ("l_A2D", this, () => nIn == 0,       dProto))
    val dLink = Array (CausalLink ("l_D2D", this, () => nIn > 1,        dProto))
    aProto.displayLinks (aLink)
    dProto.displayLinks (dLink)

    //--------------------------------------------------
    // Specify Logic for each Type of Simulation Event

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Arrival` is a subclass of `Event` for handling arrival events.
     *  @param customer  the entity that arrives, in this case a bank customer
     *  @param delay     the time delay for this event's occurrence
     */
    case class Arrival (customer: Entity, delay: Double)
         extends Event (customer, this, delay, t_ia_stat, aProto):
 
        def occur (): Unit =
            if aLink(0).condition () then
                val toArrive = Entity (iArrivalRV.gen, serviceRV.gen, BankModel3.this)
                schedule (Arrival (toArrive, toArrive.iArrivalT))
            end if
            if aLink(1).condition () then
                schedule (Departure (customer, customer.serviceT))
            else
                waitQueue.enqueue (customer)                     // collects time in Queue statistics
            end if
            nArr += 1                                            // update the current state
            nIn  += 1
        end occur

    end Arrival

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Departure` is a subclass of `Event` for handling departure events.
     *  @param customer  the entity that arrives, in this case a bank customer
     *  @param delay     the time delay for this event's occurrence
     */
    case class Departure (customer: Entity, delay: Double)
         extends Event (customer, this, delay, t_s_stat, dProto):

        def occur (): Unit =
            leave (customer)                                     // collects time in sYstem statistics
            if dLink(0).condition () then
                val nextService = waitQueue.dequeue ()           // first customer in queue
                schedule (Departure (nextService, nextService.serviceT))
            end if
            nIn  -= 1                                            // update the current state
            nOut += 1
        end occur

    end Departure

    //--------------------------------------------------
    // Start the simulation after scheduling the first priming event

    val firstArrival = Entity (iArrivalRV.gen, serviceRV.gen, this)
    schedule (Arrival (firstArrival, firstArrival.iArrivalT))    // first priming event
    simulate ()                                                  // start simulating

    Thread.sleep (10000)                                         // wait on animation trace to finish
    report (("nArr", nArr), ("nIn", nIn), ("nOut", nOut))
    reportStats ()
    waitQueue.summary (nStop)

    //--------------------------------------------------
    // Verify the results using an M/M/c Queueing Model

    println ("\nVerification ...")
    val mm1 = new MM1_Queue (lambda / HOUR, mu / HOUR)
    mm1.view ()
    mm1.report ()

end BankModel3

