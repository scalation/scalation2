
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Sep 26 15:00:24 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Example Model: Poisson for Event Graph Simulation
 */

package scalation
package simulation
package event
package example_1

import scalation.mathstat.Statistic
import scalation.random.Exponential

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runPoisson2` function is used to launch the `PoissonModel2` class.
 *  It is the same as `runPoisson`, except that causal links are added to enable
 *  the model to be animated as an Event Graph.
 *  > runMain scalation.simulation.event.example_1.runPoisson2
 */
@main def runPoisson2 (): Unit =  new PoissonModel2 ()

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PoissonModel` models the detection of particles (e.g., gamma rays)
 *  from the decay of radioactive atoms as a Poisson Process.
 *  @see http://stuff.mit.edu/afs/sipb/user/biyeun/Public/8.13/poisson/poisson_statistics_biyeun.pdf
 *  @param name    the name of the simulation model
 *  @param reps    the number of independent replications to run
 *  @param nStop   the number arrivals before stopping
 *  @param stream  the base random number stream (0 to 999)
 */
class PoissonModel2 (name: String = "Poisson2", reps: Int = 1, nStop: Int = 100, stream: Int = 0)
//    extends Model (name, reps, false):              // animation off
      extends Model (name, reps, true):               // animation on

    //--------------------------------------------------
    // Initialize Model Constants

    val lambda = 1.0                                  // customer arrival rate (per hr)

    //--------------------------------------------------
    // Create Random Variables (RVs)

    val iArrivalRV = Exponential (1.0 / lambda, stream)

    //--------------------------------------------------
    // Create State Variables

    var nArr = 0.0                                    // number of particles detected

    val t_ia_stat = new Statistic ("t_ia")            // time between Arrivals statistics
    addStats (t_ia_stat)

    //--------------------------------------------------
    // Create Nodes in the Event Graph (event prototypes)

    val aLoc   = Array (150.0, 200.0, 50.0, 50.0)     // Arrival event node location
    val aProto = new EventNode (this, aLoc)           // prototype for all Arrival events

    //--------------------------------------------------
    // Create Edges in the Event Graph (causal links between events)

    val aLink = Array (CausalLink ("l_A2A", this, () => nArr < nStop-1, aProto))
    aProto.displayLinks (aLink)

    //--------------------------------------------------
    // Specify Logic for each Type of Simulation Event

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Arrival` is a subclass of `Event` for handling arrival events.
     *  @param call   the entity that arrives, in this case a phone call
     *  @param delay  the time delay for this event's occurrence
     */
    case class Arrival (call: Entity, delay: Double)
         extends Event (call, this, delay, t_ia_stat, aProto):

        def occur (): Unit =
            if aLink(0).condition () then
                val toArrive = Entity (iArrivalRV.gen, 0.0, PoissonModel2.this)
                schedule (Arrival (toArrive, toArrive.iArrivalT))
            end if
            nArr += 1                                      // update the current state
        end occur

    end Arrival

    //--------------------------------------------------
    // Start the simulation after scheduling the first priming event

    val firstArrival = Entity (iArrivalRV.gen, 0.0, this)
    schedule (Arrival (firstArrival, firstArrival.iArrivalT))     // first priming event
    simulate ()                                                   // start simulating

    Thread.sleep (10000)                                          // wait on animation trace to finish
    report (("nArr", nArr))
    reportStats ()

end PoissonModel2

