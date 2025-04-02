
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Sep 26 15:00:24 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Example Model: Traffic Loop for Process Simulation
 */

package scalation
package simulation
package process
package example_1                                       // One-Shot

import scalation.random.{Bernoulli, Sharp, Uniform}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runLoop` function is used to launch the `LoopModel` class.
 *  > runMain scalation.simulation.process.example_1.runLoop
 */
@main def runLoop (): Unit = new LoopModel ()


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LoopModel` class simulates a two-lane road in two directions, i.e., it
 *  has 2 West-bound lanes and 2 East-bound lanes.  It used a composite class called
 *  `Path`, which will have a `Transport` for each lane.
 *  @param name       the name of the simulation model
 *  @param reps       the number of independent replications to run
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed vs. animation speed
 *  @param nStop      the number arrivals before stopping
 *  @param stream     the base random number stream (0 to 999)
 */
class LoopModel (name: String = "Loop", reps: Int = 1, animating: Boolean = true,
                 aniRatio: Double = 8.0, nStop: Int = 100, stream: Int = 0)
      extends Model (name, reps, animating, aniRatio):

    //--------------------------------------------------
    // Initialize Model Constants

    val ia_time = (4000.0, 6000.0)                      // inter-arrival time range
    val mv_time = (2900.0, 3100.0)                      // move time range

    //--------------------------------------------------
    // Create Random Variables (RVs)

    val iArrivalRV = Uniform (ia_time, stream)          // use different random number streams for independence
    val moveRV     = Uniform (mv_time, stream + 1)
    val laneRV     = Bernoulli (stream = stream + 2)

    //--------------------------------------------------
    // Create Model Components

    val src1 = Source ("src1", this, () => Car1 (), 0, nStop, iArrivalRV, (400, 500))
    val src2 = Source ("src2", this, () => Car2 (), 0, nStop, iArrivalRV, (400, 420))
    val jun1 = Junction ("jun1", Sharp (10.0), (850, 475))
    val jun2 = Junction ("jun2", Sharp (10.0), (870, 475))
    val snk1 = Sink ("snk1", (400, 450))
    val snk2 = Sink ("snk2", (400, 530))

    val road1a = new Path ("road1a", 2, src1, jun1, moveRV, false, -0.8)
    val road1b = new Path ("road1b", 2, jun1, snk1, moveRV, false,  0.8)
    val road2a = new Path ("road2a", 2, src2, jun2, moveRV, false,  0.8)
    val road2b = new Path ("road2b", 2, jun2, snk2, moveRV, false, -0.8)

    addComponent (src1, src2, jun1, jun2, snk1, snk2, road1a, road1b, road2a, road2b)

    //--------------------------------------------------
    // Specify Scripts for each Type of Simulation Actor

    case class Car1 () extends SimActor ("c1", this):   // for West-bound cars
 
        override def act (): Unit =
            val i = laneRV.igen                         // pick a lane
            road1a.lane(i).move ()                      // move to junction 1 (via road1a)
            jun1.jump ()                                // go thru junction 1
            road1b.lane(i).move ()                      // move to sink 1 (via road1b)
            snk1.leave ()                               // leave via sink 1
        end act

    end Car1

    case class Car2 () extends SimActor ("c2", this):   // for East-bound cars

        override def act (): Unit =
            val i = laneRV.igen                         // pick a lane
            road2a.lane(i).move ()                      // move to junction 2 (via road2a)
            jun2.jump ()                                // go thru junction 2
            road2b.lane(i).move ()                      // move to sink 2 (via road2b)
            snk2.leave ()                               // leave via sink 2
        end act

    end Car2

    simulate ()
    waitFinished ()
    Model.shutdown ()

end LoopModel

