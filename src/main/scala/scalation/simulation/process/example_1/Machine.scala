
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Sep 26 15:00:24 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     simulation/event/example_1/Machine.scala
 *
 *  @note    Example Model: Machine Shop for Process-Interaction Simulation
 */

package scalation
package simulation
package process
package example_1                                       // One-Shot

import scalation.random.{Exponential, Uniform}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runMachine` is used to launch the `MachineModel` class.
 *  > runMain scalation.simulation.process.example_1.runMachine
 */
@main def runMachine (): Unit = new MachineModel ()


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MachineModel` class defines a process-interaction model of a simple
 *  two-stage manufacturing process (machine shop).
 *  Tandem M/M/1/4 Queues (queue capacity = 4-1 = 3).
 *  @param name       the name of the simulation model
 *  @param reps       the number of independent replications to run
 *  @param animating  whether to animate the model
 *  @param aniRatio   the ratio of simulation speed vs. animation speed
 *  @param nStop      the number arrivals before stopping
 *  @param stream     the base random number stream (0 to 999)
 */
class MachineModel (name: String = "Machine", reps: Int = 1, animating: Boolean = true,
                    aniRatio: Double = 8.0, nStop: Int = 100, stream: Int = 0)
      extends Model (name, reps, animating, aniRatio):

    //--------------------------------------------------
    // Initialize Model Constants

    val lambda = 6.0                                    // part arrival rate (per hour)
    val mu     = 7.5                                    // part service rate (per hour)
    val nUnits = 1                                      // the number of machines at each stage/station
    val qCap   = 3                                      // the capacity of the queues for holding parts

    //--------------------------------------------------
    // Create Random Variables (RVs)

    val iArrivalRV = Exponential (HOUR / lambda, stream)    // use different random number streams for independence
    val serviceRV  = Exponential (HOUR / mu, stream + 1)
    val moveRV     = Uniform (30*MINUTE-10, 30*MINUTE+10, stream + 2)

    //--------------------------------------------------
    // Create Model Components

    val entry       = Source ("entry", this, () => Part (), 0, nStop, iArrivalRV, (100, 390))
    val machine1Q   = WaitQueue ("machine1Q", (330, 390), qCap)
    val machine1    = Resource ("machine1", machine1Q, nUnits, serviceRV, (350, 385))
    val machine2Q   = WaitQueue ("machine2Q", (530, 390), qCap)
    val machine2    = Resource ("machine2", machine2Q, nUnits, serviceRV, (550, 385))
    val ship        = Sink ("ship", (800, 390))
    val scrap       = Sink ("scrap", (430, 600))
    val toMachine1Q = Transport ("toMachine1Q", entry, machine1Q, moveRV)
    val toMachine2Q = Transport ("toMachine2Q", machine1, machine2Q, moveRV)
    val toShip      = Transport ("toShip", machine2, ship, moveRV)
    val toScrap1    = Transport ("toScrap1", machine1Q, scrap, moveRV)
    val toScrap2    = Transport ("toScrap2", machine2Q, scrap, moveRV)

    addComponent (entry, machine1Q, machine1, machine2Q, machine2, ship, scrap,
                  toMachine1Q, toMachine2Q, toShip, toScrap1, toScrap2)

    //--------------------------------------------------
    // Specify Scripts for each Type of Simulation Actor

    case class Part () extends SimActor ("p", this):

        override def act (): Unit =
            toMachine1Q.move ()                          // move to queue for machine 1
            if machine1.busy then                        // when machine 1 is busy:
                if ! machine1Q.waitIn () then            // wait unless its queue is full
                   toScrap1.move ()                      // move part to scrap
                   scrap.leave ()                        // ends processing of part
                   return
            else
                machine1Q.noWait ()                      // record no waiting for machine 1
            machine1.utilize ()                          // part is processed by machine 1
            machine1.release ()                          // release machine 1

            toMachine2Q.move ()                          // move to queue for machine 2
            if machine2.busy then                        // when machine 2 is busy:
                if ! machine2Q.waitIn () then            // wait unless its queue is full
                   toScrap2.move ()                      // move part scrap
                   scrap.leave ()                        // ends processing of part
                   return
            else
                machine2Q.noWait ()                      // record no waiting for machine 2
            machine2.utilize ()                          // part id processed by machine 2
            machine2.release ()                          // release machine 2

            toShip.move ()                               // move complted part to shipping
            ship.leave ()                                // processing done => ship part
        end act

    end Part

    simulate ()
    waitFinished ()
    Model.shutdown ()

end MachineModel

