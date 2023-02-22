
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Dec 27 15:41:58 EST 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Example Model: Bank - Tableau Simulation 
 */

package scalation
package simulation
package tableau

import scalation.random.Exponential
import scalation.random.RandomSeeds.N_STREAMS

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runEx_Bank` function defines a simple tableau model of a bank where service is
 *  provided by one teller and models an M/M/1 queue.  In this case, the default
 *  simulate method in `scalation.simulation.tableau.Model` is sufficient and need
 *  not be overridden.
 *  > runMain scalation.simulation.tableau.runEx_Bank
 */
@main def runEx_Bank (): Unit =

    val stream   = 0                                      // random number stream (0 to 999)
    val lambda   = 6.0                                    // customer arrival rate (per hr)
    val mu       = 7.5                                    // customer service rate (per hr)
    val maxCusts = 100                                    // stopping rule: at maxCusts

    val iArrivalRV = Exponential (HOUR / lambda, stream)
    val serviceRV  = Exponential (HOUR / mu, (stream + 1) % N_STREAMS)

    // Run the simulation of the model `Bank`.

    val mm1 = new Model ("Bank", maxCusts, Array (iArrivalRV, serviceRV))
    mm1.simulate ()
    mm1.report ()

end runEx_Bank

