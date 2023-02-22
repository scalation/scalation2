
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Dec 27 15:41:58 EST 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   M/M/1 Queue Simulation Model using tableau Package  
 */

package scalation
package simulation
package tableau

import scalation.random.Known
import scalation.random.RandomSeeds.N_STREAMS

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runQueue_MM1` function defines a simple tableau model of a bank where service is
 *  provided by one teller and models an M/M/1 queue.  In this case, the default simulate
 *  method in `scalation.simulation.tableau.Model` is sufficient and need not be overridden.
 *  Note, the Known random variate generator simply repeats the given sequence.
 *  > runMain scalation.simulation.tableau.runQueue_MM1
 */
@main def runQueue_MM1 (): Unit =

    val iArrivalArr = Array [Double] (6, 3, 5, 4, 3, 8, 5, 7, 9, 6)
    val serviceArr  = Array [Double] (5, 6, 4, 3, 5, 7, 2, 4, 5, 8)

    val stream     = 0                                             // random number stream  (0 to 999)
    val maxCusts   = 10                                            // stopping rule: at maxCusts

    val iArrivalRV = Known (iArrivalArr, stream)                   // inter-arrival time random var
    val serviceRV  = Known (serviceArr, (stream + 1) % N_STREAMS)  // service time random variate

    // Run the simulation of the model `Queue_MM1`.

    val mm1 = new Model ("Queue_MM1", maxCusts, Array (iArrivalRV, serviceRV))
    mm1.simulate ()
    mm1.report ()
    mm1.summary ()
    Model.occupancy (mm1.timeLine ())

end runQueue_MM1

