
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Dec 27 15:41:58 EST 2013
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Example Model: Call Center - Tableau Simulation
 */

package scalation
package simulation
package tableau

import scalation.random.{Exponential, Variate}
import scalation.random.RandomSeeds.N_STREAMS

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runEx_CallCenter` function defines a simple tableau model of a Call Center where
 *  service is provided by one tele-service representative and models an M/M/1/1
 *  queue (i.e., no call waiting).  The default 'simulate' method provided by
 *  `scalation.simulation.tableau.Model` won't suffice and must be overridden in the
 *  `CallCenterModel` class.
 *  > runMain scalation.simulation.tableau.runEx_CallCenter
 */
@main def runEx_CallCenter (): Unit =

    val stream     = 0                                     // random number stream (0 to 999)
    val lambda     = 6.0                                   // customer arrival rate (per hr)
    val mu         = 7.5                                   // customer service rate (per hr)
    val maxCalls   = 100                                   // stopping rule: at maxCalls

    val iArrivalRV = Exponential (HOUR / lambda, stream)
    val serviceRV  = Exponential (HOUR / mu, (stream + 1) % N_STREAMS)

    // Run the simulation of the `CallCenterModel`.

    val mm11 = new CallCenterModel ("CallCenter", maxCalls, Array (iArrivalRV, serviceRV))
    mm11.simulate ()
    mm11.report ()
    mm11.summary ()
    mm11.save ()

end runEx_CallCenter


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CallCenterModel` class customizes `scalation.simulation.tableau.Model` for
 *  Call Center simulations by overriding the 'simulate' method.
 *  @param name   the name of simulation model
 *  @param m      the number entities to process before stopping
 *  @param rv     the random variate generators to use
 *  @param label  the column labels for the matrix
 */
class CallCenterModel (name: String, m: Int, rv: Array [Variate])
      extends Model (name, m, rv):

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Perform tableau-based simulation by recording timing information about
     *  the i-th entity in the i-th row of the matrix.
     *  @param startTime  the start time for the simulation
     */
    override def simulate (startTime: Double = 0): Unit =
        var l = 0                                          // last established call
        for i <- 1 to m do
            tab(i, 1)  = rv(0).gen                         // IArrival-1
            tab(i, 2)  = tab(i-1, 2) + tab(i, 1)           // Arrival-2
            if tab(l, 6) <= tab(i, 2) then                 // call established
                tab(i, 3) = tab(i, 2); l = i               // Begin-3
                tab(i, 4) = tab(i, 3) - tab(i, 2)          // Wait-4
                tab(i, 5) = rv(1).gen                      // Service-5
                tab(i, 6) = tab(i, 3) + tab(i, 5)          // Departure-6
                tab(i, 7) = tab(i, 6) - tab(i, 2)          // Total-7
            end if
        end for
    end simulate

end CallCenterModel

