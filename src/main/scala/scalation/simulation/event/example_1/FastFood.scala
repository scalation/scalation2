
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Sep 26 15:00:24 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Example Model: Fast Food for Event-Scheduling Simulation
 */

package scalation
package simulation
package event
package example_1

import scalation.mathstat.Statistic
import scalation.random.Known
//import scalation.random.RandomSeeds.N_STREAMS

import queueingnet.MMck_Queue

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `runFastFood` function is used to launch the `FastFoodModel` class.
 *  > runMain scalation.simulation.event.example_1.runFastFood
 */
@main def runFastFood (): Unit = new FastFoodModel (nStop = 20, nUnits = 3) 


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FastFoodModel` class defines a simple Event-Scheduling model of a FastFood
 *  restaurant where service is provided by nUnits servers and models an M/M/c/k queue.
 *  It estimates the restaurant profit based on the number servers.
 *  @param name    the name of the simulation model
 *  @param reps    the number of independent replications to run
 *  @param nStop   the number arrivals before stopping
 *  @param nUnits  the number of servers
 *  @param stream  the base random number stream (0 to 999)
 */
class FastFoodModel (name: String = "FastFood", reps: Int = 1, nStop: Int = 100, nUnits: Int = 2, stream: Int = 0)
      extends Model (name, reps):

    //--------------------------------------------------
    // Initialize Model Constants

    val lambda = 75.0                                 // customer arrival rate (per hr)
    val mu     = 30.0                                 // customer service rate (per hr)
    val cap    = 3                                    // queue capacity

    //--------------------------------------------------
    // Create Random Variables (RVs)

//  import scalation.random.Exponential
//  val iArrivalRV = Exponential (HOUR / lambda, stream)
//  val serviceRV  = Exponential (HOUR / mu, (stream + 1) % N_STREAMS)

    val iArrivalRV = Known (Array(0.089051585232241,
                            0.237565690240952,
                            0.484150846121557,
                            0.654041041301971,
                            0.201172764093047,
                            0.450232523603984,
                            0.209096124852582,
                            0.120031306275928,
                            0.0459964992566533,
                            3.36145426400551,
                            0.26557991200433,
                            0.94722440369138,
                            0.371821036157681,
                            0.249557428450418,
                            2.14371620459574,
                            0.756630580043188,
                            0.776278792733385,
                            1.2964705991507,
                            1.00127071932171,
                            1.60738429476461))
    val serviceRV  = Known (Array(0.600970810310147,
                            1.00705128383432,
                            0.777487760767155,
                            0.457699587760096,
                            0.73874898120453,
                            0.761732703655817,
                            1.29893919243397,
                            1.71795400483708,
                            3.69663408516588,
                            1.73978759640144,
                            5.15694562455652,
                            3.779275429934,
                            0.131862276286972,
                            2.33611429608166,
                            1.59342942424436,
                            1.11111111111111,
                            2.80129975243755,
                            0.0640026731810368,
                            3.75377193652216,
                            10.4008888496292))

    //--------------------------------------------------
    // Create State Variables

    var nArr      = 0.0                               // number of customers that have arrived
    var nIn       = 0.0                               // number of customers in the restaurant
    var nLost     = 0.0                               // number of customers lost
    var nOut      = 0.0                               // number of completed customers 

    val t_ia_stat = new Statistic ("t_ia")            // time between Arrivals statistics
    val t_s_stat  = new Statistic ("t_s")             // time in Service statistics
    val waitQueue = WaitQueue (this, cap = cap)       // waiting queue that collects stats
    addStats (t_ia_stat, t_s_stat)

    //--------------------------------------------------
    // Specify Logic for each Type of Simulation Event

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Arrival` is a subclass of `Event` for handling arrival events.
     *  The 'occur' method triggers future events and updates the current state.
     *  @param customer  the entity that arrives, in this case a restaurant customer
     *  @param delay     the time delay for this event's occurrence
     */
    case class Arrival (customer: Entity, delay: Double)
         extends Event (customer, this, delay, t_ia_stat):

        def occur (): Unit =
            var lost = false
            if nArr < nStop - 1 then
                val toArrive = Entity (iArrivalRV.gen, serviceRV.gen, FastFoodModel.this)
                schedule (Arrival (toArrive, toArrive.iArrivalT))
            end if
            if nIn < nUnits then
                schedule (Departure (customer, customer.serviceT))
            else if waitQueue.isFull then
                lost = true                                   // no place for customer
            else
                waitQueue.enqueue (customer)                  // wait in the queue
            end if
            nArr += 1                                         // update the current state
            if lost then nLost += 1
            else nIn += 1
        end occur

    end Arrival

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** `Departure` is a subclass of `Event` for handling departure events.
     *  The 'occur' method triggers future events and updates the current state.
     *  @param customer  the entity that departs, in this case a restaurant customer
     *  @param delay     the time delay for this event's occurrence
     */
    case class Departure (customer: Entity, delay: Double)
         extends Event (customer, this, delay, t_s_stat):

        def occur (): Unit =
            leave (customer)                                  // collects time in sYstem statistics
            if ! waitQueue.isEmpty then                       // nIn > nUnits
                val nextService = waitQueue.dequeue ()        // first customer in queue
                schedule (Departure (nextService, nextService.serviceT))
            end if
            nIn  -= 1                                         // update the current state
            nOut += 1
        end occur

    end Departure

    //--------------------------------------------------
    // Start the simulation after scheduling the first priming event

    val firstArrival = Entity (iArrivalRV.gen, serviceRV.gen, this)
    schedule (Arrival (firstArrival, firstArrival.iArrivalT))     // first priming event
    simulate ()                                                   // start simulating

    val dt     = _clock - start                                   // simulation time in minutes
    val profit = 2.0 * nOut * HOUR / dt - 15.0 * nUnits           // profit per hour

    report (("nArr", nArr), ("nIn", nIn), ("nLost", nLost), ("nOut", nOut), ("profit", profit))
    reportStats ()
    waitQueue.summary (nStop)

    //--------------------------------------------------
    // Verify the results using an M/M/c/k Queueing Model

    println ("\nVerification ...")
    val mm1 = new MMck_Queue (lambda / HOUR, mu / HOUR, nUnits, nUnits + cap)
    mm1.view ()
    mm1.report ()

end FastFoodModel

