
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Dec 13 22:00:52 EST 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @see     http://aix1.uottawa.ca/~glamothe/mat4371/Queues.pdf
 *  @see     http://irh.inf.unideb.hu/~jsztrik/education/16/SOR_Main_Angol.pdf
 *  @see     https://hkumath.hku.hk/~wkc/queue/qpart2.pdf
 *
 *  @title   M/M Queues:  M/M/1, M/M/2, M/M/c
 */

package scalation
package simulation
package queueingnet

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM_Queue` trait is used to solve single node Markovian Queueing problems.
 *  It models a service station consisting of one queue and c servers.
 *  The arrivals are Poisson and the service time distribution is Exponential.
 *------------------------------------------------------------------------------
 *  @see also `MMck_Queue` to model finite capacity queues.
 *  @see also `MGc_Queue` to model queues with general service time distributions.
 *------------------------------------------------------------------------------
 *  @param λ  the overall arrival rate (lambda)
 *  @param μ  the per unit service rate (mu)
 */
trait MM_Queue (λ: Double, μ: Double, c: Int):

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Probability the system is empty (in state 0, no entities).
     */
    def π_0: Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Probability the system is full (in state k where k is the capacity).
     *  For infinite capacity queues, it is 0.  Override as needed.
     */
    def π_k: Double = 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The effective arrival rate = λ (1 - π_k) that factors in turn aways.
     *  For infinite capacity queues, λe = λ.
     */
    def λe: Double = λ * (1 - π_k)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The overAll traffic intensity or rate ratio.
     */
    def a: Double = λ / μ

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The server utilization factor (overall arrival rate / total service rate).
     *  Also referred to as the traffic intensity per server ρ (rho) in [0, 1]
     *  where 0 => always idle, 1 => always busy.
     */
    def ρ: Double = a / c

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expected length/number in the sYstem, Service and Queue.
     */
    def l_q: Double = l_y - l_s
    def l_s: Double = λe / μ
    def l_y: Double

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Expected time in the sYstem, Service and Queue (computed using Little's Law).
     */
    def t_q: Double = l_q / λe
    def t_s: Double = l_s / λe
    def t_y: Double = l_y / λe

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** View/check intermediate results.
     */
    def view (): Unit =
        println ("Check queueing parameters:")
        println ("λ   = %g".format (λ)   + "\t\t overall arrival rate (lambda)")
        println ("λe  = %g".format (λe)  + "\t\t effective arrival rate")
        println ("μ   = %g".format (μ)   + "\t\t service rate per server (mu)")
        println ("c   = %d".format (c)   + "\t\t\t number of servers")
        println ("a   = %g".format (a)   + "\t\t overAll traffic intensity")
        println ("ρ   = %g".format (ρ)   + "\t\t per server utilization/traffic intensity (rho)")
        println ("π_0 = %g".format (π_0) + "\t\t probability system is empty")
        println ("π_k = %g".format (π_k) + "\t\t probability system is full")
    end view

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Report the results.
     */
    def report (): Unit =
        println ("Results for queue:")
        println ("---------------------------------------------------")
        println ("|              Number             Time            |")
        println ("---------------------------------------------------")
        println ("|  Queue    |  l_q = %8.4g".format (l_q) + "  |  t_q = %8.4g".format (t_q) + "  |")
        println ("|  Service  |  l_s = %8.4g".format (l_s) + "  |  t_s = %8.4g".format (t_s) + "  |")
        println ("|  sYstem   |  l_y = %8.4g".format (l_y) + "  |  t_y = %8.4g".format (t_y) + "  |")
        println ("---------------------------------------------------")
        println ("After time unit conversion")
        println (s"t_q = ${60*t_q}, t_s = ${60*t_s}, t_y = ${60*t_y}")
    end report

end MM_Queue 


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM1_Queue` class computes results for M/M/1 Queues (1 servers).
 */
class MM1_Queue (λ: Double, μ: Double) extends MM_Queue (λ, μ, 1):

    def π_0: Double = 1 - ρ

    def l_y: Double = ρ / (1 - ρ)

end MM1_Queue


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MM2_Queue` class computes results for M/M/2 Queues (2 servers).
 */
class MM2_Queue (λ: Double, μ: Double) extends MM_Queue (λ, μ, 2):

    def π_0: Double = (1 - ρ) / (1 + ρ)

    def l_y: Double = (2 * ρ) / (1 - ρ~^2)

end MM2_Queue


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MMc_Queue` class computes results for M/M/c Queues (c servers).
 *  @see https://web.mst.edu/~gosavia/queuing_formulas.pdf
 *  @see http://courses.washington.edu/inde411/QueueingTheoryPart3.pdf
 */
class MMc_Queue (λ: Double, μ: Double, c: Int) extends MM_Queue (λ, μ, c):

    import mathstat.Combinatorics.fac

    private val cf = fac (c)

    def π_0: Double =
        var sum = a~^c / (cf * (1 - ρ)) 
        for j <- 0 until c do sum += a~^j / fac (j)
        1 / sum
    end π_0

    def l_y: Double = π_0 * a~^c * ρ / (cf * (1 - ρ)~^2) + a

end MMc_Queue


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `mM_QueueTest` main function is used to test the `MM_Queue` class.
 *  > runMain scalation.simulation.queueingnet.mM_QueueTest
 */
@main def mM_QueueTest (): Unit =

    val λ = 6.0                                       // customer arrival rate (per hour)
    val μ = 7.5                                       // customer service rate (per hour)

    banner ("M/M/1 Queue Results:")
    val mm1 = new MM1_Queue (λ, μ)                    // M/M/1 Queue
    mm1.view ()
    mm1.report ()

    banner ("M/M/c Queue Results:")
    val mmc1 = new MMc_Queue (λ, μ, 1)                 // M/M/1 Queue
    mmc1.view ()
    mmc1.report ()

    banner ("M/M/2 Queue Results:")
    val mm2 = new MM2_Queue (2*λ, μ)                  // M/M/2 Queue
    mm2.view ()
    mm2.report ()

    banner ("M/M/c Queue Results:")
    val mmc2 = new MMc_Queue (2*λ, μ, 2)               // M/M/2 Queue
    mmc2.view ()
    mmc2.report ()

end mM_QueueTest

