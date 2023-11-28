
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Dec 13 22:00:52 EST 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    M/M/c/k Queues
 *
 *  @see     http://irh.inf.unideb.hu/~jsztrik/education/16/SOR_Main_Angol.pdf
 */

package scalation
package simulation
package queueingnet

import scalation.mathstat.Combinatorics.fac

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MMck_Queue` class is used to solve single node Markovian Queueing problems.
 *  It models a service station consisting of one queue, c servers and a capacity
 *  to hold at most k entities, i.e., an M/M/c/k queue.
 *------------------------------------------------------------------------------
 *  @see also `MMc_Queue` to model infinite capacity Markovian queues.
 *  @see also `MGc_Queue` to model queues with general service time distributions.
 *------------------------------------------------------------------------------
 *  @param λ  the overall arrival rate (lambda)
 *  @param μ  the per unit service rate (mu)
 *  @param c  the number of servers (defualts to one)
 *  @param k  the capacity of the queue (defualts to infinite)
 */
class MMck_Queue (λ: Double, μ: Double, c: Int = 1, k: Int = Int.MaxValue)
      extends MM_Queue (λ, μ, c):

     private val flaw = flawf ("MMck_Queue")             // flaw function

     if c < 1 then flaw ("init", "must have at least on server")
     if k < c then flaw ("init", "not enough capacity")

     private val k_c   = k - c                           // waiting capacity
     private val c_fac = fac (c)                         // c! (factorial)
     private val a_c   = a~^c / c_fac                    // all servers busy probability factor
     private val _1_ρ  = 1 - ρ                           // one minus ρ
     private val pr_0  = π_0                             // probability system is empty

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Probability system is empty.
      */
     def π_0: Double =
         var sum = (for i <- 0 until c yield a~^i / fac (i)).sum
         sum += ( if ρ == 1 then a_c * (k_c+1) else a_c * (1.0 - ρ~^(k_c+1)) / _1_ρ )
         1.0 / sum
     end π_0

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Probability system is full.
      */
     override def π_k: Double = if k == Int.MaxValue then 0 else pr_0 * a~^k / (c~^k_c * c_fac)

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Expected length of the waiting queue.
      */
     override def l_q = pr_0 * a_c * ρ * ( 1.0 - ρ~^(k_c+1) - _1_ρ * (k_c+1) * ρ~^k_c ) / _1_ρ~^2

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Expected length/number in sYstem.
      */
     def l_y = l_q + l_s

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** View/check intermediate results.
      */
     override def view (): Unit =
         super.view ()
         println ("k   = %d".format (k) + "\t\t system holding capacity")
     end view

end MMck_Queue


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `mMck_QueueTest` main function is used to test the `MMck_Queue` class.
 *  > runMain scalation.simulation.queueingnet.mMck_QueueTest
 */
@main def mMck_QueueTest (): Unit =

    val λ = 6.0                                        // customer arrival rate (per hour)
    val μ = 7.5                                        // customer service rate (per hour)

    banner ("M/M/1/1 Queue Results:")
    val mm1_1 = new MMck_Queue (λ, μ, 1, 1)            // M/M/1/1 Queue
    mm1_1.view ()
    mm1_1.report ()

    banner ("M/M/2/4 Queue Results:")
    val mm2_4 = new MMck_Queue (2*λ, μ, 2, 4)          // M/M/2/4 Queue
    mm2_4.view ()
    mm2_4.report ()

    banner ("M/M/1 Queue Results:")
    val mm1 = new MMck_Queue (λ, μ, 1)                 // M/M/1 Queue
    mm1.view ()
    mm1.report ()

    banner ("M/M/2 Queue Results:")
    val mm2 = new MMck_Queue (2*λ, μ, 2)               // M/M/2 Queue
    mm2.view ()
    mm2.report ()

end mMck_QueueTest

