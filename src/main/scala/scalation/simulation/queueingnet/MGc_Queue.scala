
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Dec 29 21:28:40 EST 2013
 *  @see     LICENSE (MIT style license file).
 *  @see     http://irh.inf.unideb.hu/~jsztrik/education/16/SOR_Main_Angol.pdf
 *
 *  @note    M/G/c Queue
 */

package scalation
package simulation
package queueingnet

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `MGc_Queue` class is used to solve single node Queueing problems.
 *  It models a service station consisting of one queue and c servers, i.e.,
 *  an M/G/c queue.  The arrivals are Poisson and the service time distribution
 *  is General.
 *------------------------------------------------------------------------------
 *  @see also `MMc_Queue` to model Markovian /M/M/c queues.
 *  @see also `MMck_Queue` to model finite capacity M/M/c/k queues.
 *------------------------------------------------------------------------------
 *  @param lambda  the arrival rate
 *  @param mu      the service rate
 *  @param c       the number of servers
 *  @param cv2     the coefficient of variations squared (variance / mean^2)
 */
class MGc_Queue (lambda: Double, mu: Double, c: Int = 1, cv2: Double = 1.0)
      extends MMc_Queue (lambda, mu, c):

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Expected time in the waiting Queue from (1) Pollaczek-Khinchin for c = 1
      *  and Kingman's approximation for c > 1.  super.t_q is from base class `MMc_Queue`.
      */
     override def t_q = 0.5 * (cv2 + 1.0) * super.t_q

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Expected length of the waiting queue from Little's Law.
      */
     override def l_q = lambda * t_q

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Expected length/number in sYstem.
      */
     override def l_y = l_q + l_s 

end MGc_Queue


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `mGc_QueueTest` main function is used to test the `MGc_Queue` class.
 *  > runMain scalation.simulation.queueingnet.mGc_QueueTest
 */
@main def mGc_QueueTest (): Unit =

    val lambda = 6.0                                   // customer arrival rate (per hour)
    val mu     = 7.5                                   // customer service rate (per hour)

    println("\nM/G/1 Queue Results (cv2 = 1):")
    val mg1 = new MGc_Queue (lambda, mu, 1)            // M/M/c Queue, Exponential
    mg1.view ()
    mg1.report ()

    println("\nM/G/2 Queue Results (cv2 = 1):")
    val mg2 = new MGc_Queue (lambda, mu, 2)            // M/M/c Queue, Exponential
    mg2.view ()
    mg2.report ()

    println("\nM/G/1 Queue Results (cv2 = 0):")
    val mg3 = new MGc_Queue (lambda, mu, 1, 0.0)       // M/M/c Queue, Deterministic
    mg3.view ()
    mg3.report ()

    println("\nM/G/2 Queue Results (cv2 = 0):")
    val mg4 = new MGc_Queue (lambda, mu, 2, 0.0)       // M/M/c Queue, Deterministic
    mg4.view ()
    mg4.report ()

end mGc_QueueTest

