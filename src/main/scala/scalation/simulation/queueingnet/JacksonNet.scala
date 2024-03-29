
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue Nov 29 15:46:51 EST 2011
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Jackson Queueing Network
 *
 *  @see www.ece.virginia.edu/~mv/edu/715/lectures/QNet.pdf
 *  @see hspm.sph.sc.edu/Courses/J716/pdf/716-8%20Queuing%20Theory%20Cookbook.pdf
 */

package scalation
package simulation
package queueingnet

import scala.runtime.ScalaRunTime.stringOf

import scalation.mathstat._
import scalation.mathstat.Combinatorics.fac
import scalation.mathstat.MatrixD.eye

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `JacksonNet` class is used to solve Jackson Queueing Network problems.
 *  Each node in the network represents a service station consisting of one queue
 *  and k_i servers.  It is currently limited to networks of M/M/k queues.
 *  @param p   the routing probabilities from node to node
 *  @param r   the external arrival rates for each node
 *  @param mu  the service rates for each node
 *  @param k   the number of servers for each node
 */
class JacksonNet (p: MatrixD, r: VectorD, mu: VectorD, private var k: Array [Int] = null):

     /** Size of the Jackson network (number of nodes)
      */
     private val m = mu.dim

     /** Identity matrix
      */
     private val ident = eye (m, m)

     /** Effective arrival rates at each node
      */
     private val lambda = r *: Fac_LU.inverse (ident - p)()  // with routing
//   private val lambda = r                                  // no routing

     if k == null then k = Array.fill [Int] (m)(1)           // default to M/M/1 queues

     /** Utilization factor for each node
      */
     private val rho = new VectorD (m)
     for i <- 0 until m do rho(i) = lambda(i) / (mu(i) * k(i).toDouble)

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** View/check intermediate results.
      */
     def view (): Unit =
         println ("view queueing network parameters:")
         println ("p      = " + p)                           // routing probability matrix
         println ("r      = " + r)                           // external rate vector
         println ("lambda = " + lambda)                      // effective arrival rate vector
         println ("mu     = " + mu)                          // service rate vector
         println ("k      = " + stringOf (k))                // number of servers vector
         println ("rho    = " + rho)                         // utilization factor vector
     end view

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Calculate the probability of a node being empty, based on its utilization
      *  factor and number of servers.
      *  @param ro  the utilization factor
      *  @param kk  the number of servers
      */
     def pi_0 (ro: Double, kk: Int): Double =
         val rok = ro * kk
         val sum = (for (i <- 0 until kk) yield rok~^i / fac (i)).sum
         1.0 / (sum + rok~^kk / (fac (kk) * (1.0 - ro)))
     end pi_0

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Calculate the expected number in the queue at the j-th node.
      *  @param j  the j-th node
      */
     def nQueue (j: Int): Double = if k(j) > 1 then nQueue_k (j) else nQueue_1 (k(j))

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Calculate the expected number in the queue at the j-th node for an M/M/1 queue.
      *  @param j  the j-th node
      */
     def nQueue_1 (j: Int): Double = { val ro = rho(j); ro~^2 / (1.0 - ro) }

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Calculate the expected number in the queue at the j-th node for an M/M/k queue.
      *  @param j  the j-th node
      */
     def nQueue_k (j: Int): Double =
        val ro = rho(j)
        val kk = k(j)
        pi_0 (ro, kk) * kk~^kk * ro~^(kk+1) / (fac (kk) * (1.0 - ro)~^2)
     end nQueue_k

     //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
     /** Report the results.
      */
     def report (): Unit =
         for j <- 0 until m do
             val lQ = nQueue (j)         // expected number waiting in the queue at node j
             val lS = rho(j) * k(j)      // expected number in service at node j
             val lT = lQ + lS            // expected number at node j
             val lamb_j = lambda(j)      // effective arrival rate at node j

             println ("\nResults for node " + j + ":")
             println ("lQ = %g".format (lQ) + "\twQ = %g".format (lQ / lamb_j))
             println ("lS = %g".format (lS) + "\twS = %g".format (lS / lamb_j))
             println ("lT = %g".format (lT) + "\twT = %g".format (lT / lamb_j))
         end for
     end report

end JacksonNet


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `jacksonNetTest` main function is used to test the `JacksonNet` class.
 *  > runMain scalation.simulation.jacksonNetTest
 */
@main def jacksonNetTest (): Unit =

    val p  = MatrixD ((2, 2), 0.0, 1.0,
                              0.0, 0.0)
    val r  = VectorD (5.0, 0.0)
    val mu = VectorD (8.0, 10.0)

    val jqn = new JacksonNet (p, r, mu)
    jqn.view ()
    jqn.report ()

end jacksonNetTest

