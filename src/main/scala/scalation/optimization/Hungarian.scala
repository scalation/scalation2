
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Ashwinkumar Ajithkumar Pillai, John Miller
 *  @version 2.0
 *  @date    Sat Nov 11 16:28:52 EST 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Hungarian Algorithm for Assignment Problem (AP) J x W
 *
 *  Translated from C++ code from Assignment Problem and Hungarian Algorithm
 *  @see https://en.wikipedia.org/wiki/Hungarian_algorithm
 */

package scalation
package optimization

import scala.collection.mutable.Set
import scalation.mathstat.{MatrixD, VectorD, VectorI}
  
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `hungarian` method is an O(n^3) [ O(J^2W) ] implementation of the Hungarian
 *  algorithm (or Kuhn-Munkres algorithm) for assigning jobs to workers.
 *  Given J jobs and W workers, find a minimal cost assignment of JOBS to WORKERS such
 *  that each worker is assigned to at most one job and each job has one worker assigned.
 *  It solves the minimum-weighted bipartite graph matching problem.
 *
 *      minimize sum_j { cost(j, w) }
 *
 *  @param cost  the cost matrix: cost(j, w) = cost of assigning job j to worker w
 */
def hungarian (cost: MatrixD): (VectorI, VectorD) = 

    val MAX = Double.MaxValue                                 // largest `Double`
    val J   = cost.dim                                        // number of jobs (rows)
    val W   = cost.dim2                                       // number of workers (columns)
    assert (J <= W)                                           // assumes there are enough workers to cover jobs

    val job = VectorI.fill (W + 1)(-1)                        // job(w) is job assigned to worker w
                                                              // extra slot (+ 1) added for storing delta sums
    val ys    = new VectorD (J)                               // source potentials
    val yt    = new VectorD (W + 1)                           // target potentials (with extra slot)
    val acost = new VectorD (J)                               // accumulating costs of assigning the first j+1 jobs

    val min_to = VectorD.fill (W + 1)(MAX)                    // for computing min cost to worker w
    val prv    = VectorI.fill (W + 1)(-1)                     // previous worker
    val in_Z   = Set [Int] ()                                 // set of workers in Z

    for j_cur <- 0 until J do                                 // loop through all jobs (current job)
        var w_cur  = W                                        // current worker
        job(w_cur) = j_cur                                    // temp. assign current job
        min_to.set (MAX); prv.set (-1); in_Z.clear ()         // reset

        while job(w_cur) != -1 do
            in_Z      += w_cur                                // put the current worker in Z
            val j      = job(w_cur)                           // j is the job assigned to the current worker
            var delta  = MAX
            var w_next = 0                                    // worker giving min delta

            for w <- 0 until W do                             // loop through all workers
                if ! (in_Z contains w) then
                    val del_jw = cost(j, w) - ys(j) - yt(w)   // delta for (j, w) case
                    if del_jw < min_to(w) then
                        min_to(w) = del_jw                    // smaller than min_to => update min_to
                        prv(w) = w_cur                        // record previous as current worker
                    if min_to(w) < delta then
                        delta = min_to(w)                     // smaller than delta => update delta
                        w_next = w                            // record worker giving lower delta
            end for
        
            for w <- 0 to W do
                if in_Z contains w then { ys(job(w)) += delta; yt(w) -= delta }  // update potentials
                else min_to(w) -= delta
            end for
        
            w_cur = w_next                                     // update current worker 
        end while

        while w_cur != -1 do                                   // update assignments using prv to give path
            val w_ = prv(w_cur)
            job(w_cur) = if w_ == -1 then 0 else job(w_)       // update job for current worker
            w_cur = w_                                         // current worker becomes previous
        end while

        acost(j_cur) = -yt(W)                                  // record accumulated cost so far
    end for

    (job, acost)                                               // return assignments and accumulated costs

end hungarian


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Show the assignments of jobs to workers and the accumulating costs.
 *  @param job_acost  the (job, acost) tuple
 *  @param cost       the cost matrix: cost(j, w) = cost of assigning job j to worker w
 */
def showAssignments (job_cost: (VectorI, VectorD), cost: MatrixD): Unit =

    val (job, acost) = job_cost

    for w <- 0 until job.dim-1 do
        val j = job(w)
        val costStr = if job(w) != -1 then cost(j, w).toString else "NA"
        println (s"job $j assigned to worker $w with cost (j = $j, w = $w) = $costStr") 
    end for

    println (s"accumulating costs: $acost")

end showAssignments


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `hungarianTest` main function test the `hungarianTest` method.
 *  @see http://people.whitman.edu/~hundledr/courses/M339S20/M339/Ch07_5.pdf
 *  Minimal total cost = 51
 *  > runMain scalation.optimization.hungarianTest
 */
@main def hungarianTest (): Unit =

    banner ("Hungarian Algorithm Problem 1")

    val cost = MatrixD ((5, 5), 11,  7, 10, 17, 10,     // workers (5) x jobs (5) => transpose
                                13, 21,  7, 11, 13,
                                13, 13, 15, 13, 14,
                                18, 10, 13, 16, 14,
                                12,  8, 16, 19, 10).transpose

    showAssignments (hungarian (cost), cost)
      
end hungarianTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `hungarianTest2` main function test the `hungarianTest` method.
 *  @see https://d13mk4zmvuctmz.cloudfront.net/assets/main/study-material/notes/
 *  electrical-engineering_engineering_operations-research_assignment-problems_notes.pdf
 *
 *  Solution:
 *  job 4 assigned to worker 0 with cost (j = 4, w = 0) = 1.0
 *  job 1 assigned to worker 1 with cost (j = 1, w = 1) = 5.0
 *  job 0 assigned to worker 2 with cost (j = 0, w = 2) = 3.0
 *  job 2 assigned to worker 3 with cost (j = 2, w = 3) = 2.0
 *  job 3 assigned to worker 4 with cost (j = 3, w = 4) = 9.0
 *  job -1 assigned to worker 5 with cost (j = -1, w = 5) = NA (worker 5 is unassigned)
 *  Minimal total cost = 20
 *
 *  > runMain scalation.optimization.hungarianTest2
 */
@main def hungarianTest2 (): Unit =

    banner ("Hungarian Algorithm Problem 2")

//                               W0    W1    W2    W3    W4     W5      // jobs (5) x workers (6)
    val cost = MatrixD ((5, 6),  2.5,  2.0, +3.0,  3.5,  4.0,   6.0,    // J0
                                 5.0, +5.0,  6.5,  7.0,  7.0,   9.0,    // J1
                                 1.0,  1.5,  2.0, +2.0,  3.0,   5.0,    // J2
                                 6.0,  7.0,  9.0,  9.0, +9.0,  10.0,    // J3
                                +1.0,  3.0,  4.5,  4.5,  6.0,   6.0)    // J4

    showAssignments (hungarian (cost), cost)
      
end hungarianTest2

