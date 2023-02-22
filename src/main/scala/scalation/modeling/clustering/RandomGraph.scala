
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Jun  6 14:30:32 EDT 2012
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Random Graph Generator
 */

package scalation
package modeling
package clustering

import scala.math.abs

import scalation.mathstat.MatrixD
import scalation.random.{Bernoulli, Randi0}

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `RandomGraph` class generates random undirected graphs with clusters
 *  (as adjacency matrices).
 *  @param n  the number of nodes in the graph
 *  @param p  the probability that any two nodes are connected
 *  @param c  the number of clusters to generate
 */
class RandomGraph (n: Int, p: Double, c: Int):

    private val g    = new MatrixD (n, n)     // adjacency matrix representation of graph
    private val coin = Bernoulli (p)          // a biased coin (p = probability of head(1))
    private val pick = Randi0 (n-1)           // random integer generator: 0, ..., n-1

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Generate a cluster random graph returning its adjacency matrix.
     */
    def gen (): MatrixD =
        for i <- g.indices; j <- 0 until i do
            g(i, j) = coin.gen; g(j, i) = g(i, j)
        end for
        for k <- 0 until c do
            val ic = pick.igen
            var jc = 0
            while { jc = pick.igen; abs (jc - ic) < 3 } do {}
            println (s"cluster $k at ($ic, $jc)")
            g(ic, jc) = 1.0
            if ic > 0 then   { g(ic-1, jc) = 1.0 - coin.gen; g(jc, ic-1) = g(ic-1, jc) }
            if ic > 1 then   { g(ic-2, jc) = 1.0 - coin.gen; g(jc, ic-2) = g(ic-2, jc) }
            if ic < n-1 then { g(ic+1, jc) = 1.0 - coin.gen; g(jc, ic+1) = g(ic+1, jc) }
            if ic < n-2 then { g(ic+2, jc) = 1.0 - coin.gen; g(jc, ic+2) = g(ic+2, jc) }
            if jc > 0 then   { g(ic, jc-1) = 1.0 - coin.gen; g(jc-1, ic) = g(ic, jc-1) }
            if jc > 1 then   { g(ic, jc-2) = 1.0 - coin.gen; g(jc-2, ic) = g(ic, jc-2) }
            if jc < n-1 then { g(ic, jc+1) = 1.0 - coin.gen; g(jc+1, ic) = g(ic, jc+1) }
            if jc < n-2 then { g(ic, jc+2) = 1.0 - coin.gen; g(jc+2, ic) = g(ic, jc+2) }
        end for
        g
    end gen

end RandomGraph


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `randomGraphTest` object is used to test the `RandomGraph` class.
 *  > runMain scalation.modeling.clutering.randomGraphTest
 */
@main def randomGraphTest (): Unit =

    val rg = new RandomGraph (20, .1, 5)
    println ("graph = " + rg.gen ())

end randomGraphTest

