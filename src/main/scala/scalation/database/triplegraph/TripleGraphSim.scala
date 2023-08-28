
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Oct  9 15:40:47 EDT 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Multi-Graph `TripleGraph` Graph Simulation Using Mutable Sets
 */

package scalation
package database
package triplegraph

import scala.collection.mutable.{ArrayBuffer => Bag, Set => SET}
import scala.util.control.Breaks.{break, breakable}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TripleGraphSim` class provides an implementation for Simple Graph Simulation.
 *  For each vertex in a query graph q, it returns all matching vertices in the
 *  data graph g with the same vertex label and matching children.
 *  @see https://cobweb.cs.uga.edu/~squinn/mmd_f15/articles/graphanalytics2.pdf
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class TripleGraphSim (g: TripleGraph, q: TripleGraph)
      extends TripleGraphMatcher (g, q):

    private val debug = debugf ("TripleGraphSim", true)                 // debug function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings φ produced by the feasibleMates method,
     *  eliminate mappings u -> v when v's children fail to match u's.
     *  This version ignores edge labels.
     *  @see https://getd.libs.uga.edu/pdfs/saltz_matthew_w_201308_ms.pdf
     *  @param φ  array of mappings from a query vertex u to { graph vertices v }
     */
    def prune0 (φ: Array [SET [Int]]): Array [SET [Int]] =
        var (rem, alter) = (SET [Int] (), true)                     // vertices to be removed, whether removed?
        breakable {
            while alter do                                          // check for matching children
                alter = false                                       // no vertices removed yet

                for u <- q.label.indices; u_c <- q.ch(u) do         // for each u in q and its children u_c
                    debug ("prune0", s"for u = $u, u_c = $u_c")
                    val φ_u_c = φ(u_c)                              // u_c mapped to vertices in g

                    for v <- φ(u) do                                // for each v in g image of u
                        if (g.ch(v) & φ_u_c).isEmpty then           // v must have a child in φ(u_c)
                            rem += v; alter = true                  // match failed => add v to removal list
                        end if
                    end for
                    if remove (φ, u, rem) then break ()             // remove rem vertices from φ(u)
                end for
            end while
        } // breakable
        φ
    end prune0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings φ produced by the feasibleMates method,
     *  eliminate mappings u -> v when v's children fail to match u's.
     *  This version checks edge labels.
     *  @param φ  array of mappings from a query vertex u to { graph vertices v }
     */
    def prune (φ: Array [SET [Int]]): Array [SET [Int]] =
        var (rem, alter) = (SET [Int] (), true)                     // vertices to be removed, whether removed?
        breakable {
            while alter do                                          // check for matching children
                alter = false                                       // no vertices removed yet

                for u <- q.label.indices; u_c <- q.ch(u) do         // for each u in q and its children u_c
                    debug ("prune", s"for u = $u, u_c = $u_c")
                    val l_u2u_c = q.elabelMap ((u, u_c))            // edge label in q for (u, u_c)
                    val φ_u_c   = φ(u_c)                            // u_c mapped to vertices in g

                    for v <- φ(u) do                                // for each v in g image of u
                        val v_c = g.ch(v).filter (l_u2u_c subsetOf g.elabelMap (v, _))   // filter on edge labels with subset
                        if (v_c & φ_u_c).isEmpty then               // v must have a child in φ(u_c)
                            rem += v; alter = true                  // match failed => add v to removal list
                        end if
                    end for
                    if remove (φ, u, rem) then break ()             // remove rem vertices from φ(u)
                end for
            end while
        } // breakable
        φ
    end prune

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove the vertices found by a pruninng step and signal early termination
     *  when φ(u) become empty by returning true.
     *  @param φ    array of mappings from a query vertex u to { graph vertices v }
     *  @param u    the query graph vertices whose mapping is being pruned
     *  @param rem  the vertices to be pruned from mapping of u
     */
    def remove (φ: Array [SET [Int]], u: Int, rem: SET [Int]): Boolean =
        var terminate = false                                       // early termination flag
        if ! rem.isEmpty then
            debug ("prune", s"rem = $rem from φ($u)")
            φ(u) --= rem                                            // remove vertices in v_rem from φ(u)
            rem.clear ()                                            // clear the rem set
            if φ(u).isEmpty then terminate = true                   // no match for vertex u => no overall match
        end if
        terminate
    end remove

end TripleGraphSim


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `tripleGraphSimTest` main function is used to test the `TripleGraphSim` class.
 *  > runMain scalation.database.triplegraph.tripleGraphSimTest
 */
@main def tripleGraphSimTest (): Unit =

    val g = new TripleGraph (Array (10.0, 11.0, 11.0, 11.0, 11.0, 10.0),
                             Bag (Triple (0, -1.0, 1),
                                  Triple (0, -2.0, 1),
                                  Triple (0, -3.0, 1),
                                  Triple (0, -1.0, 3),
                                  Triple (1, -1.0, 2),
                                  Triple (4, -1.0, 2),
                                  Triple (5, -1.0, 4),        // change from -1 to -2 filter out vertices
                                  Triple (5, -2.0, 4)),       // change from -1 to -2 filter out vertices
                             "g")
 
    val q = new TripleGraph (Array (10.0, 11.0, 11.0),
                             Bag (Triple (0, -1.0, 1),
                                  Triple (0, -2.0, 1),
                                  Triple (1, -1.0, 2)),
                             "q")

    banner ("Data Graph g")
    g.printG ()
    banner ("Query Graph g")
    q.printG ()

    banner ("Graph Simulation")
    val matcher = new TripleGraphSim (g, q)                          // Graph Simulation Pattern Matcher
//  matcher.test ("TripleGraphSim")                                  // check edge labels
    matcher.test ("TripleGraphSim", ignore = true)                   // ignore edge labels

end tripleGraphSimTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `tripleGraphSimTest2` main function is used to test the `TripleGraphSim` class.
 *  > runMain scalation.database.triplegraph.tripleGraphSimTest2
 *
@main def tripleGraphSimTest2 (): Unit =

    import triplegraph.{ExampleTrGraphD => EX_GRAPH}
    import MatchAnswers._

    val g = EX_GRAPH.g2p
    val q = EX_GRAPH.q2p

    println (s"g.checkEdges   = ${g.checkEdges}")
    println (s"g.checkElabels = ${g.checkElabels}")
    g.printG ()
    println (s"q.checkEdges   = ${q.checkEdges}")
    println (s"q.checkElabels = ${q.checkElabels}")
    q.printG ()

    val matcher = new TripleGraphSim (g, q)                          // Graph Simulation Pattern Matcher
    matcher.test("TripleGraphSim", shift (graphSim))

end tripleGraphSimTest2
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `tripleGraphSimTest3` main function is used to test the `TripleGraphSim` class.
 *  > runMain scalation.database.triplegraph.tripleGraphSimTest3
 *
@main def tripleGraphSimTest3 (): Unit =

    import triplegraph.{ExampleTrGraphD => EX_GRAPH}

    val g = EX_GRAPH.g1p
    val q = EX_GRAPH.q1p

    println (s"g.checkEdges   = ${g.checkEdges}")
    println (s"g.checkElabels = ${g.checkElabels}")
    g.printG ()
    println (s"q.checkEdges   = ${q.checkEdges}")
    println (s"q.checkElabels = ${q.checkElabels}")
    q.printG ()

    val matcher = new TripleGraphSim (g, q)                          // Graph Simulation Pattern Matcher
    matcher.test ("TripleGraphSim")

end tripleGraphSimTest3
 */


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `tripleGraphSimTest4` main function is used to test the `TripleGraphSim` class.
 *  > runMain scalation.database.triplegraph.tripleGraphSimTest4
 *
object tripleGraphSimTest4 extends App

    val gSize     = 1000         // size of the data graph
    val qSize     =   10         // size of the query graph
    val nLabels   =  100         // number of distinct labels
    val gAvDegree =    5         // average vertex out degree for data graph
    val qAvDegree =    2         // average vertex out degree for query graph

    val g = null.asInstanceOf [TripleGraph]   // GraphGen.genRandomGraph (gSize, nLabels, gAvDegree, false, "g")        // FIX
    val q = null.asInstanceOf [TripleGraph]   // GraphGen.genBFSQuery (qSize, qAvDegree, g, false, "q")                 // FIX

    println ("q.checkEdges   = " + q.checkEdges)
    println ("q.checkElabels = " + q.checkElabels)
    q.printG ()

    val matcher = new TripleGraphSim (g, q)                          // Graph Simulation Pattern Matcher
    matcher.test ("TripleGraphSim")

end tripleGraphSimTest4
 */

