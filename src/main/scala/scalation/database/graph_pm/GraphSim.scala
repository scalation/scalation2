
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 2.0
 *  @date    Tue Nov  1 19:12:16 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    `Graph` Simulation Using Mutable Sets
 */

package scalation
package database
package graph_pm

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}
import scala.util.control.Breaks.{break, breakable}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphSim` class provides an implementation for Simple Graph Simulation.
 *  For each vertex in a query graph q, it returns all matching vertices in the
 *  data graph g with the same vertex label and matching children.
 *  @see https://cobweb.cs.uga.edu/~squinn/mmd_f15/articles/graphanalytics2.pdf
 *  @param g  the data graph  G(V, E, l)
 *  @param q  the query graph Q(U, D, k)
 */
class GraphSim (g: Graph, q: Graph)
      extends GraphMatcher (g, q):

    private val debug = debugf ("GraphSim", true)                   // debug function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the mappings φ produced by the feasibleMates method,
     *  eliminate mappings u -> v when v's children fail to match u's.
     *  This version ignores edge labels.
     *  @see https://getd.libs.uga.edu/pdfs/saltz_matthew_w_201308_ms.pdf
     *  @param φ  array of mappings from a query vertex u to { graph vertices v }
     */
    def prune0 (φ: Array [SET [Int]]): Array [SET [Int]] =
        val rem   = SET [Int] ()                                    // vertices to be removed
        var alter = true                                            // vertices to be removed, whether removed?
        breakable {
            while alter do                                          // check for matching children
                alter = false                                       // no vertices removed yet

                for u <- qRange; u_c <- q.ch(u) do                  // for each u in q and its children u_c
                    debug ("prune0", s"for u = $u, u_c = $u_c")
                    val φ_u_c = φ(u_c)                              // u_c mapped to vertices in g

                    for v <- φ(u) do                                // for each v in g image of u
                        if (g.ch(v) & φ_u_c).isEmpty then           // v must have a child in φ(u_c)
                            rem += v; alter = true                  // match failed => add v to removal list
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
        val rem   = SET [Int] ()                                    // vertices to be removed
        var alter = true                                            // vertices to be removed, whether removed?
        breakable {
            while alter do                                          // check for matching children
                alter = false                                       // no vertices removed yet

                for u <- qRange; u_c <- q.ch(u) do                  // for each u in q and its children u_c
                    debug ("prune", s"for u = $u, u_c = $u_c")
                    val l_u2u_c = q.elabel ((u, u_c))               // edge label in q for (u, u_c)
                    val φ_u_c   = φ(u_c)                            // u_c mapped to vertices in g

                    for v <- φ(u) do                                // for each v in g image of u
                        val v_c = g.ch(v).filter (l_u2u_c == g.elabel (v, _))   // filter on edge labels with ==
                        if (v_c & φ_u_c).isEmpty then               // v must have a child in φ(u_c)
                            rem += v; alter = true                  // match failed => add v to removal list
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

end GraphSim


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `graphSimTest` main function is used to test the `GraphSim` class.
 *  > runMain scalation.database.graph_pm.graphSimTest
 */
@main def graphSimTest (): Unit =

    val g = new Graph (Array (SET (1, 3),                      // ch(0)
                              SET (2),                         // ch(1)
                              SET (),                          // ch(2)
                              SET (),                          // ch(3)
                              SET (2),                         // ch(4)
                              SET (4)),                        // ch(5)
                       Array (10.0, 11.0, 11.0, 11.0, 11.0, 10.0),
                       Map ((0, 1) -> -1.0,
                            (0, 3) -> -1.0,
                            (1, 2) -> -1.0,
                            (4, 2) -> -1.0,
                            (5, 4) -> -1.0),                   // change from -1 to -2 filter out vertices
                       false, "g")

    val q = new Graph (Array (SET (1),                         // ch(0)
                              SET (2),                         // ch(1)
                              SET ()),                         // ch(2)
                       Array (10.0, 11.0, 11.0),
                       Map ((0, 1) -> -1.0,
                            (1, 2) -> -1.0),
                       false, "q")

    println (s"g.checkEdges   = ${g.checkEdges}")
    println (s"g.checkElabels = ${g.checkElabels}")
    g.printG ()
    println (s"q.checkEdges   = ${q.checkEdges}")
    println (s"q.checkElabels = ${q.checkElabels}")
    q.printG ()

    val matcher = new GraphSim (g, q)                          // Graph Simulation Pattern Matcher
    matcher.test ("GraphSim")                                  // check edge labels
//  matcher.test ("GraphSim", ignore = true)                   // ignore edge labels

end graphSimTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `graphSimTest2` main function is used to test the `GraphSim` class.
 *  > runMain scalation.database.graph_pm.graphSimTest2
 */
@main def graphSimTest2 (): Unit =

    import graph_pm.{ExampleGraphD => EX_GRAPH}
    import MatchAnswers._

    val g = EX_GRAPH.g2p
    val q = EX_GRAPH.q2p

    println (s"g.checkEdges   = ${g.checkEdges}")
    println (s"g.checkElabels = ${g.checkElabels}")
    g.printG ()
    println (s"q.checkEdges   = ${q.checkEdges}")
    println (s"q.checkElabels = ${q.checkElabels}")
    q.printG ()

    val matcher = new GraphSim (g, q)                          // Graph Simulation Pattern Matcher
    matcher.test("GraphSim", shift (graphSim))

end graphSimTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `graphSimTest3` main function is used to test the `GraphSim` class.
 *  > runMain scalation.database.graph_pm.graphSimTest3
 */
@main def graphSimTest3 (): Unit =

    import graph_pm.{ExampleGraphD => EX_GRAPH}

    val g = EX_GRAPH.g1p
    val q = EX_GRAPH.q1p

    println (s"g.checkEdges   = ${g.checkEdges}")
    println (s"g.checkElabels = ${g.checkElabels}")
    g.printG ()
    println (s"q.checkEdges   = ${q.checkEdges}")
    println (s"q.checkElabels = ${q.checkElabels}")
    q.printG ()

    val matcher = new GraphSim (g, q)                          // Graph Simulation Pattern Matcher
    matcher.test ("GraphSim")

end graphSimTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `graphSimTest4` main function is used to test the `GraphSim` class.
 *  > runMain scalation.database.graph_pm.graphSimTest4
 */
@main def graphSimTest4 (): Unit =

    val gSize     = 1000                      // size of the data graph
    val qSize     =   10                      // size of the query graph
    val vLabels   =  100                      // number of distinct vertex labels
    val eLabels   =   10                      // number of distinct edge labels
    val gAvDegree =    5                      // average vertex out degree for data graph
    val qAvDegree =    2                      // average vertex out degree for query graph

    val gGen = new GraphGen ('D')

    val g = gGen.genRandomGraph (gSize, vLabels, eLabels, gAvDegree)
    val q = gGen.genBFSQuery (qSize, qAvDegree, g, false, "q")

    println ("q.checkEdges   = " + q.checkEdges)
    println ("q.checkElabels = " + q.checkElabels)
    q.printG ()

    val matcher = new GraphSim (g, q)                          // Graph Simulation Pattern Matcher
    matcher.test ("GraphSim")

end graphSimTest4

