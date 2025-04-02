
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Aug 29 14:14:32 EDT 2020
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    PGraph - Property Graph Database
 */

package scalation
package database
package graph

import java.util.concurrent.ConcurrentLinkedQueue 

//import scala.collection.immutable.{Vector => VEC}
import scala.collection.mutable.{ArrayBuffer => VEC}

import scala.collection.mutable.Map

import scalation.animation.{AnimateCommand, DgAnimator}
import scalation.animation.CommandType.{CreateEdge, CreateNode}
import scalation.mathstat.VectorD
import scalation.scala2d.Colors._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `PGraph` class is used to store property graphs.
 *  @param name       the name of the property graph
 *  @param vt         the set of vertex-types
 *  @param et         the set of edge-types
 *  @param animating  whether to animate the model (defaults to false)
 *  @param aniRatio   the ratio of simulation speed vs. animation speed
 *  @param width      the width of the drawing canvas
 *  @param height     the height of the drawing canvas
 */
class PGraph (val name: String,
              val vt: VEC [VertexType] = VEC [VertexType] (null),
              val et: VEC [EdgeType]   = VEC [EdgeType] (null),
              animating: Boolean = false, aniRatio: Double = 1.0,
              width: Int = 800, height: Int = 600)
      extends Serializable:

    private val debug = debugf ("PGraph", true)                              // debug function
    private val flaw  = flawf ("PGraph")                                     // flaw function

    debug ("init", s"""PGraph (name = $name, ${vt.size} vertex-types, ${et.size} edge-types,
                               animating = $animating, aniRatio = $aniRatio)""") 

    private val dgAni: DgAnimator =                                          // animation engine
            if animating then
                debug ("init", s"create DgAnimator for property-graph $name")
                new DgAnimator (s"$name Property Graph Animator", black, white, aniRatio, width, height)
            else null

    private val aniQ: ConcurrentLinkedQueue [AnimateCommand] =               // animation engine's command queue
            if animating then
                dgAni.getCommandQueue
            else null

    val vmap = Map [String, VertexType] ()                                   // map name to vertex-type
    val emap = Map [String, EdgeType] ()                                     // map name to edge-type

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the animation flag (that is private) to done.
     */
    def setAniDone () = dgAni.setAniDone ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the name maps, vmap and emap, after constructing this property graph.
     */
    def updateNameMaps (): Unit =
        for v <- vt do vmap += v.name -> v
        for e <- et do emap += e.name -> e
    end updateNameMaps

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a graph containing relationships (edge-types)
     *  starting from the given vertex-type and ending at the given set of vertex-types
     *  vt_from - { et } -> { vt_to }
     *  @param from     the starting vertex-type
     *  @param ets      the edge-types to traverse/retain
     *  @param tos      the set of vertex-types the edges end at
     *  @param newName  the new name for the expanded graph
     */
    def expandOut (from: VertexType, ets: VEC [EdgeType], tos: VEC [VertexType], newName: String): PGraph =
        expandBoth (VEC (from), ets, tos, newName)
    end expandOut

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a graph containing relationships (edge-types)
     *  starting from the given set of vertex-types and ending at the given vertex-type
     *  { vt_from } - { et } -> vt_to
     *  @param froms    the set of vertex-type the relationship is expanding into
     *  @param ets      the edge-types to traverse/retain
     *  @param to       the ending vertex-type
     *  @param newName  the new name for the expanded graph
     */
    def expandIn (froms: VEC [VertexType], ets: VEC [EdgeType], to: VertexType, newName: String): PGraph =
        expandBoth (froms, ets, VEC (to), newName)
    end expandIn

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Returns a graph with containing relationships (edge-types)
     *  starting from the given set of vertex-types and ending at the given set of vertex-types
     *  { vt_from } - { et } -> { vt_to }
     *  FIX - complete implementation
     *  @param froms    the set of vertex-types the relationships start from
     *  @param ets      the edge-types to traverse/retain
     *  @param tos      the set of vertex-types the edges end at
     *  @param newName  the new name for the expanded graph
     */
    def expandBoth (froms: VEC [VertexType], ets: VEC [EdgeType], tos: VEC [VertexType], newName: String): PGraph =
        PGraph (newName, vt.filter ((froms ++ tos).contains (_)), et.filter (ets contains _))
    end expandBoth

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Join two subgraphs on the specified vertex-types
     *  It combines the two graphs such that all vertices are from both the graphs
     *  exist in the new graph but with only unique vertices from specified vertex-types
     *  we are joining on.
     *  All the edges-types from the two subgraphs are present in the new graph.
     *  The edge-types from the other graph, that join the vertex-type that we are joining on
     *  are updated to relate new joined vertex-type
     *  starting from the given vertex-type and ending at the given set of vertex-types
     *  @param g2       the other (sub)graph to join this graph with
     *  @param vt1      the vertex-type in this graph that we are joining on
     *  @param vt2      the vertex-type in the other graph that we are joining on
     *  @param newName  the new name for the joined graph
     */
    def join (g2: PGraph, vt1: VertexType, vt2: VertexType, newName: String): PGraph =
        val new_vts = VEC (vt1 union vt2) ++ (vt diff VEC (vt1)) ++ (g2.vt diff VEC (vt2))
        val new_ets = et.clone
        for _et <- g2.et do
            new_ets += new EdgeType (newName + "_je",
                                     if _et.from == vt2 then vt1 else _et.from, _et.schema,
                                     if _et.to == vt2   then vt1 else _et.to, _et.edges)
        end for
        PGraph (newName, new_vts, new_ets.distinct)        // FIX - distinct does not work
    end join

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print this property graph including all vertex-types and edge-types.
     */
    def print (): Unit =
        banner (s"PGraph name = $name:")
        banner ("Vertex Types:")
        for i <- vt.indices do println (s"vt($i) = ${vt(i)}")
        banner ("Edge Types:")
        for j <- et.indices do println (s"et($j) = ${et(j)}")
    end print

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** SHOW/print this property graph.
     */
    def show (): Unit =
        for vti <- vt do vti.show ()                                         // show each vertex-type
        for eti <- et do eti.show ()                                         // show each edge-type
    end show

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Display this property graph in 2D.
     *  @param stop  the time to stop the animation
     */
    def display (stop: Double): Unit =
        if ! animating then flaw ("display", "unable to animate this property-graph as animating = false")

        banner (s"Display Property Graph $name in 2D and animate until $stop")

        for i <- vt.indices do
            val vt_i = vt(i)                                                 // vertex-type i
            println (s"vt($i) = $vt_i")
            for v <- vt_i.verts do
                val at = v.pos.toArray
                aniQ.add (AnimateCommand (CreateNode, v.id, vt_i.newShapeObj, v.name, true, vt_i.color, at, 0))
            end for
        end for

        for j <- et.indices do
            val et_j = et(j)                                                 // edge-type j
            println (s"et($j) = $et_j")
            for e <- et_j.edges do
//              val at  = e.calcEndPoints                                    // logic replaced by move2Boundary
                val at  = (e.from.pos(0 to 2), e.to.pos(0 to 2))
                val att = (at._1 ++ at._2).toArray
//              banner (s"PGraph: e = $e, at = $at")
                aniQ.add (AnimateCommand (CreateEdge, e.id, et_j.newShapeObj, e.name, true, et_j.color, att, 0,
                                          e.from.id, e.to.id, e.shift))      // shift for bundle of edges
            end for
        end for

//        assert (1 == 2)
        dgAni.animate (0, stop)
    end display

    def add_aniQ (aniCmd: AnimateCommand): Unit = aniQ.add (aniCmd)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this property graph to a string.
     */
    override def toString: String =
        s"PGraph (name = $name,\n vt = $vt},\n et = $et\n)"
                 .replace ("ArrayBuffer", "VEC")
    end toString

end PGraph


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `SocialNetwork` onject provides a sample property-graph database.
 */
object SocialNetwork:

    // Populate the property-graph

    val employs: Property = Map ("type" -> "employs")

    val x0 = VectorD (200, 100, 40, 40)                           // vertex coordinates, size: x, y, w, h
    val x1 = VectorD (100, 400, 40, 40)
    val x2 = VectorD (500, 500, 40, 40)

    // Build vertices and vertex-types

    val v = VEC (
        new Vertex ("Bob", Map ("name" -> "Bob", "state" -> "GA", "salary" -> 85000.0), x0),
        new Vertex ("Sue", Map ("name" -> "Sue", "state" -> "FL", "salary" -> 95000.0), x1),
        new Vertex ("Joe", Map ("name" -> "Joe", "state" -> "GA", "salary" -> 99000.0), x2))
    val vt0 = VertexType ("person", "name, state, salary", v)

    println (s"check schema for vertex-type vt0 = ${vt0.check}")
    vt0.buildIndex ("name")

    // Build edges and edge-types

    val et0 = EdgeType ("knows", vt0, "type", vt0, VEC (
        new Edge ("knows", v(0), Map ("type" -> "knows", "since" -> 5), v(1)),
        new Edge ("knows", v(1), Map ("type" -> "knows", "since" -> 2), v(0), -1),
        new Edge ("knows", v(2), Map ("type" -> "knows", "since" -> 4), v(0))))
    val et1 = EdgeType ("employs", vt0, "type", vt0, VEC (
        new Edge ("employs", v(1), employs, v(0), 1),
        new Edge ("employs", v(2), employs, v(1))))

    println (s"check schema for edge-type et0 = ${et0.check}")
    println (s"check schema for edge-type et1 = ${et1.check}")

end SocialNetwork

import SocialNetwork._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `pGraphTest` main function is used to test the `PGraph` class, by displaying the graph.
 *  > runMain scalation.database.graph.pGraphTest
 */
@main def pGraphTest (): Unit =

    val g = PGraph ("SocialNetwork", VEC (vt0), VEC (et0, et1), true)
    g.updateNameMaps ()                                              // update vmap and emap

    println (s"g = $g")                                              // via toString
    g.print ()                                                       // more readable version via internal println's
    g.display (100)                                                  // display the graph in 2D

end pGraphTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `pGraphTest2` main function is used to test the `PGraph` class, by performing several queries.
 *  > runMain scalation.database.graph.pGraphTest2
 */
@main def pGraphTest2 (): Unit =

    val g = PGraph ("SocialNetwork", VEC (vt0), VEC (et0, et1))
    g.updateNameMaps ()                                              // update vmap and emap
    val person = g.vmap("person")                                    // shorthand definition
    val knows  = g.emap("knows")                                     // shorthand definition

    println (s"g = $g")                                              // via toString
    g.print ()                                                       // more readable version via internal println's
    g.show ()                                                        // show in tabular form

    println (" VertexType Queries ------------------------------------------------------") 

    banner ("query1: ==")
    val query1 = g.vt(0) == ("name", "Sue")
    println (query1)
    query1.print ()
    query1.show ()

    banner ("query2: ==")
    val query2 = person == ("name", "Sue")
    println (query2)
    query2.print ()
    query2.show ()

    banner ("query3: <")
    val query3 = person < ("salary", 90000.0)
    println (query3)
    query3.print ()
    query3.show ()

    banner ("query4: project")
    val query4 =  person.project ("name")
    println (query4)
    query4.print ()
    query4.show ()

    banner ("query4b: project")
    val query4b = person.project ("name")
    println (query4b)
    query4b.print ()
    query4b.show ()

    banner ("query5: select")
    val query5 = person.select ((p: Property) => p("name") == "Sue")
    println (query5)
    query5.print ()
    query5.show ()

    banner ("query5b: select")
    val query5b = person.select (_("name") == "Sue")
    println (query5b)
    query5b.print ()
    query5b.show ()

    banner ("query6: unionAll")
    val query6 = person unionAll person
    println (query6)
    query6.print ()
    query6.show ()

    banner ("query7: union")
    val query7 = person union person
    println (query7)
    query7.print ()
    query7.show ()

    banner ("query8: minus")
    val query8 = person minus person
    println (query8)
    query8.print ()
    query8.show ()

    banner ("query9: expandTo")
    val query9 = knows.expandTo
    println (query9)
    query9.print ()
    query9.show ()

    banner ("query10: expandFrom")
    val query10 = knows.expandFrom
    println (query10)
    query10.print ()
    query10.show ()

    banner ("query11: expand")
    val query11 = knows.expand
    println (query11)
    query11.print ()
    query11.show ()

    banner ("query12: distinct")
    val query12 = person.distinct
    println (query12)
    query12.print ()
    query12.show ()

    banner ("query13: rename")
    val query13 = person.rename ("salary", "earnings")
    println (query13)
    query13.print ()
    query13.show ()

    banner ("query14: insert")
//  val v_new = Vertex (Map ("name" -> "Alice", "salary" -> 30000.0))
    val v_new = Vertex (Map ("name" -> "Alice", "state" -> "SC", "salary" -> 30000.0))
    val query14 = person.insert (v_new)
    println (query14)
    query14.print ()
    query14.show ()

    banner ("query15: limit")
    val query15 = person.limit (1, 3)
    println (query15)
    query15.print ()
    query15.show ()

    banner ("query16: find")
    val query16 = person.find ("Sue")
    println (query16)
    // query16.print ()  -- only returns a single vertex
    query16.show ()

    banner ("query17: orderBy")
    val query17 = person.orderBy ("name")
    println (query17)
    query17.print ()
    query17.show ()

    banner ("query18: groupBy")
    val query18 = person.groupBy ("state", "salary", (sal: Double) => sal * sal)
    println (query18)
    query18.print ()
    query18.show ()

    println (" EdgeType Queries --------------------------------------------------------") 

    banner ("query19: EdgeType insert")
    val e_new = Edge (v(0), Map ("type" -> "knows", "since" -> 6), v(2))
    val query19 = et0.insert (e_new)
    println (query19)
    query19.print ()
    query19.show ()

    banner ("query20: EdgeType project")
    val query20 =  knows.project ("type")
    println (query20)
    query20.print ()
    query20.show ()

    banner ("query20b: EdgeType project")
    val query20b = knows.project ("type")
    println (query20b)
    query20b.print ()
    query20b.show ()

    banner ("query21: EdgeType select")
    val query21 = knows.select ((p: Property) => p("since") >= 4)
    println (query21)
    query21.print ()
    query21.show ()

    banner ("query21b: EdgeType select")
    val query21b = knows.select (_("since") >= 4)
    println (query21b)
    query21b.print ()
    query21b.show ()

    banner ("query22: EdgeType unionAll")
    val query22 = knows unionAll knows
    println (query22)
    query22.print ()
    query22.show ()

    banner ("query23: EdgeType union")
    val query23 = knows union knows
    println (query23)
    query23.print ()
    query23.show ()

    banner ("query24: EdgeType minus")
    val query24 = knows minus knows
    println (query24)
    query24.print ()
    query24.show ()

    banner ("query25: EdgeType distinct")
    val query25 = knows.distinct
    println (query25)
    query25.print ()
    query25.show ()

    banner ("query26: EdgeType rename")
    val query26 = knows.rename ("type", "kind")
    println (query26)
    query26.print ()
    query26.show ()

    banner ("query27: EdgeType limit")
    val query27 = knows.limit (1, 2)
    println (query27)
    query27.print ()
    query27.show ()

    banner ("query28: EdgeType orderBy")
    val query28 = knows.orderBy ("since")
    println (query28)
    query28.print ()
    query28.show ()

    println (" PGraph Queries ----------------------------------------------------------") 

    banner ("Current Graph g")
    println (g)
    g.print ()
    g.show ()

    banner ("query29: PGraph expandOut")
    val query29 = g.expandOut (person, VEC (et0), VEC(person), "expand_out")
    println (query29)
    query29.print ()
    query29.show ()

    banner ("query30: PGraph expandIn")
    val query30 = g.expandIn (VEC (person), VEC (et0), person, "expand_in")
    println (query30)
    query30.print ()
    query30.show ()

    banner ("query31: PGraph expandBoth")
    val query31 = g.expandBoth (VEC (person), VEC (et0), VEC (person), "expand_both")
    println (query31)
    query31.print ()
    query31.show ()

    banner ("query32: PGraph join")
    val query32 = g.join (g, person, person, "joined_graph")
    println (query32)
    query32.print ()
    query32.show ()

end pGraphTest2

