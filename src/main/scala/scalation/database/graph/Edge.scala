
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Oct  2 18:36:01 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Edge in a Property Graph
 */

package scalation
package database
package graph

//import scala.collection.immutable.{Vector => VEC}
import scala.collection.mutable.{ArrayBuffer => VEC}

import scala.collection.mutable.Map

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Edge` class maintains the edge's connections between vertices as well as its own properites.
 *  An edge is roughly analogous to implicit relationship manifest via foreign key-primary key pairs.
 *  The parameters may be thought of like a triple, e.g., (h, r, t) or (s, p, o).
 *  @param _name  the name of this edge ('name' from `Identifiable`), edge label
 *  @param from   this edge's source/from vertex
 *  @param prop   maps edge's property names into property values
 *  @param to     this edge's target/to vertex
 *  @param shift  number of units to shift to accomodate a bundle of egdes in a composite edge
 */
class Edge (_name: String, val from: Vertex, val prop: Property, val to: Vertex, val shift: Int = 0)
      extends Identifiable (_name)
         with Spatial (if from == null then to.pos else from.pos)
         with Serializable:

    val tokens = Set [Topological] ()                     // topological objects/tokens at this edge

//  private val GAP = 5                                   // gap distance between edges in a bundle

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this edge object to a string.
     */
    override def toString: String =
        val propStr = if prop == null then "()" else prop.mkString (", ")
        val fromStr = if from == null then "null-vertex" else s"Vertex(${from.id}, ${from.name})"
        val toStr   = if to   == null then "null-vertex" else s"Vertex(${to.id}, ${to.name})"
        s"Edge($id, $name, $fromStr, $propStr, $toStr, $shift)"
    end toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  When shift is non-zero, the endpoints for the edge must be shifted by shift * GAP
     *  in the direction orthogonal to the edge.
     *
    def calcEndPoints: (VectorD, VectorD) = 
        import scala.math.{atan2, cos, Pi, sin}
        val p1 = from.pos(0 to 2) + from.pos(2 to 4) / 2.0
        val p2 = to.pos(0 to 2)   + to.pos(2 to 4) / 2.0
        val an = atan2 (p2(1) - p1(1), p2(0) - p1(0)) + Pi / 2
        val del = (shift * GAP * cos (an), shift * GAP * sin (an))
        (VectorD (p1(0) + del._1, p1(1) + del._2),
         VectorD (p2(0) + del._1, p2(1) + del._2))
    end calcEndPoints
     */

end Edge


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Edge` companion object provides a factory function for creating edges.
 */
object Edge:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create an object of type `Edge`.
     *  @param from   the source vertex
     *  @param prop   maps edge's property names into property values
     *  @param to     the target vertex
     *  @param shift  number of units to shift to accomodate a bundle of egdes in a composite edge
     */
    def apply (from: Vertex, prop: Property, to: Vertex, shift: Int = 0): Edge =
        new Edge (null, from, prop, to, shift)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the edges.
     *  @param edges  the edges to show
     */
    def show (edges: VEC [Edge]): Unit =
        for e <- edges do println (e)
    end show

end Edge


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `edgeTest` main function is used to test the `Edge` class.
 *  > runMain scalation.database.graph.edgeTest
 */
@main def edgeTest (): Unit =

    val x0 = VectorD (200, 100, 40, 40)                           // vertex coordinates, size: x, y, w, h
    val x1 = VectorD (100, 400, 40, 40)
    val x2 = VectorD (500, 500, 40, 40)

    val v = VEC (
        new Vertex ("Bob", Map ("name" -> "Bob", "state" -> "GA", "salary" -> 85000.0), x0),
        new Vertex ("Sue", Map ("name" -> "Sue", "state" -> "FL", "salary" -> 95000.0), x1),
        new Vertex ("Joe", Map ("name" -> "Joe", "state" -> "GA", "salary" -> 99000.0), x2))

    // use the class constructor to explicitly assign edge label

    val edges = VEC (
        new Edge ("knows", v(0), Map ("type" -> "knows", "since" -> 5), v(1)),
        new Edge ("knows", v(1), Map ("type" -> "knows", "since" -> 2), v(0), -1),
        new Edge ("knows", v(2), Map ("type" -> "knows", "since" -> 4), v(0)))

    banner ("Edges with explicitly assigned edge labels")
    Edge.show (edges)

    // use the companion object's apply method to use system generated edge label

    val edges2 = VEC (
        Edge (v(0), Map ("type" -> "knows", "since" -> 5), v(1)),
        Edge (v(1), Map ("type" -> "knows", "since" -> 2), v(0), -1),
        Edge (v(2), Map ("type" -> "knows", "since" -> 4), v(0)))

    banner ("Edges with system generated edge labels")
    Edge.show (edges2)

end edgeTest

