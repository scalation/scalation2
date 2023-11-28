
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Matthew Saltz
 *  @version 2.0
 *  @date    Thu Jul  9 14:47:27 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Supports Depth First Search (DFS) and
 *                    Breadth First Search (BFS)
 */

package scalation
package database
package graph_pm

import scala.collection.mutable.{Stack, Queue}
import scala.util.control.Breaks.{break, breakable}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `GraphDFS` performs Depth First Search (DFS) or Breadth First Search (BFS)
 *  on a Directed Graph.  The class currently supports three predicates:
 *      (1) to find a matching label, and
 *      (2) to see if a destination vertex is reachable.
 *  @param g    the directed graph to search
 *  @param bfs  switch from DFS to BFS, if bfs flag is true (defaults to false)
 */
class GraphDFS (g: Graph, bfs: Boolean = false):

    type STACK = Stack [Int]
    type QUEUE = Queue [Int]

    private val debug = debugf ("GraphDFS", false)                // debug function
    private val go    = Array.ofDim [Boolean] (g.size)            // go (unvisited) flags
    private val qu    = if bfs then new STACK ()                  // vertex FIFO queue for BFS
                        else new QUEUE ()                         // or stack (LIFO queue) for DFS
    private var lab: ValueType = null                             // label to find
    private var dest  = -1                                        // destination vertex to reach

    def pred1 (j: Int): Boolean = g.label(j) == lab               // predicate to find label
    def pred2 (j: Int): Boolean = j == dest                       // predicate to reach destination vertex

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the label lab in this graph, returning the first vertex id with
     *  a matching label, or -1 if vertex is not found.
     *  @param _lab  the label to search for
     */
    def find (_lab: ValueType): Int =
        var res = -1
        lab = _lab                                                // assign field for pred1 closure
        for i <- 0 until g.size do go(i) = true                   // set go flags to true

        breakable {
            for i <- 0 until g.size if go(i) do
                qu += i                                           // put unvisited vertex in queue
                res = visit (pred1)                               // visit vertices in DFS/BFS order
                if res >= 0 then break ()                         // return vertex where found
            end for
        } // breakable
        qu.clear (); res                                          // res; -1 =>  not found
    end find

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether vertex _dest is reachable from src, i.e., there is
     *  a directed path from vertex src to _dest.
     *  @param src    the source/starting vertex
     *  @param _dest  the destination/ending vertex
     */
    def reach (src: Int, _dest: Int): Boolean =
        dest = _dest                                              // assign field for pred2 closure
        for i <- 0 until g.size do go(i) = true                   // set go flags to true

        qu += src                                                 // put source vertex in queue
        val res = visit (pred2)                                   // visit vertices in DFS/BFS order
        if res >= 0 then { qu.clear (); return true }             // return true, if dest vertex found
        qu.clear (); false                                        // not found
    end reach

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of weakly connected components in the graph.
     *  In a weakly connected component, the vertices in the underlying
     *  undirected graph are connected.
     */
    def weakComps: Int =
        for i <- 0 until g.size do go(i) = true                   // set go flags to true
        var count = 0

        for i <- 0 until g.size if go(i) do
            qu += i                                               // put unvisited vertex in queue
            visit ()                                              // visit vertices in DFS/BFS order
            count += 1                                            // increment component count
        end for
        qu.clear (); count                                        // return strongly connected component count
    end weakComps

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Count the number of strongly connected components in the graph.
     *  In a strongly connected component, reach (u, v) is always true.
     */
    def strongComps: Int = 0                                      // FIX - to be implemented

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Visit vertices in DFS/BFS order, returning the id of the first vertex
     *  satisfying the predicate pred, or -1 if vertex is not found.
     *  @param pred  the predicate to satisfy
     */
    def visit (pred: Int => Boolean): Int =
        while qu.nonEmpty do
            val j = if bfs then qu.asInstanceOf [STACK].pop ()    // take next go vertex from LIFO queue
                    else qu.asInstanceOf [QUEUE].dequeue ()       // take next go vertex from FIFO queue

            if go(j) then
                debug ("visit", s"label($j) = ${pred(j)}")
                if pred (j) then return j                         // return vertex, if predicate satisfied
                go(j) = false                                     // mark as visited
                for c <- g.ch(j) if go(c) do qu += c              // put unvisited children in queue
            end if
        end while
        -1                                                        // not found
    end visit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Visit vertices in DFS/BFS order accessing children and parents (as
     *  if it is an undirected graph).
     *  @param pred  the predicate to satisfy
     */
    def visit (): Unit =
        if g.pa == null then g.addPar ()                          // make sure parent references exist
        while qu.nonEmpty do
            val j = if bfs then qu.asInstanceOf [STACK].pop ()    // take next go vertex from LIFO queue
                else qu.asInstanceOf [QUEUE].dequeue ()           // take next go vertex from FIFO queue

            if go(j) then
                debug("visit", s"label($j)")
                go(j) = false                                     // mark as visited
                for c <- g.ch(j) if go(c) do qu += c              // put unvisited children in queue
                for p <- g.ch(j) if go(p) do qu += p              // put unvisited parent in queue
            end if
        end while
    end visit

end GraphDFS


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `graphDFSTest` main function is used to test the `GraphDFS` class.
 *  > runMain scalation.database.graph_pm.graphDFSTest
 */
@main def graphDFSTest (): Unit =

    val g = ExampleGraphD.g1

    g.printG ()
    println ("Test DFS -----------------------------------------------------")
    test (new GraphDFS (g))                                        // test DFS
    println ("Test BFS -----------------------------------------------------")
    test (new GraphDFS (g, true))                                  // test BFS

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Test the find, reach and strongComps methods.
     *  @param gs  the graph search to test
     */
    def test (gs: GraphDFS): Unit =
        println ("Test find method")
        for lab <- 0 to 14 do
            println (s"find ($lab)  = ${gs.find (lab)}")           // find (lab)
        end for

        println ("Test reach method")
        for i <- 0 until g.size; j <- 0 until g.size do
            println (s"reach ($i, $j) = ${gs.reach (i, j)}")       // reach (i, j)
        end for

        println ("Test weakComps method")
        println (s"weakComps = ${gs.weakComps}")                   // weakComps

        println ("Test strongComps method")
        println (s"strongComps = ${gs.strongComps}")               // strongComps
    end test

end graphDFSTest

