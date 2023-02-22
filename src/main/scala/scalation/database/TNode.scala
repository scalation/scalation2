
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue Sep 13 22:53:44 EDT 2022 
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   `TNode` TNode in Tree
 */

package scalation.database

import scala.collection.mutable.ArrayBuffer

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TNode` object defines an indent function and flag indicating whether
 *  to indent.
 */
object TNode:

    var toIndent = false                                          // whether to indent

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Indent by the specified lavel.
     *  @param level  the number of levels of indentation
     */
    def indent (level: Int): Unit = if toIndent then print ("  " * level)

end TNode


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TNode` trait allows a hierarchy/tree structure to be created for
 *  any types that extend it, e.g., may be used to create a type hierarchy
 *  that follows single inheritence producing a multi-way tree.
 */
trait TNode:

    protected val subtypes = ArrayBuffer [TNode] ()                // all the children/subtypes

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the var-arg nodes to this subtype.
     *  @param ns  the nodes to be added
     */
    def add (ns: TNode*): Unit = subtypes ++= ns

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Visit each node in the multiway tree in pre-order, performing the given action.
     *  @see 
     *  @param action  the action to performed on each node
     */
    def visit [T >: TNode] (level: Int, action: T => Unit): Unit =
        TNode.indent (level)
        action (this)
        subtypes.map (_.visit (level+1, action))
    end visit

end TNode


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `tNodeTest` main function is used to test `TNode`.
 *  > runMain scalation.database.nodeTest
 */
@main def tNodeTest (): Unit =

    TNode.toIndent = true

    case class NamedTNode (name: String) extends TNode

    val n = for i <- 0 to 9 yield NamedTNode ("n" + i)

    n(0).add (n(1), n(2))
    n(1).add (n(3), n(4))
    n(2).add (n(5), n(6))
    n(3).add (n(7), n(8), n(9))

    n(0).visit (0, println)

end tNodeTest

