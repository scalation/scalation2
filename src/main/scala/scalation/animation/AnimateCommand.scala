
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep 21 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Animation Commands
 */

package scalation
package animation

import scala.runtime.ScalaRunTime.stringOf

import scalation.scala2d._
import scalation.scala2d.Colors.Color

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `CommandType` enumeration specifies the types commands passed from a simulation
 *  engine to the animation engine.  A message specifies one of the commands defined
 *  in the Animator interface.
 */
enum CommandType (val name: String):

    case CreateNode      extends CommandType ("CreateNode")
    case CreateEdge      extends CommandType ("CreateEdge")
    case CreateToken     extends CommandType ("CreateToken")
    case DestroyNode     extends CommandType ("DestroyNode")
    case DestroyEdge     extends CommandType ("DestroyEdge")
    case DestroyToken    extends CommandType ("DestroyToken")
    case MoveNode        extends CommandType ("MoveNode")
    case MoveToken       extends CommandType ("MoveToken")
    case MoveToken2Node  extends CommandType ("MoveToken2Node")
    case MoveTokens2Node extends CommandType ("MoveTokens2Node")
    case MoveToken2Edge  extends CommandType ("MoveToken2Edge")
    case ScaleNode       extends CommandType ("ScaleNode")
    case ScaleToken      extends CommandType ("ScaleToken")
    case ScaleTokensAt   extends CommandType ("ScaleTokensAt")
    case SetPaintNode    extends CommandType ("SetPaintNode")
    case SetPaintEdge    extends CommandType ("SetPaintEdge")
    case SetPaintToken   extends CommandType ("SetPaintToken")
    case TimeDilation    extends CommandType ("TimeDilation")

end CommandType


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AnimateCommand` class provides a data structure for holding animation
 *  command specifications.
 *  @param action    the animation action to perform
 *  @param eid       the external id for the component acted upon
 *  @param shape     the shape of graph component (node, edge or token)
 *  @param label     the display label for the component
 *  @param primary   whether the component is primary (true) or secondary (false)
 *  @param color     the color of the component
 *  @param pts       the set points/dimensions giving the shapes location and size
 *  @param time      simulation time when the command is to be performed
 *  @param from_eid  the 'eid' of the origination node (only for edges)
 *  @param to_eid    the 'eid' of the destination node (only for edges)
 *  @param shift     amount of distance to shift the shape to be resolved by an angle
 *                   e.g. to accommodate a bundle of edges in a composite edge( only for edges)
 */
case class AnimateCommand (action: CommandType, eid: Int, shape: Shape, label: String,
                           primary: Boolean, color: Color, pts: Array [Double], time: Double,
                           from_eid: Int = -1, to_eid: Int = -1, shift: Int = 0):

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** This method compares two `AnimateCommand` objects to see which one has
     *  the most recent timestamp.
     *  @param command2  the animate command to compare this to
     */
    def compare (command2: AnimateCommand) = time compare command2.time

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the elements of the array and handle the null case.
     *  @param array  the array to be shown
     */
    def show (array: Array [Double]) = 
       if array == null then "Array ( null )" else stringOf (array)
    end show

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the command to a string representation useful to printing/debugging.
     */
    override def toString =
        val __ = " , "
        s"AnimateCommand ( $action __ $eid __ $shape __ $label __ $primary __ $color __" +
                         s"${show (pts)} __ $time __ $from_eid __ $to_eid )"
    end toString

end AnimateCommand

