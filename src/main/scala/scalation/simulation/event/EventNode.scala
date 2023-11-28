
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Jan 19 20:30:33 EST 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    EventNode Graphically Represent and Event Type
 */

package scalation
package simulation
package event

import scalation.animation.CommandType._
import scalation.scala2d.Colors._
import scalation.scala2d.Ellipse

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `EventNode` class provides facilities for representing simulation events
 *  graphically.  It extends the `Event` class with animation capabilities.
 *  Note:  unique identification is mixed in via the `Identifiable` trait in
 *  the `Event` superclass.
 *  @param director  the controller/scheduler that this event node is a part of
 *  @param at        the location of this event node
 */
class EventNode (director: Model, at: Array [Double] = Array ())
      extends Event (EventNode.makePrototype (director), director, -1.0, null):

    director.animate (this, CreateNode, blue, Ellipse (), at)

    def occur (): Unit = throw new NoSuchMethodException ("this occur should not be called")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display this node's outgoing `CausalLink`'s
     *  as edges.
     */
    def displayLinks (links: Array [CausalLink]): Unit =
        if links != null then for arc <- links do arc.display (this, arc.causedEvent)
    end displayLinks

end EventNode


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `EventNode` companion object provides a method for making a prototype
 *  entity.  A prototype event or entity is thought to exist before the
 *  simulation starts.
 */
object EventNode:

    def makePrototype (director: Model): Entity = Entity (-1.0, -1.0, director)

end EventNode

