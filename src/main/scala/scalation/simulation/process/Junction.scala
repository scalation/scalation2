
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Nov 21 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Junction/Connector between Transports/Routes
 */

package scalation
package simulation
package process

import scala.collection.mutable.ListBuffer
import scala.runtime.ScalaRunTime.stringOf

import scalation.animation.CommandType._
import scalation.random.Variate
import scalation.scala2d.Ellipse
import scalation.scala2d.Colors._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Junction` class provides a connector between two `Transport`s/`Route`s.
 *  Since `Lines` and `QCurves` have limitations (e.g., hard to make a loop back),
 *  a junction may be needed.
 *  @param name   the name of the junction
 *  @param jTime  the jump-time through the junction
 *  @param at     the location of the junction (x, y, w, h)
 */
class Junction (name: String, jTime: Variate, at: Array [Double])
      extends Component:

    initComponent (name, at)

    private val debug = debugf ("Junction", true)                    // debug function 

    debug ("init", s"located at ${stringOf (at)}")

    private var onJunction = 0            // the number of entities/sim-actors on this Junction

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor that uses defaults for width 'w' and height 'h'.
     *  @param name      the name of the junction
     *  @param jTime     the jump-time through the junction
     *  @param xy        the (x, y) coordinates for the top-left corner of the junction
     */
    def this (name: String, jTime: Variate, xy: (Double, Double)) =
        this (name, jTime, Array (xy._1, xy._2, 10.0, 10.0))
    end this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display this Junction.
     */
    def display (): Unit =
        director.animate (this, CreateNode, purple, Ellipse (), at)
    end display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Jump the entity `SimActor` from the incoming "from" transport to the
     *  the middle of the junction.
     */
    def jump (): Unit =
        val actor    = director.theActor
        val duration = jTime.gen
        tally (duration)
        accum (onJunction)
        onJunction += 1
        director.log.trace (this, s"jump for $duration", actor, director.clock)

        director.animate (actor, MoveToken, null, null, Array (at(0) + RAD, at(1) + RAD))
        actor.schedule (duration)
        actor.yieldToDirector ()
        accum (onJunction)
        onJunction -= 1
    end jump
    
end Junction


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Junction` companion object provides a builder method for junctions.
 */
object Junction:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a junction using defaults for width 'w' and height 'h'.
     *  @param name   the name of the junction
     *  @param jTime  the jump-time through the junction
     *  @param xy     the (x, y) coordinates for the top-left corner of the junction
     */
    def apply (name: String, jTime: Variate, xy: (Int, Int)): Junction =
        new Junction (name, jTime, Array (xy._1.toDouble, xy._2.toDouble, 10.0, 10.0))
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a group of related junctions using defaults for width 'w' and height 'h'.
     *  @param jTime     the jump-time through the junction
     *  @param xy        the (x, y) coordinates for the top-left corner of the reference junction.
     *  @param jnt       repeated junction specific info: name, offset
     */
    def group (jTime: Variate, xy: (Int, Int),
               jnt: (String, (Int, Int))*): List [Junction] =
        val junctionGroup = new ListBuffer [Junction] ()
        for j <- jnt do junctionGroup += Junction (j._1, jTime, (xy._1 + j._2._1, xy._2 + j._2._2))
        junctionGroup.toList
    end group

end Junction

