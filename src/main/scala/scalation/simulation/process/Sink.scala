
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Sink Terminates Entities/SimActors
 */

package scalation
package simulation
package process

import scala.collection.mutable.ListBuffer
import scala.runtime.ScalaRunTime.stringOf

import scalation.animation.CommandType._
import scalation.scala2d.Ellipse
import scalation.scala2d.Colors._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Sink` class is used to terminate entities `SimActor`s when they are finished.
 *  @param name  the name of the sink
 *  @param at    the location of the sink (x, y, w, h)
 */
class Sink (name: String, at: Array [Double])
      extends Component:

    initComponent (name, at)

    private val debug = debugf ("Sink", true)                          // debug function 

    debug ("init", s"located at ${stringOf (at)}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor that uses defaults for width 'w' and height 'h'.
     *  @param name  the name of the sink
     *  @param xy    the (x, y) coordinates for the top-left corner of the sink.
     */
    def this (name: String, xy: (Double, Double)) =
        this (name, Array (xy._1, xy._2, 20.0, 20.0))
    end this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display this Sink.
     */
    def display (): Unit =
        director.animate (this, CreateNode, darkred, Ellipse (), at)
    end display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Leave the model, effectively terminating the entity `SimActor`.
     */
    def leave (): Unit =
        val actor = director.theActor
        tally (director.clock - actor.arrivalT)
        director.log.trace (this, "terminates", actor, director.clock)
        director.animate (actor, MoveToken, null, null, Array (at(0) + DIAM, at(1) + at(3) / 2.0 - RAD))

        actor.yieldToDirector (true)          // yield and terminate
    end leave
    
end Sink


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Sink` companion object provides a builder method for sinks.
 */
object Sink:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a sink using defaults for width 'w' and height 'h'.
     *  @param name  the name of the sink
     *  @param xy    the (x, y) coordinates for the top-left corner of the sink.
     */
    def apply (name: String, xy: (Int, Int)): Sink =
        new Sink (name, Array (xy._1.toDouble, xy._2.toDouble, 20.0, 20.0))
    end apply

   //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a group of related sinks using defaults for width 'w' and height 'h'.
     *  @param xy   the (x, y) coordinates for the top-left corner of the reference sink.
     *  @param snk  repeated sink specific info: name, offset
     */
    def group (xy: (Int, Int),
               snk: (String, (Int, Int))*): List [Sink] =
        val sinkGroup = new ListBuffer [Sink] ()
        for s <- snk do sinkGroup += Sink (s._1, (xy._1 + s._2._1, xy._2 + s._2._2))
        sinkGroup.toList
    end group

end Sink

