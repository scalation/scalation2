
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Resource Provides Sevices to Entities/SimActors
 */

package scalation
package simulation
package process

import scala.collection.mutable.ListBuffer

import scalation.animation.CommandType._
import scalation.random.Variate
import scalation.scala2d.Rectangle
import scalation.scala2d.Colors._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Resource` class provides services to entities (`SimActors`).
 *  It may or may not have an associated waiting queue.
 *  @param name         the name of the resource
 *  @param line         the line/queue where entities wait
 *  @param units        the number of service units (e.g., bank tellers)
 *  @param serviceTime  the service time distribution
 *  @param at           the location of the resource (x, y, w, h)
 */
class Resource (name: String, line: WaitQueue, private var units: Int, serviceTime: Variate,
                at: Array [Double])
      extends Component:

    if units < 0 then flaw ("init", "resource may not have negative units")

    initComponent (name, at)

    private val flaw = flawf ("Resource")                          // flaw function

    /** Number of service units of this Resource currently in use (0 ... units)
     */
    private var inUse = 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor that uses defaults for width 'w' and height 'h'.
     *  @param name         the name of the resource
     *  @param line         the line/queue where entities wait
     *  @param units        the number of service units (e.g., bank tellers)
     *  @param serviceTime  the service time distribution
     *  @param xy           the (x, y) coordinates for the top-left corner of the resource.
     */
    def this (name: String, line: WaitQueue, units: Int, serviceTime: Variate,
              xy: (Double, Double)) =
        this (name, line, units, serviceTime, Array (xy._1, xy._2, 40.0, 30.0))
    end this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Change the number of units in this resource (e.g., add a teller).
     *  @param dUnits  the number of units to add (+ve) or remove (-ve)
     */
    def changeUnits (dUnits: Int): Unit =
        val actor = director.theActor
        director.log.trace (this, s"change units by $dUnits", actor, director.clock)
        if units + dUnits >= inUse then units += dUnits
        else flaw ("changeUnits", "attempt to reduce the resource to negative units")
    end changeUnits

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation engine to display this resource.
     */
    def display (): Unit =
        director.animate (this, CreateNode, orange, Rectangle (), at)
    end display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether this resource is busy (no units available).
     */
    def busy: Boolean = inUse == units

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Utilize this resource for a period of time (models an activity).
     *  The duration (service time) is randomly generated according to the
     *  resource's service time distribution.
     */
    def utilize (): Unit =
        if busy then flaw ("utilize", "no units available")
        val actor    = director.theActor
        val duration = serviceTime.gen                             // randomly generate
        tally (duration)                                           // collect sample statistics
        accum (inUse)                                              // collect persistent statistics
        director.log.trace (this, s"serves for $duration", actor, director.clock)
        director.animate (actor, MoveToken, null, null, Array (at(0) + DIAM, at(1) + at(3) / 2.0 - RAD))
        if inUse < units then inUse += 1 else flaw ("utilize", "no units left")
        actor.schedule (duration)
        actor.yieldToDirector ()
    end utilize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Utilize this resource for a given period of time (models an activity).
     *  @param duration  the given service time
     */
    def utilize (duration: Double): Unit =
        if busy then flaw ("utilize", "no units available")
        val actor = director.theActor
        tally (duration)                                           // collect sample statistics
        accum (inUse)                                              // collect persistent statistics
        director.log.trace (this, s"serves for $duration", actor, director.clock)
        director.animate (actor, MoveToken, null, null, Array (at(0) + DIAM, at(1) + at(3) / 2.0 - RAD))
        if inUse < units then inUse += 1 else flaw ("utilize", "no units left")
        actor.schedule (duration)
        actor.yieldToDirector ()
    end utilize

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Release this resource after service is finished (also check wait-queue).
     */
    def release (): Unit =
        val actor = director.theActor
        director.log.trace (this, "releases", actor, director.clock)
        if line != null && ! line.isEmpty then
            val waitingActor = line.dequeue ()
            waitingActor.schedule (0.0)
        end if
        accum (inUse)                                              // collect persistent statistics
        if inUse > 0 then inUse -= 1 else flaw ("release", "no units currently in use")
    end release

end Resource


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Resource` companion object provides a builder method for resources.
 */
object Resource:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a resource using defaults for width 'w' and height 'h'.
     *  @param name         the name of the resource
     *  @param line         the line/queue where entities wait
     *  @param units        the number of service units (e.g., bank tellers)
     *  @param serviceTime  the service time distribution
     *  @param xy           the (x, y) coordinates for the top-left corner of the resource.
     */
    def apply (name: String, line: WaitQueue, units: Int, serviceTime: Variate, xy: (Int, Int)): Resource =
        new Resource (name, line, units, serviceTime,
                      Array (xy._1.toDouble, xy._2.toDouble, 40.0, 30.0))
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a group of related resources using defaults for width 'w' and height 'h'.
     *  @param serviceTime  the service time distribution
     *  @param xy           the (x, y) coordinates for the top-left corner of the reference resource.
     *  @param rsc          repeated resource specific info: name, line, units, offset
     */
    def group (serviceTime: Variate, xy: (Int, Int),
               rsc: (String, WaitQueue, Int, (Int, Int))*): List [Resource] =
        val resourceGroup = new ListBuffer [Resource] ()
        for r <- rsc do resourceGroup += Resource (r._1, r._2, r._3, serviceTime,
                                                  (xy._1 + r._4._1, xy._2 + r._4._2))
        resourceGroup.toList
    end group

end Resource

