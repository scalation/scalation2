
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep  7 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    WaitQueue Provides a Place for Entities/SimActors to Wait
 */

package scalation
package simulation
package process

import scala.collection.mutable.{ArrayBuffer => VEC, Queue}
import scala.runtime.ScalaRunTime.stringOf

import scalation.animation.CommandType._
import scalation.scala2d.Rectangle
import scalation.scala2d.Colors._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `WaitQueue` class is a wrapper for Scala's `Queue` class, which supports
 *  First-Come, First-Serve 'FCSC' Queues.  It adds monitoring capabilities and
 *  optional capacity restrictions.  If the queue is full, entities (`SimActor`s)
 *  attempting to enter the queue are 'barred'.  At the model level, such entities
 *  may be (1) held in place, (2) take an alternate route, or (3) be lost (e.g.,
 *  dropped call/packet). 
 *  @param name  the name of the wait-queue
 *  @param at    the location of the wait-queue (x, y, w, h)
 *  @param cap   the capacity of the queue (defaults to unbounded)
 */
class WaitQueue (name: String, at: Array [Double], cap: Int = Int.MaxValue)
      extends Queue [SimActor]
         with Component:

    initComponent (name, at)

    private val debug   = debugf ("WaitQueue", true)           // debug function

    protected var _barred = 0            // number of entities barred from entering due to the wait-queue being full

    debug ("init", s"name = $name, located at ${stringOf (at)}")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor that uses defaults for width (w) and height (h).
     *  @param name  the name of the wait-queue
     *  @param xy    the (x, y) coordinates for the top-left corner of the wait-queue.
     *  @param cap   the capacity of the queue (defaults to unbounded)
     */
    def this (name: String, xy: (Double, Double), cap: Int) =
        this (name, Array (xy._1, xy._2, 20.0, 20.0), cap)
    end this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether this queue is full.
     */
    def isFull: Boolean = length >= cap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number entities barred because of this wait-queue being full.
     */
    def barred: Int = _barred

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tell the animation queue to display this wait-queue.
     */
    def display (): Unit =
        director.animate (this, CreateNode, cyan, Rectangle (), at)
    end display

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Wait in the queue, recording the waiting time.  Return whether the entity
     *  was able to actually join the queue or was barred.
     */
    def waitIn (): Boolean =
        val actor  = director.theActor
        val timeIn = director.clock
        accum (size)                                           // collect for persistent statistics
        director.log.trace (this, "wait begins", actor, timeIn)

        val joined = if isFull then
            _barred += 1                                       // entity/actor barred
            false
        else
            super.+= (actor)                                   // entity/actor joins queue
            actor.yieldToDirector ()                           // indefinite delay
            true

        val timeOut = director.clock
        tally (timeOut - timeIn)                               // collect for sample/durations statistics
        accum (if joined then size + 1 else size)              // collect for persistent statistics
        director.log.trace (this, "wait ends", actor, timeOut)
        joined
    end waitIn

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Do not wait in the queue and record zero waiting time.  Call this method
     *  to get average waiting time for all actors.  If you just want the waiting
     *  time for those who wait, do not call this method.
     */
    def noWait (): Unit = tally (0.0)

end WaitQueue


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `WaitQueue` companion object provides a builder method for wait-queues.
 */
object WaitQueue:

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a wait-queue using integer-valued defaults for width (w) and height (h).
     *  @param name  the name of the wait-queue
     *  @param xy    the (x, y) coordinates for the top-left corner of the wait-queue.
     *  @param cap   the capacity of the queue (defaults to unbounded)
     */
    def apply (name: String, xy: (Int, Int), cap: Int = Int.MaxValue): WaitQueue =
        new WaitQueue (name, Array (xy._1.toDouble, xy._2.toDouble, 20.0, 20.0), cap)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a group of related wait-queues using defaults for width 'w' and height 'h'.
     *  @param xy   the (x, y) coordinates for the top-left corner of the reference queue.
     *  @param que  repeated queue specific info: name, offset
     */
    def group (xy:  (Int, Int),
               que: (String, (Int, Int))*): List [WaitQueue] =
        val queueGroup = new VEC [WaitQueue] ()
        for q <- que do queueGroup += WaitQueue (q._1, (xy._1 + q._2._1, xy._2 + q._2._2))
        queueGroup.toList
    end group

end WaitQueue

