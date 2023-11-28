
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Nov 15 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Controls the Event-Oriented Simulation
 */

package scalation
package simulation
package event

import scala.collection.mutable.{ListBuffer, PriorityQueue}
import scala.runtime.ScalaRunTime.stringOf

import scalation.animation.{AnimateCommand, CommandType, DgAnimator}
import scalation.animation.CommandType._
import scalation.mathstat.Statistic
import scalation.scala2d._
import scalation.scala2d.Colors._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Model` class schedules events and implements the time advance mechanism
 *  for simulation model following the event-scheduling world view.
 *  @param name       the name of the model
 *  @param reps       the number of independent replications to run (to be implemented)
 *  @param animating  whether to animate the model (only for Event Graphs)
 */
class Model (name: String, reps: Int = 1, animating: Boolean = false)
      extends Modelable with Identifiable:

    private val debug = debugf ("Model", true)                        // debug function
    private val flaw  = flawf ("Model")                               // flaw function
    private [event] val log  = Monitor ("simulation")                 // log for model execution

    /** The future event list (time-ordered list of events)
     */
    private val eventList = PriorityQueue.empty [Event]
//  private val eventList = new PQueue [Event] () 

    private val stats = ListBuffer [Statistic] ()

    /** The time in sYstem statistics
     */
    private val t_y_stat = new Statistic ("t_y")

    /** The animation engine
     */
    private val dgAni = if animating then new DgAnimator ("Event Animator", black, white) else null

    /** The animation engine's command queue
     */
    private val aniQ = if animating then dgAni.getCommandQueue else null

    /** The start time for the simulation
     */
    protected var start = -1.0

    if reps > 1 then flaw ("init", "multiple replications not implemented yet")

    addStats (t_y_stat)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add statistical collector to the model.
     *  @param stat  one or more statistical collectors
     */
    def addStats (stat: Statistic*): Unit = for st <- stat do stats += st

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Place an event on the Future Event List 'FEL' for later execution, thus
     *  scheduling the event to occur sometime in the future.  Events are ordered
     *  by their event/act time.
     *  @param event  the event to schedule
     */
    def schedule (event: Event): Unit =
        eventList += event
//      println ("event.proto = " + event.proto)
        if animating then aniQ.add (AnimateCommand (CreateToken, event.id, Ellipse (),
                                    "ev" + event.id, false, black, null, _clock, event.proto.id))
    end schedule

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Cancel the specified 'event' so it will not occur.
     *  @param event  the event to cancel
     */
    def cancel (event: Event): Unit = event.cancel ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Leave the model recording the entity's time in the sYstem.
     *  @param entity  the entity leaving the model
     */
    def leave (entity: Entity): Unit = t_y_stat.tally (clock - entity.arrivalT)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Run the simulation by iteratively processing events in time order.
     *  Requires at least one to already be scheduled (see next method).
     *  @param startTime   the time at which the simulation is to begin
     */
    def simulate (startTime: Double = 0.0): Unit =
        start  = startTime
        _clock = startTime
        log.trace (this, "starts", this, _clock)

        var nextEvent: Event = null                                    // the next event
        var nextEnt: Entity  = null                                    // the next entity
        simulating = true
        if eventList.isEmpty then flaw ("simulate", "eventList must not be empty at start")

        while simulating && ! eventList.isEmpty do
            nextEvent = eventList.dequeue ()
            if nextEvent.live then
                _clock  = nextEvent.actTime
                nextEnt = nextEvent.entity
                log.trace (this, s"executes ${nextEvent.me} on ${nextEnt.eid}", nextEnt, _clock)
                debug ("simulate", s"$nextEvent \t" + "%g".format (_clock))
                nextEvent.occur ()
            end if
            if animating then aniQ.add (AnimateCommand (DestroyToken, nextEvent.id, null,
                                        null, false, null, null, _clock, nextEvent.proto.id))
        end while

        if animating then dgAni.animate (0, 100000)
        log.trace (this, "terminates", this, _clock)
        log.finish ()
    end simulate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Report values of the specified model result/output variables.
     *  @param vars  the result/output variables for the simulation
     */
    def report (vars: (String, Double)*): Unit =
        banner (s"Results for $name model executed over [$start, $_clock]")
        for v <- vars do println ("result: " + v._1 + " \t= " + v._2)
    end report

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Report statistical results of the simulation for all the collectors.
     */
    def reportStats (): Unit =
        banner ("Statistical results for " + name)
        println (Statistic.line)
        println (Statistic.labels)
        println (Statistic.line)
        for st <- stats do println (st)
        println (Statistic.line)
        banner ("Compute adjusted wait time t_q by multiplying be num ratio")
    end reportStats

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the statistical results of the simulation (statistics for each part).
     */
    def getStatistics: ListBuffer [Statistic] = stats

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put a node/token command on the animation queue.
     *  @param who    who is being animated
     *  @param what   what animation command
     *  @param color  the color the node/token
     *  @param shape  the shape of the node/token
     *  @param at     the location of the node/token
     */
    def animate (who: Identifiable, what: CommandType, color: Color, shape: Shape, at: Array [Double]): Unit =
        if animating then
            val eid   = who.id
            val label = who.name
            println (s"Model.animate: $label.$eid $what $color $shape ${stringOf (at)}")
            aniQ.add (AnimateCommand (what, eid, shape, label, true, color, at, _clock))
        end if
    end animate

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put a edge command on the animation queue.
     *  @param who    who is being animated
     *  @param what   what animation command
     *  @param color  the color the edge
     *  @param shape  the shape of the edge
     *  @param from   the location of the origination node
     *  @param to     the location of the destination node
     *  @param at     the location of the edge (empty array => implicitly determined)
     */
    def animate (who: Identifiable, what: CommandType, color: Color,
                 shape: Shape, from: Event, to: Event, at: Array [Double] = Array ()): Unit =
        if animating then
            val eid   = who.id
            val label = who.name
            println (s"Model.animate: $label.$eid $what $color $shape ${from.me} ${to.me} ${stringOf (at)}")
            aniQ.add (AnimateCommand (what, eid, shape, label, true, color, at, _clock, from.id, to.id))
        end if
    end animate

end Model

