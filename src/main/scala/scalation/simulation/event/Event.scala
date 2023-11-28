
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Jan 19 20:30:33 EST 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Event Contains Logic for Event Occurrences
 */

package scalation
package simulation
package event

import scalation.mathstat.Statistic

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Event` class provides facilities for defining simulation events.  Subclasses
 *  of Event provide event-logic in their implementation of the 'occur' method.
 *  Note: unique identification is mixed in via the `Identifiable` trait.
 *  @param entity    the entity involved in this event
 *  @param director  the controller/scheduler that this event is a part of
 *  @param delay     the time delay before this event's occurrence
 *  @param stat      the object for collecting statistics about delay times
 *  @param proto     the prototype (serves as node in animation) for this event
 */
abstract class Event (val entity: Entity, director: Model, delay: Double = 0.0,
                      stat: Statistic = null, val proto: EventNode = null)
         extends Identifiable with Ordered [Event]:

    /** The activation/occurrence time for the event
     */
    val actTime = director.clock + delay

    /** Live events will occur, cancelled ones will not
     */
    private var _live = true

    if stat != null then stat.tally (delay)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compare the activation times (actTime) of the two events ('ev' and 'this').
     *  Note: the order is reversed since Scala's Priority Queue is Highest Priority First (HPF).
     *  @param ev  the other event
     */
    def compare (ev: Event): Int = ev.actTime compare actTime

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Cancel 'this' event by marking it not live.
     */
    def cancel (): Unit = _live = false

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether this event is live (i.e., not cancelled).
     */
    def live: Boolean = _live

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Execute this event where the event-logic is specified in the 'occur' method.
     *  This method is abstract, so it must be implemented in subclasses and it
     *  (1) may schedule other events and (2) may specify state changes.
     */
    def occur (): Unit

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert the event to a string.
     */
    override def toString: String = entity.toString + "\t" + me

end Event

