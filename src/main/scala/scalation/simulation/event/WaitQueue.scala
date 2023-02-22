
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Feb  5 19:43:10 EST 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   FCFS Wait Queue holds Entities Waiting for Service
 */

package scalation
package simulation
package event

import scala.collection.mutable.{ArrayBuffer, Queue}

import scalation.mathstat.{Statistic, TimeStatistic}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `WaitQueue` class is used to hold entities waiting for service and
 *  collect statistics on waiting times.  When the queue is full, entities
 *  are 'barred' from entering the queue.
 *  @param director  the controller/scheduler that this event is a part of
 *  @param ext       the extension to distinguish the wait queues
 *  @param cap       the capacity of the queue (defaults to unbounded)
 */
case class WaitQueue (director: Model, ext: String = "", cap: Int = Int.MaxValue)
     extends Queue [Entity]:

    /** The number of entities barred from entering due to the wait queue being full
     */
    private var _barred = 0

    /** The time in Queue (t_q) sample statistics
     */
    val t_q_stat = new Statistic ("t_q" + ext)

    /** The number in Queue (l_q) time-persistent statistics
     */
    val l_q_stat = new TimeStatistic ("l_q" + ext)

    /** The waiting times for all entities that wait
     */
    val waitTimes = ArrayBuffer [Double] ()

    director.addStats (t_q_stat, l_q_stat)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the number entities barred because of this wait queue being full.
     */
    def barred: Int = _barred

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether the queue is full (at capacity).
     *  Note, isEmpty is inherited from the `Queue` class.
     */
    def isFull: Boolean = length >= cap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Enqueue the entity 'ent' in the wait queue and record when waiting started.
     *  @param ent  the entity to be enqueued
     */
    override def enqueue (ent: Entity): WaitQueue.this.type =
        ent.startWait = director.clock
        if length <= cap then super.enqueue (ent) else _barred += 1
        this
    end enqueue

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Dequeue the first entity from the wait queue and record the time in the
     *  wait queue.
     */
    override def dequeue (): Entity =
        val ent = super.dequeue ()
        val timeInQ = director.clock - ent.startWait
        waitTimes += timeInQ
        director.log.trace (director, s"records $timeInQ wait for ${ent.eid}", ent, director.clock)
        t_q_stat.tally (timeInQ)
        l_q_stat.accum (length + 1, director.clock)
        ent
    end dequeue

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Summarize the time waiting in this queue.
     *  @param numEntities  the number of entities entering or skipping the queue
     */
    def summary (numEntities: Int): Unit =
        val numWaiting = waitTimes.length
        val sumWaiting = waitTimes.sum
        println (s"numEntities   = $numEntities")
        println (s"numWaiting    = $numWaiting")
        println (s"waitTimes avg = ${sumWaiting / numWaiting}")
        println (s"waitTimes adj = ${sumWaiting / numEntities}")
    end summary

end WaitQueue

