
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep 20 15:47:16 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Statistical Trait
 */

package scalation
package simulation.agent

import scala.collection.mutable.{ArrayBuffer => VEC}

import scalation.database.Identifiable
import scalation.mathstat.{Statistic, TimeStatistic, VectorD}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Statistical` trait maintain statistical collectors.
 *  @param name  the name of this statistical object
 */
trait Statistical (name: String):

    private val sampStatistic = new Statistic (name)              // sample statistics
    private val persStatistic = new TimeStatistic (name)          // time-persistent statistics

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tally sample statistics.
     *  @param duration  the time duration to tally
     */
    private [agent] def tallyStats (duration: Double): Unit =
        sampStatistic.tally (duration)
    end tallyStats

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Accumulate time-persistent statistics.
     *  @param count  the numerical count to accumulate
     *  @param time   the relevant time
     */
    private [agent] def accumStats (count: Int, time: Double): Unit =
        persStatistic.accum (count, time)
    end accumStats

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Tally sample statistics and accumulate time-persistent statistics.
     *  @param duration  the time duration to tally
     *  @param count     the numerical count to accumulate
     *  @param time      the relevant time
     */
    private [agent] def collectStats (duration: Double, count: Int, time: Double): Unit =
        sampStatistic.tally (duration)
        persStatistic.accum (count, time)
    end collectStats

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the sample statistics and time-persistent statistic to the list.
     *  @param list  the collection of statistical objects
     */
    private [agent] def addStats (list: VEC [Statistic]): Unit =
        list += sampStatistic
        list += persStatistic
    end addStats

end Statistical

