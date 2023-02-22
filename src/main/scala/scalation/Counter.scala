
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Jan  1 13:54:44 EST 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Class used for Counting
 */

package scalation

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Counter` class supports the creation of counters.
 *  @param value_  the initial value for the counter
 */
class Counter (value_ : Int = 0):

    private var value: Int = value_

    def reset (value_ : Int = 0): Unit = value = value_
    def inc (): Int = { value += 1; value }
    def dec (): Int = { value -= 1; value }
    def get: Int = value
    override def toString: String = value.toString

end Counter


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `counterTest` main function test the `Counter` class.
 *  > runMain scalation.counterTest
 */
@main def counterTest (): Unit =

    val cnt = new Counter ()

    cnt.inc (); cnt.inc (); cnt.inc ()
    cnt.dec (); cnt.dec ()

    println (s"cnt = $cnt")

end counterTest

