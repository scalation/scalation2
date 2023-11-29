
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Jun 11 13:25:46 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Fixed Size Circular Array (Ring)
 */

package scalation

import scala.reflect.ClassTag
import scala.runtime.ScalaRunTime.stringOf

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Ring` class provides a circular array that can be used to store the
 *  latest cap elements.  Example to add elements 1, 2, 3, 4, 5, 6, 7 keeping
 *  the most recebt 5:
 *  [ 1, 2, 3, 4, 5 ]   f = 0, l = 4
 *  [ 6, 7, 3, 4, 5 ]   f = 2, l = 1
 *  @param cap   the capacity or maximum number of elements that can be stored
 *  @param zero  an element that indicates zero for type A
 */
class Ring [A: ClassTag] (cap: Int, zero: A):

    private val flaw  = flawf ("Ring")                   // flaw function
    private var f     = 0                                // index first element
    private var l     = -1                               // index of last element
    private var full  = false                            // whether the ring is full (has no open locations)
    private val store = Array.fill [A](cap)(zero)        // storage space for elements

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the i-th element in logical order [ old, ..., recent ].
     *  Calling apply(0) returns the oldest element stored.
     *  @param i  the logical index
     */
    def apply (i: Int): A =
        val j = (f + i) % cap                            // access i locations past f
        if ! (j in (0, cap-1)) then flaw ("apply", s"indices i = $i, j = $j out of bounds")
        store(j)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the i-th element in logical order [ old, ..., recent ].
     *  Calling apply(0) returns the oldest element stored.
     *  @param i     the logical index
     *  @param elem  the element to be assigned
     */
    def update (i: Int, elem: A): Unit =
        val j = (f + i) % cap                            // access i locations past f
        if ! (j in (0, cap-1)) then flaw ("apply", s"indices i = $i, j = $j out of bounds")
        store(j) = elem
    end update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add an element into the next available location which will be either the
     *  next open location or the oldest location in use (over-writing the element
     *  at that location).
     *  @param elem  the next element to be added to the ring
     */
    def add (elem: A): Unit = 
        if l == cap - 1 then full = true                 // no open locations left
        l = (l + 1) % cap                                // next available location
        store(l) = elem                                  // store the next element here
        if full && l == f then f = (f + 1) % cap         // over-written => advance f
    end add

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset/clear the ring.
     */
    def reset (): Unit =
        f = 0; l = -1; full = false
    end reset

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show/print the elements in the ring in logical order.
     */
    def show (): Unit =
        println ("-" * 60)
        print ("Ring: ") 
        for j <- 0 until cap do print (s"${this(j)}, ")
        println (s" \t f = $f, l = $l")
        println ("-" * 60)
    end show

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this ring into a string showing the storage/physical view.
     */
    override def toString: String = s"Ring (${stringOf (store)})"

end Ring


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ringTest` main function is used to test the `Ring` class.
 *  > runMain scalation.ringTest
 */
@main def ringTest (): Unit =

    val a = new Ring [Int](5, 0)
    a.add (1); a.show (); println (a)
    a.add (2); a.show (); println (a)
    a.add (3); a.show (); println (a)
    a.add (4); a.show (); println (a)
    a.add (5); a.show (); println (a)
    a.add (6); a.show (); println (a)
    a.add (7); a.show (); println (a)

end ringTest

