
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Mar 13 21:00:26 EDT 2024
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Extension Method for Scala's Mutable Sets
 */

package scalation

import scala.collection.mutable.Set

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Extend `Set` to include unicode symbols for subset and proper subset (⊆, ⊂, ⊈, ⊄).
 */
extension [T] (x: Set [T])

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether set x is a subset of set y.
     *  @param y  the other set
     */
    inline def ⊆ (y: Set [T]): Boolean = x subsetOf y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether set x is a proper subset of set y.
     *  @param y  the other set
     */
    inline def ⊂ (y: Set [T]): Boolean = (x subsetOf y) && x != y

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether set x is not a subset of set y.
     *  @param y  the other set
     */
    inline def ⊈ (y: Set [T]): Boolean = ! (x subsetOf y)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether set x is not a proper subset of set y.
     *  @param y  the other set
     */
    inline def ⊄ (y: Set [T]): Boolean = ! (x subsetOf y) || x == y


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `setExtTest` main function test the SetExt extention methods.
 *  > runMain scalation.setExtTest
 */
@main def setExtTest (): Unit =

    val x = Set (1, 2)
    val y = Set (1, 2, 3)

    println (s"x = $x")
    println (s"y = $y")

    println (s"x ⊆ y = ${x ⊆ y}")
    println (s"x ⊆ x = ${x ⊆ x}")

    println (s"x ⊂ y = ${x ⊂ y}")
    println (s"x ⊂ x = ${x ⊂ x}")

    println (s"x ⊈ y = ${x ⊈ y}")
    println (s"x ⊈ x = ${x ⊈ x}")

    println (s"x ⊄ y = ${x ⊄ y}")
    println (s"x ⊄ x = ${x ⊄ x}")

end setExtTest

