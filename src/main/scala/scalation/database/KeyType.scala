
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Jul  4 12:55:00 EDT 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    KeyType for Handling Non-Composite and Composite Keys
 */

package scalation
package database

import scala.collection.mutable.ArrayBuffer
import scala.util.control.Breaks.{break, breakable}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KeyType` class provides a key type for handling both non-composite and
 *  composite keys.  A key is a minimal set of attributes that can be used to
 *  uniquely identify a tuple.
 *  @param key  the key values stored in an array buffer
 */
case class KeyType (key: ArrayBuffer [ValueType]):

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor allowing a variable number of arguments.
     *  @param ky  the values making up the key as a variable-argument list
     */
    def this (ky: ValueType*) = this (ArrayBuffer (ky :_*))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor allowing the key values to passed in as a tuple.
     *  @param ky  the key values in a Tuple = Array [ValueType]
     */
    def this (ky: Tuple) = this (ky.to (ArrayBuffer))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Auxiliary constructor allowing the key values to passed in as a row.
     *  @param ky  the key values in a Row = Vector [ValueType]
     */
    def this (ky: Row) = this (ky.to (ArrayBuffer))

end KeyType


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KeyType` companion object provides `Ordering` for `KeyType` via the compare method.
 *  Create a given instance for use as an implicit parameter.
 */
object KeyType:

    given Ordering [KeyType] with
        def compare (x: KeyType, y: KeyType): Int =
            var res = 0
            breakable {
                for j <- x.key.indices do
                    if x.key(j) < y.key(j) then { res = -1; break () }
                    if x.key(j) > y.key(j) then { res =  1; break () }
                end for
            } // breakable
            res 
        end compare
    end given

end KeyType

