
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue Aug 24 20:13:53 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Vectr Union Type for Vectors Used in Databases
 */

package scalation
package database
package relation

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Type definition for a union of column/vector types for `Double`, `Int`, `Long`, `String`,
 *  and `TimeNum`.
 */
type Vectr = VectorD | VectorI | VectorL | VectorS | VectorT

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Extension methods for the `Vectr` type.
 */
extension (x: Vectr)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Covert `Vectr` to a vector of double-precision reals `VectorD`.
     */
    def toDouble: VectorD =
        x match
        case _: VectorD => x.asInstanceOf [VectorD]
        case _: VectorI => x.asInstanceOf [VectorI].toDouble
        case _: VectorL => x.asInstanceOf [VectorL].toDouble
        case _: VectorS => x.asInstanceOf [VectorS].toDouble
        case _: VectorT => x.asInstanceOf [VectorT].toDouble
    end toDouble

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Covert `Vectr` to a vector of integers `VectorI`.
     */
    def toInt: VectorI =
        x match
        case _: VectorD => x.asInstanceOf [VectorD].toInt
        case _: VectorI => x.asInstanceOf [VectorI]
        case _: VectorL => x.asInstanceOf [VectorL].toInt
        case _: VectorS => x.asInstanceOf [VectorS].toInt
        case _: VectorT => x.asInstanceOf [VectorT].toInt
    end toInt

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Covert `Vectr` to a vector of long inetgers `VectorL`.
     */
    def toLong: VectorL =
        x match
        case _: VectorD => x.asInstanceOf [VectorD].toLong
        case _: VectorI => x.asInstanceOf [VectorI].toLong
        case _: VectorL => x.asInstanceOf [VectorL]
        case _: VectorS => x.asInstanceOf [VectorS].toLong
        case _: VectorT => x.asInstanceOf [VectorT].toLong
    end toLong

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Covert `Vectr` to a vector of strings `VectorS`.
     */
    def toString2: VectorS =
        x match
        case _: VectorD => x.asInstanceOf [VectorD].toString2
        case _: VectorI => x.asInstanceOf [VectorI].toString2
        case _: VectorL => x.asInstanceOf [VectorL].toString2
        case _: VectorS => x.asInstanceOf [VectorS]
        case _: VectorT => x.asInstanceOf [VectorT].toString2
    end toString2

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Covert `Vectr` to a vector of strings `VectorT`.
     */
    def toTimeNum: VectorT =
        x match
        case _: VectorD => x.asInstanceOf [VectorD].toTimeNum
        case _: VectorI => x.asInstanceOf [VectorI].toTimeNum
        case _: VectorL => x.asInstanceOf [VectorL].toTimeNum
        case _: VectorS => x.asInstanceOf [VectorS].toTimeNum
        case _: VectorT => x.asInstanceOf [VectorT]
    end toTimeNum

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Filter the vector according the predicate, return the index positions where it is true.
     *  @param p  the predicate function
     */
    def filterPos (p: ValueType => Boolean): collection.immutable.IndexedSeq [Int] =
        x match
        case _: VectorD => x.asInstanceOf [VectorD].filterPos (p)
        case _: VectorI => x.asInstanceOf [VectorI].filterPos (p)
        case _: VectorL => x.asInstanceOf [VectorL].filterPos (p)
        case _: VectorS => x.asInstanceOf [VectorS].filterPos (p)
        case _: VectorT => x.asInstanceOf [VectorT].filterPos (p)
    end filterPos

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the sub-vector of elements from the given index positions.
     *  @param pos  the given index positions
     */
    def at (pos: collection.immutable.IndexedSeq [Int]): Vectr =
        x match
        case _: VectorD => x.asInstanceOf [VectorD] (pos)
        case _: VectorI => x.asInstanceOf [VectorI] (pos)
        case _: VectorL => x.asInstanceOf [VectorL] (pos)
        case _: VectorS => x.asInstanceOf [VectorS] (pos)
        case _: VectorT => x.asInstanceOf [VectorT] (pos)
    end at 

