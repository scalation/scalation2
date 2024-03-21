
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Thu Nov  9 22:58:29 EST 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Support Functions for Database Normalization Based on Functional Dependencies
 */

package scalation
package database

import scala.collection.mutable.{SortedSet => SET}
import scala.runtime.ScalaRunTime.stringOf

type Attrs = SET [Char]

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `FD` case class is used to represent a Functional Dependency (x -> y).
 *  @param x  the LHS attributes of the FD
 *  @param y  the RHS attributes of the FD
 */
case class FD (x: Attrs, y: Attrs):

    override def toString: String = s"${x.toString.replace ("TreeSet", "")} -> ${y.toString.replace ("TreeSet", "")}"

end FD


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Normalization` class provides functions useful in normalization theory.
 *  These include closures, tests for superkeys, keys, losslessness and dependency
 *  preservation.
 *  @param r  the schema (complete set of attributes)
 *  @param f  the given functional dependencies
 */
class Normalization (r: Attrs, f: Array [FD]):

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the closure of z.
     *  @param z  the given set of attributes
     */
    def closure (z: Attrs): Attrs =
        val z_ = z.clone
        var changes = true
        while changes do
            changes = false
            for i <- f.indices do
                val (x, y) = (f(i).x, f(i).y)
                if (x subsetOf z_) && ! (y subsetOf z_) then { z_ ++= y; changes = true }
        end while
        z_
    end closure

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the restricted closure of z with respect to sub-schema ri.
     *  @param z   the given set of attributes
     *  @param ri  the given sub-schema (a subsetOf of r)
     */
    def rclosure (z: Attrs, ri: Attrs): Attrs = closure (z & ri) & ri

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the restricted closure of z obtained from db design p.
     *  @param z  the given set of attributes
     *  @param p  the set of sub-schemas defining the tables
     */
    def rclosure (z: Attrs, p: Array [Attrs]): Attrs =
        val z_ = z.clone
        var changes = true
        while changes do
            changes = false
            for ri <- p do
                val zi = rclosure (z_, ri)
                if ! (zi subsetOf z_) then { z_ ++= zi; changes = true }
        end while
        z_
    end rclosure

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the functional dependency fi is preserved by db design p.
     *  @param fi  the given function dependency
     *  @param p   the set of sub-schemas defining the tables
     */
    def preserve (fi: FD, p: Array [Attrs]): Boolean = fi.y subsetOf rclosure (fi.x, p)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the attributes z form a superkey for sub-schema ri.
     *  @param z   the given set of attributes
     *  @param ri  the given sub-schema (a subset of r, defaults to r)
     */
    def superkey (z: Attrs, ri: Attrs = r): Boolean = ri subsetOf closure (z)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the attributes z form a key for sub-schema ri.
     *  @param z   the given set of attributes
     *  @param ri  the given sub-schema (a subset of r, defaults to r)
     */
    def key (z: Attrs, ri: Attrs = r): Boolean =
        if ! superkey (z, ri) then return false
        z.forall ( (a: Char) => ! superkey (z &~ SET (a)) )
    end key

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the two sub-schemas { r1, r2 } represent a lossless decomposition
     *  using the Pairwise Losslessness Test.
     *  @param r1  the first sub-schema defining a table
     *  @param r2  the second sub-schema defining a table
     */
    def lossless (r1: Attrs, r2: Attrs): Boolean =
        val z = closure (r1 & r2)
        (r1 subsetOf z) || (r2 subsetOf z)
    end lossless

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether the db design p is lossless by checking that one of its
     *  sub-schemas contains a global key.
     *  @param p  the set of sub-schemas defining the tables
     */
    def lossless (p: Array [Attrs]): Boolean = p.exists (r subsetOf rclosure (_, p))

end Normalization


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `normalizationTest` main method test the `Normalization` class.
 *  For this example all FD are preserved.
 *  > runMain scalation.database.normalizationTest
 */
@main def normalizationTest (): Unit =

    val r = SET ('A', 'B', 'C', 'D')                             // R = ABCD
    val f = Array (FD (SET ('A', 'B'), SET ('C')),               // F = AB -> C
                   FD (SET ('A', 'D'), SET ('B', 'C')))          //     AD -> BC

    val db = Normalization (r, f)

    banner ("Schema r and Functional Dependencies f")
    println (s"r = $r")
    println (s"f = ${stringOf (f)}")

    banner ("Closure")
    println (s"closure (SET ('A', 'B')) = ${db.closure (SET ('A', 'B'))}")
    println (s"closure (SET ('A', 'D')) = ${db.closure (SET ('A', 'D'))}")

    val r1 = SET ('A', 'B', 'C')
    banner (s"Restricted Closure for r1 = $r1")
    println (s"rclosure (SET ('A', 'B'), r1) = ${db.rclosure (SET ('A', 'B'), r1)}")
    println (s"rclosure (SET ('A', 'D'), r1) = ${db.rclosure (SET ('A', 'D'), r1)}")

    val r2 = SET ('A', 'B', 'D')
    banner (s"Restricted Closure for r2 = $r2")
    println (s"rclosure (SET ('A', 'B'), r2) = ${db.rclosure (SET ('A', 'B'), r2)}")
    println (s"rclosure (SET ('A', 'D'), r2) = ${db.rclosure (SET ('A', 'D'), r2)}")

    val p = Array (r1, r2)
    banner (s"Restricted Closure for p = ${stringOf (p)}")
    println (s"rclosure (SET ('A', 'B'), p) = ${db.rclosure (SET ('A', 'B'), p)}")
    println (s"rclosure (SET ('A', 'D'), p) = ${db.rclosure (SET ('A', 'D'), p)}")

    banner (s"Is FD Preserved?")
    println (s"preserve (FD (SET ('A', 'B'), SET ('C'), p) = ${db.preserve (FD (SET ('A', 'B'), SET ('C')), p)}")
    println (s"preserve (FD (SET ('A', 'D'), SET ('B'), p) = ${db.preserve (FD (SET ('A', 'D'), SET ('B')), p)}")
    println (s"preserve (FD (SET ('A', 'D'), SET ('C'), p) = ${db.preserve (FD (SET ('A', 'D'), SET ('C')), p)}")

    banner (s"LHS of FD a Superkey?")
    println (s"superkey (SET ('A', 'B')) = ${db.superkey (SET ('A', 'B'))}")
    println (s"superkey (SET ('A', 'D')) = ${db.superkey (SET ('A', 'D'))}")

    banner (s"LHS of FD a Key?")
    println (s"key (SET ('A', 'B')) = ${db.key (SET ('A', 'B'))}")
    println (s"key (SET ('A', 'D')) = ${db.key (SET ('A', 'D'))}")

    banner ("Lossless?")
    println (s"lossless (r1, r2) = ${db.lossless (r1, r2)}")

end normalizationTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `normalizationTest2` main method test the `Normalization` class.
 *  For this example some FDs may not be preserved.
 *
 *  f = sid -> sname address phone       A -> BCD
 *      cid -> cname desc hours pid      E -> FGHI
 *      pid -> pname rank                I -> JK
 *      sid cid -> grade                 AE -> L
 *  
 *  Comment out line (1) => success
 *  Comment out line (2) => Dependency Preservation fails
 *  Comment out lines (1 and 2) => Losslessness and Dependency Preservation fail
 *
 *  > runMain scalation.database.normalizationTest2
 */
@main def normalizationTest2 (): Unit =

    val r = SET ('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'L')
    val f = Array (FD (SET ('A'), SET ('B', 'C', 'D')),
                   FD (SET ('E'), SET ('F', 'G', 'H', 'I')),
                   FD (SET ('I'), SET ('J', 'K')),
                   FD (SET ('A', 'E'), SET ('L')))
    val p = Array (SET ('A', 'B', 'C', 'D'),
                   SET ('E', 'F', 'G', 'H', 'I'),
                   SET ('E', 'J', 'K'),                           // (1) a BCNF Decomposition
//                 SET ('I', 'J', 'K'),                           // (2) 3NF Synthesis
                   SET ('A', 'E', 'L'))

    val db = Normalization (r, f)

    banner ("Schema r, Functional Dependencies f, and DB Design p")
    println (s"r = $r")
    println (s"f = ${stringOf (f)}")
    println (s"p = ${stringOf (p)}")

    banner ("LHS Superkey?")
    for fi <- f do
        val x = fi.x
        println (s"fi = $fi, closure ($x) = ${db.closure (x)}, " +
                          s"superkey ($x) = ${db.superkey (x)}")

    banner ("Lossless?")
    println (s"lossless (p) = ${db.lossless (p)}")

    banner ("Dependency Preservation?")
    for fi <- f do
        println (s"fi = $fi, preserve (fi, p) = ${db.preserve (fi, p)}")

end normalizationTest2

