
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Mon Sep 27 15:03:10 EDT 2021
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Identifiable Provides a Means for Identifying Objects
 */

package scalation
package database

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Identifiable` trait provides unique identification for objects.
 *  @param _name  the name of the object
 *  @param id     the globally unique integer identifier (may use auto-gen)
 */
trait Identifiable (_name: String, val id: Int = Identifiable.next ()):

    val name = if _name == null then s"${typeName}_$id" else _name       // corrected name

    private val debug = debugf ("Identifiable", false)                   // debug function

    debug ("init", s"new object $me")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the type of the object.
     */
    def typeName: String = getClass.getSimpleName ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the full identity.
     */
    def me: String = s"$typeName.$name.$id"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Determine whether Identifiable object 'this' equals Identifiable object 'that'.
     *  Works since 'id' is unique for all Identifiable objects.
     */
    override def equals (that: Any): Boolean =
        that match
        case that: Identifiable => this.id == that.id
        case _ => false
        end match
    end equals

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the hashCode as the unique id.
     */
    override def hashCode: Int = id

end Identifiable


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Identifiable` object is used to generate unique identifiers.
 */
object Identifiable:

    /** Used for counter
     */
    private var i = 0

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the next value from the counter.
     */
    def next (): Int = { i += 1; i }

end Identifiable

