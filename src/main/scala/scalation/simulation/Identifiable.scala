
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue Sep  15 15:05:06 EDT 2009
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Identifiable Provides a Means for Identifying Simulation Components
 */

package scalation
package simulation

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Identifiable` trait provides unique identification for simulation components,
 *  entities and events.  Includes a mandatory id and an optional name.
 */
trait Identifiable:

    import Identifiable.next

    /** The flaw function
     */
    private val flaw = flawf ("Identifiable")

    /** The globally unique integer identifier
     */
    val id = next ()

    /** The given name (assigned once)
     */
    private var _name = ""

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the name.
     */
    def name: String = _name

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the name.
     *  @param name  the name to assign
     */
    def name_= (name: String): Unit =
        if _name == "" && name != null then _name = name
        else flaw ("name_=", "name may only be set once")
    end name_=

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Get the type of the simulation object.
     */
    def simType: String = getClass.getSimpleName ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the full identity.
     */
    def me: String = s"$simType.$_name.$id"

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

