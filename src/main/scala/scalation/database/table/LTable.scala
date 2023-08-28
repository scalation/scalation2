
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Jul 22 00:20:15 EDT 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Linkable-Relational Algebra (LRA) for Linkable-Relational DBMS
 */

package scalation
package database
package table 

import scala.collection.mutable.Map
import scala.runtime.ScalaRunTime.stringOf

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LTable` companion object provides factory methods for creating linkable-tables.
 *  Supported domains/data-types are 'D'ouble, 'I'nt, 'L'ong, 'S'tring, and 'T'imeNum.
 */
object LTable:

    private val debug = debugf ("LTable", true)                             // debug function
    private val flaw  = flawf ("LTable")                                    // flaw function
    private val cntr  = Counter ()                                          // counter for generating unique names

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a linkable-table given convenient string specifications.
     *  @param name     the name of the linkable-table
     *  @param schema   the attributes for the linkable-table
     *  @param domain_  the domains/data-types for attributes ('D', 'I', 'L', 'S', 'X', 'T')
     *  @param key      the attributes forming the primary key
     */
    def apply (name: String, schema: String, domain_ : String, key: String): LTable =
        new LTable (name, strim (schema), strim (domain_).map (_.head), strim (key))
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a new empty linkable-table with the same schema as an existing table.
     *  @param name  the name of the new linkable-table
     *  @param tab   the existing table
     */
    def apply (name: String, tab: Table): LTable =
        new LTable (name, tab.schema, tab.domain, tab.key)
    end apply

end LTable

import LTable.{cntr, debug, flaw}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `LTable` class (linkable-table) stores linkable-relational data and implements
 *  linkable-relational algebra operators.
 *  Supported domains/data-types are 'D'ouble, 'I'nt, 'L'ong, 'S'tring, and 'T'imeNum.
 *  @param name_    the name of the linkable-table
 *  @param schema_  the attributes for the linkable-table
 *  @param domain_  the domains/data-types for attributes ('D', 'I', 'L', 'S', 'X', 'T')
 *  @param key_     the attributes forming the primary key
 */
case class LTable (name_ : String, schema_ : Schema, domain_ : Domain, key_ : Schema)
     extends Table (name_, schema_, domain_, key_)
        with Serializable:

    private val links = Map [String, Map [ValueType, Tuple]] ()             // fkey -> pkey links

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add LINKAGE (foreign key reference) from this table to refTab and for each
     *  tuple in this table, add a link to the referenced table, so that the foreign
     *  key is linked to the primary key.
     *  Caveat: does not handle composite foreign keys
     *  @param fkey    the foreign key column
     *  @param refTab  the referenced table being linked to
     */
    override def addLinkage (fkey: String, refTab: Table): Unit =
        if ! refTab.hasIndex then refTab.create_index ()                    // make sure refTab has a primary index
        links += fkey -> Map [ValueType, Tuple] ()                          // establish links map for fkey
        for t <- tuples do addLink (fkey, t, refTab)                        // add link for each tuple
    end addLinkage

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For the given tuple t, add a link to the referenced table, so that the
     *  foreign key is linked to the primary key.
     *  Caveat: does not handle composite foreign keys
     *  @param fkey    the foreign key attribute/column
     *  @param t       the tuple containing the foreign key
     *  @param refTab  the referenced table being linked to
     */
    def addLink (fkey: String, t: Tuple, refTab: Table): Unit =
        val t_fkey = pull (t, fkey)
        val refTup = refTab.index.getOrElse (new KeyType (t_fkey), null)
        if refTup == null then
            flaw ("addLink", s"$name: referential integrity violation for fkey = $fkey, value = $t_fkey")
        else
            val rTup = refTup.asInstanceOf [Tuple]
            debug ("addLink", s"$name: foreign key = $fkey add $t_fkey -> ${stringOf (rTup)}")
            links(fkey) += t_fkey -> rTup
        end if
    end addLink

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** For the given tuple t, remove the link for the given foreign key fkey,
     *  returning whether the removal was successful.
     *  @param fkey  the foreign key attribute/column
     *  @param t     the tuple containing the foreign key
     */
    def removeLink (fkey: String, t: Tuple): Boolean =
        val t_fkey = pull (t, fkey)
        links(fkey).remove (t_fkey).isDefined
    end removeLink

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the EQUI-JOIN via direct LINKS from this linkable-table to the referenced table
     *  keeping concatenated tuples that are equal on the primary key and foreign key attributes.
     *  Caveat:  Requires the foreign key table to be first [ fkey_table join ((fkey, pkey_table) ].
     *  Usage:   deposit join (("cname", customer))
     *  @param ref  the foreign key reference (foreign key attribute, referenced table)
     */
    override def join (ref: (String, Table)): LTable =
        val (fkey, refTab) = ref                                            // foreign key, referenced table

        val s = new LTable (s"${name}_j_${cntr.inc ()}", disambiguate (schema, refTab.schema),
                            domain ++ refTab.domain, key)

        val link = links.getOrElse (fkey, null)                             // get link for foreign key
        if link == null then
            s.addLinkage (fkey, refTab)                                     // add the linkage
//          flaw ("join", s"$name: foreign key $fkey not established as a link")
        else
            for t <- tuples do                                              // iterate over fkey table
                val t_fkey = pull (t, fkey)                                 // pull out foreign key value
                val u = link.getOrElse (t_fkey, null)                       // get tuple from pkey table
                if u != null then s.tuples += t ++ u                        // add concatenated tuples
            end for
        end if
        s
    end join

end LTable


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `lTableTest` main function tests the `LTable` class with queries on the
 *  Bank database.
 *  > runMain scalation.database.table.lTableTest
 */
@main def lTableTest (): Unit =

    // Data Definition Language

    val customer = LTable ("customer", "cname, street, ccity", "S, S, S", "cname")
    val branch   = LTable ("branch", "bname, assets, bcity", "S, D, S", "bname")
    val deposit  = LTable ("deposit", "accno, balance, cname, bname", "I, D, S, S", "accno")
    val loan     = LTable ("loan", "loanno, amount, cname, bname", "I, D, S, S", "loanno")

    customer.create_index ()
    branch.create_index ()

    //--------------------------------------------------------------------------
    banner ("Populate Database")

    customer += ("Peter", "Oak St",   "Bogart")
             += ("Paul",  "Elm St",   "Watkinsville")
             += ("Mary",  "Maple St", "Athens")
    customer.show ()

    branch += ("Alps",     20000000.0, "Athens")
           += ("Downtown", 30000000.0, "Athens")
           += ("Lake",     10000000.0, "Bogart")
    branch.show ()

    deposit += (11, 2000.0, "Peter", "Lake")
            += (12, 1500.0, "Paul",  "Alps")
            += (13, 2500.0, "Paul",  "Downtown")
            += (14, 2500.0, "Paul",  "Lake")
            += (15, 3000.0, "Mary",  "Alps")
            += (16, 1000.0, "Mary",  "Downtown")
    deposit.show ()

    loan += (21, 2200.0, "Peter", "Alps")
         += (22, 2100.0, "Peter", "Downtown")
         += (23, 1500.0, "Paul",  "Alps")
         += (24, 2500.0, "Paul",  "Downtown")
         += (25, 3000.0, "Mary",  "Alps")
         += (26, 1000.0, "Mary",  "Lake")
    loan.show ()

    deposit.addLinkage ("cname", customer)
    deposit.addLinkage ("bname", branch)
    loan.addLinkage ("cname", customer)
    loan.addLinkage ("bname", branch)
 
    //--------------------------------------------------------------------------
    banner ("Show Table Statistics")

    customer.stats.show ()
    branch.stats.show ()
    deposit.stats.show ()
    loan.stats.show ()

    //--------------------------------------------------------------------------
    banner ("Example Queries")

    banner ("Names of customers who live in Athens")
    val liveAthens = customer.σ ("ccity == 'Athens'").π ("cname")
    liveAthens.show ()

    banner ("Names of customers who bank (deposits) in Athens")
//  val bankAthens = (deposit ⋈ branch).σ ("bcity == 'Athens'").π ("cname")
    val bankAthens = (deposit ⋈ (("bname", branch.σ ("bcity == 'Athens'")))).π ("cname")
    bankAthens.show ()

    banner ("Names of customers who live or bank in Athens")
    val liveBank = customer.σ ("ccity == 'Athens'").π ("cname") ⋃
                   (deposit ⋈ branch).σ ("bcity == 'Athens'").π ("cname")
    liveBank.create_index ()
    liveBank.show ()

    banner ("Names of customers who live and bank in the same city")
    val sameCity = (customer ⋈ deposit ⋈ branch).σ ("ccity == bcity").π ("cname")
    sameCity.create_index ()
    sameCity.show ()

    banner ("Names and account numbers of customers with the largest balance")
    val largest = deposit.π ("cname, accno") - (deposit ⋈ ("balance < balance", deposit)).π ("cname, accno")
    largest.show ()

    banner ("Names of customers who are silver club members")
    val silver = (loan.π ("cname, bname") ⋂ deposit.π ("cname, bname")).π ("cname")
    silver.create_index ()
    silver.show ()

    banner ("Names of customers who are gold club members")
    val gold = loan.π ("cname") - (loan.π ("cname, bname") - deposit.π ("cname, bname")).π ("cname")
    gold.create_index ()
    gold.show ()

    banner ("Names of branches located in Athens")
    val inAthens = branch.σ ("bcity == 'Athens'").π ("bname")
    inAthens.show ()

    banner ("Names of customers who have deposits at all branches located in Athens")
    val allAthens = deposit.π ("cname, bname") / inAthens
    allAthens.create_index ()
    allAthens.show ()

    import Table.{avg, count}

    banner ("Branch names and their average balances")
    val avgBalance = deposit.γ ("bname").aggregate ("bname", (count, "accno"), (avg, "balance"))
    avgBalance.show ()

end lTableTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `lTableTest2` main function tests the `LTable` class with queries on the
 *  Student-Course database.
 *  > runMain scalation.database.table.lTableTest2
 */
@main def lTableTest2 (): Unit =

    // Data Definition Language

    val student   = LTable ("student",   "sid, sname, street, city, dept, level",
                                         "I, S, S, S, S, I", "sid")
    val professor = LTable ("professor", "pid, pname, street, city, dept",
                                         "I, S, S, S, S", "pid")
    val course    = LTable ("course",    "cid, cname, hours, dept, pid",
                                         "I, X, I, S, I", "cid")
    val takes     = LTable ("takes",     "sid, cid",
                                         "I, I", "sid, cid")

    //--------------------------------------------------------------------------
    banner ("Populate Database")

    student += (101, "Peter", "Oak St",   "Bogart",       "CS", 3)
            += (102, "Paul",  "Elm St",   "Watkinsville", "CE", 4)
            += (103, "Mary",  "Maple St", "Athens",       "CS", 4)
    student.show ()

    professor += (104, "DrBill", "Plum St",  "Athens",       "CS")
              += (105, "DrJohn", "Pine St",  "Watkinsville", "CE")
    professor.show ()

    course += (4370, "Database Management", 4, "CS", 104)
           += (4720, "Comp. Architecture",  4, "CE", 104)
           += (4760, "Computer Networks",   4, "CS", 105)
    course.show ()

    takes += (101, 4370)
          += (101, 4720)
          += (102, 4370)
          += (102, 4760)
          += (103, 4760)
    takes.show ()

    // Add links for foreign key contraints and efficient joins (will make any needed primary indices)

    takes.addLinkage ("sid", student)                          // takes sid references student sid
    takes.addLinkage ("cid", course)                           // takes cid references course cid
    course.addLinkage ("pid", professor)                       // course pid references professor pid

    //--------------------------------------------------------------------------
    banner ("Example Queries")

    //--------------------------------------------------------------------------
    banner ("Example Queries")

    banner ("locations of students")
    val locs = student project ("sname, city")
    locs.show ()

    banner ("living in Athens")
    val inAthens = student select ("city == 'Athens'")
    inAthens.show ()

    banner ("not living in Athens")
    val notAthens = student minus inAthens
    notAthens.show ()

    banner ("student intersect inAthens")
    val inters = student intersect inAthens
    inters.show ()

    banner ("in-Athens union not-in-Athens")
    val unio = inAthens union notAthens
    unio.show ()

    // Requires join from foreign key table to primary key table - so links can be used

    banner ("course taken: course id")
    val taken_id = takes.join (("sid", student))
                        .project ("sname, cid")
    taken_id.show ()

// FIX - fails since linkage must be established for intermediate tables

    banner ("course taken: course name")
    val taken_nm = takes.join (("sid", student))
                        .join (("cid", course))
                        .project ("sname, cname")
    taken_nm.show ()

    banner ("student taught by")
    val taught_by = takes.join (("sid", student))
                         .join (("cid", course))
                         .join (("pid", professor))
                         .project ("sname, pname")
    taught_by.show ()

end lTableTest2

