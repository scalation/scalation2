
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sat Jun 25 20:41:18 EDT 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Relational Table Generation
 */

package scalation
package database
package table

import scalation.mathstat._
import scalation.random._

import relation.Vectr

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TableGen` object generates data for instances of the `Table` class.
 */
object TableGen:

    private val debug = debugf ("TableGen", true)                     // debug function
    private val flaw  = flawf ("TableGen")                            // flaw function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomly generate m tuples to populate the given table.
     *  @param table   the table to populate
     *  @param m       the number of tuples to generate
     *  @param stream  the random number stream
     */
    def popTable (table: Table, m: Int, stream: Int = 0): Unit =
        val n     = table.schema.size
        val ranD  = RandomVecD (dim = m, max = 2 * m)
        val ranI  = RandomVecI (dim = m, max = 2 * m, unique = false)
        val ranS  = RandomVecS (dim = m, unique = false)
        val uranI = RandomVecI (dim = m, max = 2 * m)
        val uranS = RandomVecS (dim = m)
        val ranRw = RandomVecI (dim = m, max = m-1)
        val pkey  = table.key
        val dmain = table.domain

        makeTuples ()

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Make tuples: (1) random unique values for primary key attributes,
         *  (2) copies of primary keys from fkt for foreign key attributes, or
         *  (3) random values based on domain for other attributes.
         */
        def makeTuples (): Unit =
            val col = Array.ofDim [Vectr] (n)                         // generate column-by-column
            var j = 0
            while j < n do                                            // iterate through the attributes
                val atrj: String = table.schema(j)                    // j-th attribute name
                val fkt = table.linkTypes.getOrElse (atrj, null)      // which foreign key table or null

                if pkey contains atrj then                            // >> case PRIMARY KEY
                    col(j) = genUnique (j)                            // generate unique keys for attributes in pkey
                else if fkt != null then                              // >> case FOREIGN KEY
                    col(j) = pullPkeys (atrj, fkt)                    // foreign key = copy pkeys from fkt
                else                                                  // >> case REGULAR ATTRIBUTE
                    col(j) = genValue (j)                             // generate value for a regular attribute
                end if
                j += 1                                                // increase attribute counter
            end while

            for i <- 0 until m do
                val t = Array.ofDim [ValueType] (n)                   // insert row-by-row
                for j <- 0 until n do t(j) = col(j)(i)                // build the next tuple/row
                table.add (t)                                         // insert into table
            end for
        end makeTuples

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /** Randomly pull the primary key value out of the foreign key table (fkt).
         *  @param fkey  the foreign key attribute
         *  @param fkt   the foreign key table (fkt)
         */
        def pullPkeys (fkey: String, fkt: Table): Vectr =
            val  rows = ranRw.igen
            val jj = fkt.on (fkt.key(0))
            fkt.domain(jj) match
            case 'D' => VectorD (for i <- rows yield fkt.getPkey (i).key(0).asInstanceOf [Double])
            case 'I' => VectorI (for i <- rows yield fkt.getPkey (i).key(0).asInstanceOf [Int])
            case 'L' => VectorL (for i <- rows yield fkt.getPkey (i).key(0).asInstanceOf [Long])
            case 'S' => VectorS (for i <- rows yield fkt.getPkey (i).key(0).asInstanceOf [String])
            case _  => { flaw ("pullPkeys", "type not supported"); null }
        end pullPkeys

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Randomly generate unique values for attribute/column j.
         *  @param j  which attribute/column
         */
        def genUnique (j: Int): Vectr =
            dmain(j) match
            case 'D' => { flaw ("genUnique", "type `Double` should not be a primary key"); null }
            case 'I' => uranI.igen
            case 'L' => uranI.igen
            case 'S' => uranS.sgen
            case _  => { flaw ("genUnique", "type not supported"); null }
        end genUnique

        //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        /*  Randomly generate values for attribute/column 'j'.
         *  @param j  which attribute/column
         */
        def genValue (j: Int): Vectr =
            dmain(j) match
            case 'D' => ranD.gen
            case 'I' => ranI.igen
            case 'L' => ranI.igen
            case 'S' => ranS.sgen
            case _   => { flaw ("genValue", "type not supported"); null }
        end genValue

    end popTable 

end TableGen


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `tableGenTest` main function is used to test the `TableGen` object.
 *  Create unpopulated tables and use the table generator to populate their columns.
 *  Tables:  student, professor, course, section, transcript
 *  > runMain scalation.database.table.tableGenTest
 */
@main def tableGenTest (): Unit =

    val student = Table ("student", "sid, name, address, status", "I, S, S, S", "sid")
    TableGen.popTable (student, 40)
    student.show ()

    val professor = Table ("professor", "pid, name, deptid", "I, S, I", "pid")
    TableGen.popTable (professor, 10)
    professor.show ()

    val course = Table ("course", "cid, deptid, crsname, descr", "I, I, S, S", "cid")
    TableGen.popTable (course, 20)
    course.show ()

    val section = Table ("section", "crn, cid, semester, pid", "I, I, S, I", "crn")
    section.addLinkage ("cid", course)                             // teaching cid references course cid
    section.addLinkage ("pid", professor)                          // teaching pid references professor pid
    TableGen.popTable (section, 50)
    section.show ()
    section.show_foreign_keys ()

    val transript = Table ("transript", "sid, crn, grade", "I, I, S", "sid, crn")
    transript.addLinkage ("sid", student)                          // transript sid references student sid
    transript.addLinkage ("crn", section)                          // transript crn references section crn
    TableGen.popTable (transript, 70)
    transript.show ()
    transript.show_foreign_keys ()

end tableGenTest

