
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue Nov  1 19:12:16 EDT 2016
 *  @see     LICENSE (MIT style license file).
 */

package scalation
package database
package graph_pm

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExampleGraphD` object contains example query and data digraphs
 *  in which the vertex label type is `Double`.
 */
object ExampleGraphD:

    // -----------------------------------------------------------------------
    // Simple data and query digraphs.
    // -----------------------------------------------------------------------
    val schema  = Array ("user", "user", "user", "user", "user")

    val schema1 = Array ("user", "user", "user")
    // data digraph g1 -------------------------------------------------------

    val g1 = new Graph (Array (SET (),                          // ch(0)
                               SET (0, 2, 3, 4),                // ch(1)
                               SET (0),                         // ch(2)
                               SET (4),                         // ch(3)
                               SET ()),                         // ch(4)
                        Array (11.0, 10.0, 11.0, 11.0, 11.0),   // vertex labels
                        Map ((1, 0) -> -1.0,                    // edge labels
                             (1, 2) -> -1.0,
                             (1, 3) -> -1.0,
                             (1, 4) -> -1.0,
                             (2, 0) -> -1.0,
                             (3, 4) -> -2.0),                   // change from -1 to -2 filter out vertices
                        false, "g1")                            // inverse, name, schema

    // query digraph q1 ------------------------------------------------------

    val q1 = new Graph (Array (SET (1, 2),                      // ch(0)
                               SET (),                          // ch(1)
                               SET (1)),                        // ch(2)
                        Array (10.0, 11.0, 11.0),               // vertex labels
                        Map ((0, 1) -> -1.0,                    // edge labels
                             (0, 2) -> -1.0,
                             (2, 1) -> -1.0),
                        false, "q1")                            // inverse, name

    val g1p = new Graph (g1.ch, g1.label, g1.elabel, true, g1.name)    // with parents
    val q1p = new Graph (q1.ch, q1.label, q1.elabel, true, q1.name)    // with parents

    // -----------------------------------------------------------------------
    // Data and query graphs from the following paper:
    // John A. Miller, Lakshmish Ramaswamy, Arash J.Z. Fard and Krys J. Kochut,
    // "Research Directions in Big Data Graph Analytics,"
    // Proceedings of the 4th IEEE International Congress on Big Data (ICBD'15),
    // New York, New York (June-July 2015) pp. 785-794.
    // -----------------------------------------------------------------------

    // data digraph g2 -------------------------------------------------------

    val g2 = new Graph (Array (SET (1),                         // ch(0)
                               SET (0, 2, 3, 4, 5),             // ch(1)
                               SET (),                          // ch(2)
                               SET (),                          // ch(3)
                               SET (),                          // ch(4)
                               SET (6, 10),                     // ch(5)
                               SET (7, 4, 8, 9),                // ch(6)
                               SET (1),                         // ch(7)
                               SET (),                          // ch(8)
                               SET (),                          // ch(9)
                               SET (11),                        // ch(10)
                               SET (12),                        // ch(11)
                               SET (11, 13),                    // ch(12)
                               SET (),                          // ch(13)
                               SET (13, 15),                    // ch(14)
                               SET (16),                        // ch(15)
                               SET (17, 18),                    // ch(16)
                               SET (14, 19),                    // ch(17)
                               SET (20),                        // ch(18)
                               SET (14),                        // ch(19)
                               SET (19, 21),                    // ch(20)
                               SET (),                          // ch(21)
                               SET (21, 23),                    // ch(22)
                               SET (25),                        // ch(23)
                               SET (),                          // ch(24)
                               SET (24, 26),                    // ch(25)
                               SET (28),                        // ch(26)
                               SET (),                          // ch(27)
                               SET (27, 29),                    // ch(28)
                               SET (22)),                       // ch(29)
                        Array (10.0, 11.0, 12.0, 12.0, 12.0, 10.0, 11.0, 10.0, 12.0, 15.0, 12.0, 10.0, 11.0, 12.0, 11.0,
                               10.0, 11.0, 12.0, 10.0, 10.0, 11.0, 12.0, 11.0, 10.0, 12.0, 11.0, 10.0, 12.0, 11.0, 10.0),
                        Map ((0, 1)   -> 1.0,
                             (1, 0)   -> 1.0,
                             (1, 2)   -> 1.0,
                             (1, 3)   -> 1.0,
                             (1, 4)   -> 1.0,                   // 2
                             (1, 5)   -> 1.0,
                             (5, 6)   -> 1.0,
                             (5, 10)  -> 1.0,
                             (6, 7)   -> 1.0,
                             (6, 4)   -> 1.0,                   // 2
                             (6, 8)   -> 1.0,
                             (6, 9)   -> 1.0,
                             (7, 1)   -> 1.0,
                             (10, 11) -> 1.0,
                             (11, 12) -> 1.0,
                             (12, 11) -> 1.0,
                             (12, 13) -> 1.0,
                             (14, 13) -> 1.0,
                             (14, 15) -> 1.0,
                             (15, 16) -> 1.0,
                             (16, 17) -> 1.0,
                             (16, 18) -> 1.0,
                             (17, 14) -> 1.0,
                             (17, 19) -> 1.0,
                             (18, 20) -> 1.0,
                             (19, 14) -> 1.0,
                             (20, 19) -> 1.0,
                             (20, 21) -> 1.0,
                             (22, 21) -> 1.0,
                             (22, 23) -> 1.0,
                             (23, 25) -> 1.0,
                             (25, 24) -> 1.0,
                             (25, 26) -> 1.0,
                             (26, 28) -> 1.0,
                             (28, 27) -> 1.0,
                             (28, 29) -> 1.0,
                             (29, 22) -> 1.0),
                        false, "g2")

    // query digraph q2 ------------------------------------------------------

    val q2 = new Graph (Array (SET (1),                         // ch(0)
                               SET (0, 2, 3),                   // ch(1)
                               SET (),                          // ch(2)
                               SET ()),                         // ch(3)
                        Array (10.0, 11.0, 12.0, 12.0),
                        Map ((0, 1) -> 1.0,
                             (1, 0) -> 1.0,
                             (1, 2) -> 1.0,
                             (1, 3) -> 1.0),
                        false, "q2")

    val g2p = new Graph (g2.ch, g2.label, g2.elabel, true, g2.name)    // with parents
    val q2p = new Graph (q2.ch, q2.label, q2.elabel, true, q2.name)    // with parents

end ExampleGraphD

