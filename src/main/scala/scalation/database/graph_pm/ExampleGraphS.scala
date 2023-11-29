
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Aravind Kalimurthy
 *  @version 2.0
 *  @date    Tue Aug  9 16:39:41 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Example Data and Query Graphs (with `String` labels)
 */

package scalation
package database
package graph_pm

import scala.collection.mutable.Map
import scala.collection.mutable.{Set => SET}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ExampleGraphS` object contains example query and data digraphs in which
 *  the vertex label type is `String`.
 */
object ExampleGraphS:

    // -----------------------------------------------------------------------
    // Simple data and query digraphs.
    // -----------------------------------------------------------------------

    // data digraph g1 -------------------------------------------------------

    val g1 = new Graph (Array (SET (),                            // ch(0)
                               SET (0, 2, 3, 4),                  // ch(1)
                               SET (0),                           // ch(2)
                               SET (4),                           // ch(3)
                               SET ()),                           // ch(4)
                        Array ("abc", "xyz", "abc", "abc", "abc"),
                        Map ((0, 3) -> "knows",
                             (0, 4) -> "knows",
                             (1, 0) -> "knows",
                             (1, 2) -> "knows",
                             (1, 4) -> "knows",
                             (2, 0) -> "knows",
                             (2, 4) -> "likes",
                             (3, 2) -> "knows",
                             (3, 4) -> "likes"),
                        false, "g1")

    // query digraph q1 ------------------------------------------------------

    val q1 = new Graph (Array (SET (1, 2),                        // ch(0)
                               SET (),                            // ch(1)
                               SET (1)),                          // ch(2)
                        Array ("xyz", "abc", "abc"),
                        Map ((0, 1) -> "knows",
                             (0, 2) -> "knows",
                             (2, 1) -> "knows"),
                        false, "q1")

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

    val g2 = new Graph (Array (SET (1),                           // ch(0)
                               SET (0, 2, 3, 4, 5),               // ch(1)
                               SET (),                            // ch(2)
                               SET (),                            // ch(3)
                               SET (),                            // ch(4)
                               SET (6, 10),                       // ch(5)
                               SET (7, 4, 8, 9),                  // ch(6)
                               SET (1),                           // ch(7)
                               SET (),                            // ch(8)
                               SET (),                            // ch(9)
                               SET (11),                          // ch(10)
                               SET (12),                          // ch(11)
                               SET (11, 13),                      // ch(12)
                               SET (),                            // ch(13)
                               SET (13, 15),                      // ch(14)
                               SET (16),                          // ch(15)
                               SET (17, 18),                      // ch(16)
                               SET (14, 19),                      // ch(17)
                               SET (20),                          // ch(18)
                               SET (14),                          // ch(19)
                               SET (19, 21),                      // ch(20)
                               SET (),                            // ch(21)
                               SET (21, 23),                      // ch(22)
                               SET (25),                          // ch(23)
                               SET (),                            // ch(24)
                               SET (24, 26),                      // ch(25)
                               SET (28),                          // ch(26)
                               SET (),                            // ch(27)
                               SET (27, 29),                      // ch(28)
                               SET (22)),                         // ch(29)
                        Array ("xyz", "abc", "pqr", "pqr", "pqr", "xyz",
                               "abc", "xyz", "pqr", "efg", "pqr", "xyz",
                               "abc", "pqr", "abc", "xyz", "abc", "pqr",
                               "xyz", "xyz", "abc", "pqr", "abc", "xyz",
                               "pqr", "abc", "xyz", "pqr", "abc", "xyz"),
                        Map ((0, 1) -> "knows",
                             (1, 0) -> "knows",
                             (1, 2) -> "knows",
                             (1, 3) -> "knows",
                             (1, 4) -> "knows",                   // likes
                             (1, 5) -> "knows",
                             (5, 6) -> "knows",
                             (5, 10) -> "knows",
                             (6, 7) -> "knows",
                             (6, 4) -> "knows",                   // likes
                             (6, 8) -> "knows",
                             (6, 9) -> "knows",
                             (7, 1) -> "knows",
                             (10, 11) -> "knows",
                             (11, 12) -> "knows",
                             (12, 11) -> "knows",
                             (12, 13) -> "knows",
                             (14, 13) -> "knows",
                             (14, 15) -> "knows",
                             (15, 16) -> "knows",
                             (16, 17) -> "knows",
                             (16, 18) -> "knows",
                             (17, 14) -> "knows",
                             (17, 19) -> "knows",
                             (18, 20) -> "knows",
                             (19, 14) -> "knows",
                             (20, 19) -> "knows",
                             (20, 21) -> "knows",
                             (22, 21) -> "knows",
                             (22, 23) -> "knows",
                             (23, 25) -> "knows",
                             (25, 24) -> "knows",
                             (25, 26) -> "knows",
                             (26, 28) -> "knows",
                             (28, 27) -> "knows",
                             (28, 29) -> "knows",
                             (29, 22) -> "knows"),
                        false, "g2")

    // query digraph q2 ------------------------------------------------------

    val q2 = new Graph (Array (SET (1),                           // ch(0)
                               SET (0, 2, 3),                     // ch(1)
                               SET (),                            // ch(2)
                               SET ()),                           // ch(3)
                        Array ("xyz", "abc", "pqr", "pqr"),
                        Map ((0, 1) -> "knows",
                             (1, 0) -> "knows",
                             (1, 2) -> "knows",
                             (1, 3) -> "knows"),
                        false, "q2")

    val g2p = new Graph (g2.ch, g2.label, g2.elabel, true, g2.name)    // with parents
    val q2p = new Graph (q2.ch, q2.label, q2.elabel, true, q2.name)    // with parents

end ExampleGraphS

