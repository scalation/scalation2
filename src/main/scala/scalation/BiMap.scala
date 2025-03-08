
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Aug 24 15:46:03 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    A Bidirectional Map:  Key <-> Value
 */

package scalation

import scala.collection.mutable.HashMap

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BiMap` class maps keys to values and values to keys.
 *  @tparam K  the key type
 *  @tparam V  the value type
 */
class BiMap [K, V]:

    /** The fore map from keys to values
     */
    private val fore = HashMap [K, V] ()

    /** The back map from values to keys
     */
    private val back = HashMap [V, K] ()

    def += (kv: (K, V)): BiMap.this.type = put (kv)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Put the (key, value) to the map.
     *  @param kv  the key-value pair
     */
    def put (kv: (K, V)): BiMap.this.type =
        fore += kv._1 -> kv._2
        back += kv._2 -> kv._1
        this
    end put

    def put (k: K, v: V): Option [V] =
        val oldV = fore.put (k, v)
        back.put (v, k)
        oldV
    end put

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the a key 'k', return the most recent value it was entered with.
     *  @param k  the key whose value is sought
     */
    def apply (k: K): V = fore (k)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Given the a value 'v', return the most recent key it was entered with.
     *  If values are unqiue, keys and values will be one-to-one correspondence.
     *  Note, if all keys are needed should use a `MultiMap`.
     *  @param v  the value whose key is sought
     */
    def from (v: V) = back (v)

    def contains (k: K): Boolean = fore.contains (k)

end BiMap 


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `biMapTest` main function is used the test the `BiMap` class.
 *  > runMain scalation.biMapTest
 */
@main def biMapTest (): Unit =

    val m = new BiMap [String, Int] ()
    m.put ("Jan" ->  1)
    m.put ("Feb" ->  2)
    m.put ("Mar" ->  3)
    m.put ("Apr" ->  4)
    m.put ("May" ->  5)
    m.put ("Jun" ->  6)
    m.put ("Jul" ->  7)
    m.put ("Aug" ->  8)
    m.put ("Sep" ->  9)
    m.put ("Oct" -> 10)
    m.put ("Nov" -> 11)
    m +=  ("Dec" -> 12)

    for i <- 1 to 12 do println (s"month $i is ${m.from (i)}")
    println (s"May is month ${m("May")}")

    println ("contains (Jun): " + m.contains ("Jun"))
    println ("contains (Mon): " + m.contains ("Mon"))

end biMapTest

