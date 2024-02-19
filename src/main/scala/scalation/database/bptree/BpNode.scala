
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Wed Aug  9 01:25:50 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    B+Tree Node with find, add, split, remove, and merge Operations
 *
 *  Node key: [ . k0 . k1 . k2 . k3 . ]
 *               <  <=   <=   <=   <=         note: number of keys is one less than number of refs
 *       ref:  r0   r1   r2   r3   r4
 *  Leaf:      r0 -> next leaf node; r1 -> tuple (k0, ...); r2 -> tuple (k1, ...); etc.
 *  Internal:  r0 -> subtree with keys < k0; r1 -> subtree with keys in [k0, k1); etc.
 *  Split:     extra room in nodes allows the overflow key to be inserted before split
 */

package scalation
package database.bptree

import scala.runtime.ScalaRunTime.stringOf

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BpNode` companion object provides settings for node sizes. 
 */
object BpNode:

    private val debug = debugf ("BpNode", true)                    // debug function
    private val flaw  = flawf ("BpNode")                           // flaw function

    private var order = 5                                          // maximum number of references (reset as needed)
    private var half  = (order - 1) / 2                            // half of max keys (floor)
    private var halfp = order - half                               // rest of the keys (half plus)
    private var mink  = halfp - 1                                  // minimum number of keys (before underflow)

    debug ("init", s"order = $order, half = $half, halfp = $halfp, mink = $mink")

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the order of BpNodes to order_.
     *  @param order_  the new order for BpNodes (must be at least 4)
     */
    def setOrder (order_ : Int): Unit = 
        if order < 4 then flaw ("setOrder", s"order_ = $order_ must be at least 4")
        else
            order = order_
            half  = (order - 1) / 2
            halfp = order - half
            mink  = half - 1
        end if
    end setOrder

end BpNode

import BpNode._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BpNode` class defines nodes of size order that may be stored in a B+tree.
 *  Keys have type `ValueType` and may reference values of `Any` type.
 *  To hold an overflow before splitting, nodes have extra room (order + 1)
 *  @param keys    number of active keys
 *  @param isLeaf  whether this node is a leaf
 */
class BpNode (private [database] var keys: Int, val isLeaf: Boolean)
      extends Serializable:

    private [database] val key = Array.ofDim [ValueType] (order)    // array to hold keys
    private [database] val ref = Array.ofDim [Any] (order + 1)      // array to hold values or references to nodes

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a new root node with one key (and two references) in it.
     *  @param left   the left node (< dkey)
     *  @param dkey   the divider key
     *  @param right  the right node (>= dkey)
     */
    def this (left: BpNode, dkey: ValueType, right: BpNode) =
        this (1, false)
        key(0) = dkey                                               // divider key
        ref(0) = left; ref(1) = right                               // left and right references
    end this

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether this node has overflowed (too many keys).
     */
    inline def overflow: Boolean = keys >= order

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether this node has underflowed (too few keys).
     */
    inline def underflow: Boolean = keys < mink

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return whether this node is rich (i.e., has surplus keys).
     */
    inline def rich: Boolean = keys > mink

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the key at index position i.
     *  @param i  the index position in this node
     */
    inline def apply (i: Int): ValueType =
        if i >= keys then flaw ("apply", s"index i = $i must be less than keys = $keys")
        key(i)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update the key at index position i.
     *  @param i  the index position in this node
     *  @param k  the new value for key(i)
     */
    inline def update (i: Int, k: ValueType): Unit =
        if i >= keys then flaw ("update", s"index i = $i must be less than keys = $keys")
        key(i) = k
    end update

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find and return the first position where 'k < key_i' in this node.
     *  @param k  the key whose position is sought
     */
    def find (k: ValueType): Int =
        val i = key.indexWhere (k < _)
        if i < 0 then keys else i
    end find

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find and return the first position where 'k == key_i' in this node.
     *  @param k  the key whose position is sought
     */
    def findEq (k: ValueType): Int = key.indexWhere (k == _)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add the new key k and value v into this node at insertion position (ip).
     *  @param k  the new key
     *  @param v  the new value (or node for internal nodes)
     */
    def add (k: ValueType, v: Any): Unit =
        val ip = find (k)                                           // find insertion position (ip)
        debug ("add", s"(k = $k, v = $v) pair at ip = $ip") 
        for i <- keys until ip by -1 do                             // make room by shifting keys right
            key(i)   = key(i-1)
            ref(i+1) = ref(i)
        key(ip)   = k                                               // insert new key
        ref(ip+1) = v                                               // insert new value (right of key)
        keys     += 1                                               // increment to number of active keys
    end add

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Split this LEAF node by creating a right sibling node (rt) and moving
     *  half the keys and references to that new node, leaving halfp.
     *  Return the divider key and the right sibling node.
     */
    def split (): (ValueType, BpNode) =
        val rt = new BpNode (half, true)                            // allocate leaf right sibling node (rt)
        for i <- 0 until half do                                    // move largest half of keys (with refs) to rt
            rt.key(i)   = key(halfp + i)
            rt.ref(i+1) = ref(halfp + i + 1)                        // refs are right of keys
        rt.ref(0) = ref(0)                                          // update LINKED LIST of nodes
        ref(0)    = rt                                              // this -> rt -> old-right
        keys      = halfp                                           // reset number of active keys to help plus
        (rt.key(0), rt)                                             // (divider key (smallest right) and right sibling
    end split

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Split this INTERNAL node by creating a right sibling rt and moving half
     *  the keys and references to that new node, leaving halfp - 1.
     *  Return the divider key and the right sibling node.
     */
    def splitI (): (ValueType, BpNode) =
        val rt = new BpNode (half, false)                           // allocate internal right sibling node (rt)
        for i <- 0 until half do                                    // move largest half of keys (with refs) to rt
            rt.key(i) = key(halfp + i)
            rt.ref(i) = ref(halfp + i)
        rt.ref(half) = ref(keys)                                    // copy over the last ref
        keys = halfp - 1                                            // reset number of active keys to help plus - 1
        (key(keys), rt)                                             // divider key (middle key) and right sibling
    end splitI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Remove key at dp and its reference from this node and check for underflow.
     *  @param dp  the deletion index position (may use findEq to find it)
     */
    def remove (dp: Int): Boolean =
        debug ("remove", s"dp = $dp, key = ${key(dp)}")
        for i <- dp until keys do                                   // remove at dp by shifting keys left
            key(i) = key(i+1)
            ref(i+1) = ref(i+2)
        keys -= 1                                                   // decrement the number of keys
        underflow                                                   // if true, node underflows
    end remove

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Merge this LEAF node with its right sibling node (rt).
     *  @param rt  the right sibling node
     */
    def merge (rt: BpNode): Unit =
        for i <- 0 until rt.keys do                                 // move keys from rt into this node
            key(keys + i)     = rt.key(i)
            ref(keys + i + 1) = rt.ref(i + 1)
        keys   += rt.keys                                           // add the number of keys from rt
        rt.keys = 0                                                 // make rt empty
        ref(0)  = rt.ref(0)                                         // unlink node rt
    end merge

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Merge this INTERNAL node with its right sibling node (rt).
     *  @param dk  the divider key from parent
     *  @param rt  the right sibling node
     */
    def mergeI (dk: ValueType, rt: BpNode): Unit =
        key(keys)     = dk                                          // move divider key
        ref(keys + 1) = rt.ref(0)                                   // node corresponding to divider key
        for i <- 0 until rt.keys do                                 // move keys from rt into this node
            key(keys + i + 1) = rt.key(i)
            ref(keys + i + 2) = rt.ref(i + 1)
        keys   += rt.keys + 1                                       // add the number of keys from rt
        rt.keys = 0                                                 // make rt empty
    end mergeI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this node to a string.
     */
    override def toString: String =
        val sb = StringBuilder ("[ . " )
        for i <- 0 until keys do sb ++= (s"${key(i)} . ")
        sb ++= ("]" )
        sb.toString
    end toString

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the node's data structure.
     */
    def show (): Unit =
        println (s"isLeaf = $isLeaf")
        println (s"keys   = $keys")
        println (s"key    = ${stringOf (key)}")
        println (s"ref    = ${stringOf (ref)}")
    end show

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show the node's references.
     */
    def showRef (): Unit =
        println (s"ref = ${stringOf (ref)}")
    end showRef

end BpNode


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bpNodeTest` main function tests the `BpNode` class by inserting random
 *  key values and testing a leaf node split.
 *  > runMain scalation.database.bptree.bpNodeTest
 */
@main def bpNodeTest (): Unit =

    import java.util.Random

    banner ("Example of an Leaf Node Split")

    val totKeys = 5
    val mx      = 10 * totKeys
    val seed    = 1
    val rng     = new Random (seed)
    val node    = new BpNode (0, true)                              // empty leaf node
    var right: BpNode = null

    for i <- 1 to totKeys do
        val key = rng.nextInt (mx)
        banner (s"put key = $key")
        node.add (key, 2 * key)
        println (s"node = $node")
        if node.overflow then
            println (s"BEFORE split: node = $node")
            node.showRef ()
            val (dk, rt) = node.split ()                            // split keys between node (3) and right (2)
            right = rt
            println (s"AFTER split:  node = $node, dk = $dk, rt = $rt")
            node.showRef (); rt.showRef ()
    end for

    banner ("Show Arrays")
    node.show ()

    banner ("Example of an Leaf Node Merge")
    node.remove (2)
    node.merge (right)
    println (s"AFTER merge:  node = $node, right = $right")
    node.show ()

end bpNodeTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bpNodeTest2` main function tests the `BpNode` class by inserting random
 *  key values and testing a internal node split.
 *  > runMain scalation.database.bptree.bpNodeTest2
 */
@main def bpNodeTest2 (): Unit =

    import java.util.Random

    banner ("Example of an Internal Node Split")

    val totKeys = 5
    val mx      = 10 * totKeys
    val seed    = 1
    val rng     = new Random (seed)
    val node    = new BpNode (0, false)                             // empty internal node: false => not isLeaf

    for i <- 1 to totKeys do
        val key = rng.nextInt (mx)
        banner (s"put key = $key")
        node.add (key, 2 * key)
        println (s"node = $node")
        if node.overflow then
            println (s"BEFORE split: node = $node")
            node.showRef ()
            val (dk, rt) = node.splitI ()                           // splitI keys between node (2) and right (2)
            println (s"AFTER split:  node = $node, dk = $dk, rt = $rt")
            node.showRef (); rt.showRef ()
    end for

    banner ("Show Arrays")
    node.show ()

end bpNodeTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bpNodeTest3` main function tests the `BpNode` class by inserting random
 *  and removing key values.
 *  > runMain scalation.database.bptree.bpNodeTest3
 */
@main def bpNodeTest3 (): Unit =

    banner ("Example of key insertion and removal")

    val keyArr = Array (35, 47, 4, 38)
    val node   = new BpNode (0, true)                               // empty internal node: false => not isLeaf

    for k <- keyArr do
        banner (s"put key = $k")
        node.add (k, 2 * k)
        println (s"node = $node")
    end for

    for k <- keyArr do
        banner (s"remove key = $k")
        val dp = node.findEq (k)                                    // find the deletion position (dp)
        node.remove (dp)                                            // remove the key and its reference
        println (s"AFTER remove: node = $node")
        node.showRef ()
    end for

    banner ("Show Arrays")
    node.show ()

end bpNodeTest3

