
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Aug 11 00:26:03 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Sorted Map Implemented Using B+Trees (Indexed and Sequential Access)
 *
 *  Split Nodes on Overflow
 *  Structure for order = 5 (max of 4 keys), upon first split
 *  [ . k4 . -- . -- . -- . ]
 *      [ . k1 . k2 . k3 . -- . ]
 *      [ . k4 . k5 . -- . -- . ]
 *  Rules: divider key (k4 added to parent in this case) is the smallest key
 *             in the right subtree (SMALLEST RIGHT)
 *         split node n into (n, right_sibling_node) with larger half staying in n 
 *         internal node split promotes middle key to parent as the divider key
 *  Borrow/Merge Nodes on Underflow
 *  Rules: try to borrow one key from an adjacent (left or right) rich sibling node
 *         otherwise merge with sibling node
 */

package scalation
package database.bptree

import scala.collection.mutable.AbstractMap     // , SortedMap}
import scala.reflect.ClassTag

import BpNode._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `BpTreeMap` class provides sorted maps that use the B+Tree Data Structure.
 *  Inserts may cause the splitting of nodes, while deletes may cause borrowing
 *  if keys or merging of nodes.
 *  @tparam V  the type of the values assigned to keys in this sorted map
 */
class BpTreeMap [V: ClassTag] ()
      extends AbstractMap [ValueType, V]
//       with SortedMap [ValueType, V]
         with Serializable:

    private val debug  = debugf ("BpTreeMap", true)                      // debug function
    private var kCount = 0                                               // counter for total number of keys
            var count  = 0                                               // count # nodes accessed (performance)
    private var root   = new BpNode (0, true)                            // root node of this B+Tree (initially empty)
    private val first  = root                                            // first leaf node of this B+Tree

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `SortedMap` trait requires `Ordering` with a compare method to be defined.
     *  @see https://scala-lang.org/api/3.3.0/scala/math/Ordering.html
     *  @see ValueType.scala in `scalation.package`
     */
//  def ordering: Ordering [ValueType] = ValueTypeOrd

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the size (number of keys) of this B+Tree. 
     */
    inline override def size: Int = kCount

//------------------------------------------------------------------------------
// Retrieve values or ranges (subtrees)
//------------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The `TreeIterator` inner class supports iterating over all the elements
     *  in a B+Tree by traversing through the LEAF nodes of the tree.
     *  @param ns  the starting leaf node (defaults to first)
     *  @param js  the starting within node index (defaults to -1)
     */
    class TreeIterator (ns: BpNode = first, js: Int = -1) extends Iterator [(ValueType, V)]:
        var (n, j) = (ns, js)
        def hasNext: Boolean = j < n.keys-1 || n.ref(0) != null
        def next (): (ValueType, V) =
            debug ("next", s"node n = $n")
            if j < n.keys-1 then j += 1 else { n = n.ref(0).asInstanceOf [BpNode]; j = 0 }
            (n(j), n.ref(j+1).asInstanceOf [V])
        end next
    end TreeIterator

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return an iterator for retrieving all the elements in this B+Tree.
     *  @see scala.collection.IterableOnce
     */
    def iterator: Iterator [(ValueType, V)] = new TreeIterator ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return an iterator for retrieving all the elements in this B+Tree starting
     *  from key start.  Returns null if all keys in tree are smaller than start.
     *  @see scala.collection.SortedMapOps
     *  @param start  the key to start with
    def iteratorFrom (start: ValueType): Iterator [(ValueType, V)] =
        val (ns, js) = findp (start, root)
        if ns != null then new TreeIterator (ns, js)
        else null
    end iteratorFrom
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return an iterator for retrieving all the keys in this B+Tree starting
     *  from key start.
     *  @see scala.collection.SortedMapOps
     *  @param start  the key to start with
    def keysIteratorFrom (start: ValueType): Iterator [ValueType] =
        throw new UnsupportedOperationException ("keysIteratorFrom not available, use iteratorFrom instead")
    end keysIteratorFrom
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the submap starting at from and ending before until.
     *  @see scala.collection.SortedOps
     *  @param from   the starting key (inclusive)
     *  @param until  the ending key (exclusive)
    def rangeImpl (from: Option [ValueType], until: Option [ValueType]): BpTreeMap [V] =
        val subtree = new BpTreeMap [V] ()
        val it = if from.isDefined then iteratorFrom (from.get)
                 else iterator
        var cont = true
        while cont && it.hasNext do 
            val (k, v) = it.next ()
            if ! until.isDefined || k < until.get then subtree.addOne ((k, v))
            else cont = false
        end while
        subtree
    end rangeImpl
     */

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the value associated with the key by looking it up in this B+Tree.
     *  @param key  the key used for look up
     */
    def get (key: ValueType): Option [V] = Option (find (key))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the given key in this B+tree and return its corresponding value.
     *  Calls the recursive findp method.
     *  @param key  the key to find
     */
    inline def find (key: ValueType): V =
        val (ln, ip) = findp (key, root)                                 // leaf node, index position
        if ip >= 0 then ln.ref(ip+1).asInstanceOf [V] else null.asInstanceOf [V]
    end find

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursive helper method for finding the position of the given key in this B+tree.
     *  @param key  the key to find
     *  @param n    the current node
     */
    private def findp (key: ValueType, n: BpNode): (BpNode, Int) =
        count += 1
        if n.isLeaf then (n, n.findEq (key))
        else findp (key, n.ref(n.find (key)).asInstanceOf [BpNode])
    end findp

//------------------------------------------------------------------------------
// Add key-value pairs into the B+Tree
//------------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add one key-value pair into this B+Tree and return this.
     *  Called by the put method in `AbstractMap`.
     *  Note:  it splits the node that overflows
     *  @param elem   the key-value pair to add/insert
     */
    def addOne (elem: (ValueType, V)): this.type =
        val (key, value) = elem
        kCount += 1                                                      // increment the key count
        insert (key, value, root)                                        // call the recursive insert
        this
    end addOne

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursive helper method for inserting a key and ref into this B+tree.
     *  Returns the divider key and right sibling upon split, else null.
     *  @param key  the key to insert
     *  @param ref  the value/node to insert
     *  @param n    the current node
     */
    private def insert (key: ValueType, ref: V, n: BpNode): (ValueType, BpNode) =
        var k_r: (ValueType, BpNode) = null

        if n.isLeaf then                                                 // handle LEAF node
            k_r = add (n, key, ref)
            if k_r != null then
                if n != root then return k_r
                root = new BpNode (root, k_r._1, k_r._2)                 // make a new root
            end if

        else                                                             // handle INTERNAL node
            k_r = insert (key, ref, n.ref(n.find (key)).asInstanceOf [BpNode])
            if k_r != null then
                k_r = addI (n, k_r._1, k_r._2)
                if k_r != null && n == root then root = new BpNode (root, k_r._1, k_r._2)
            end if
        end if
        k_r
    end insert

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add new key k and value v into LEAF node n.  Upon overflow, split node n,
     *  in which case the divider key and new right sibling node are returned.
     *  @param n  the current node
     *  @param k  the new key
     *  @param v  the new value
     */
    private def add (n: BpNode, k: ValueType, v: V): (ValueType, BpNode) =
        var k_r: (ValueType, BpNode) = null                              // divider key, right sibling
        n.add (k, v)                                                     // add into node n
        if n.overflow then k_r = n.split ()                              // its full, must split
        k_r
    end add

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add new key k and value v into INTERNAL node n.  Upon overflow, split node n,
     *  in which case the divider key and new right sibling node are returned.
     *  @param n  the current node
     *  @param k  the new key
     *  @param v  the new left value (ref a node)
     */
    private def addI (n: BpNode, k: ValueType, v: BpNode): (ValueType, BpNode) =
        var k_r: (ValueType, BpNode) = null                              // divider key, right sibling
        n.add (k, v)                                                     // add into node n
        n.showRef ()
        if n.overflow then k_r = n.splitI ()                             // its full, must split
        k_r
    end addI

//------------------------------------------------------------------------------
// Remove key-value pair from the B+Tree
//------------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Subtract/remove the one element (key-value pair) with the given key.
     *  Called by the remove method in `AbstractMap`.
     *  @param key  the key whose element is to be removed
     */
    def subtractOne (key: ValueType): this.type =
        kCount -= 1                                                      // decrement the key count
        delete (key, root)                                               // call the recursive delete
        if ! root.isLeaf && root.keys == 0 then
            root = root.ref(0).asInstanceOf [BpNode]                     // remove empty root by resetting the root reference
        this
    end subtractOne

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Recursive helper method for deleting a key with its ref from this B+tree.
     *  @param key  the key to delete
     *  @param n    the current node
     *  @param par  the parent node (null for root)
     */
    private def delete (key: ValueType, n: BpNode, par: BpNode = null): Unit =

        if n.isLeaf then                                                 // handle LEAF node
            val dp = n.findEq (key)                                      // deletion position in LEAF node n
            n.remove (dp)                                                // remove key at index position dp

            // upon underflow do a leaf node borrow or merge

            if n != root && n.underflow then                             // unless root, check for underflow
                println (s"delete: needs to handle underflow of LEAF node $n")
                val j = par.find (key)                                   // j-th index position in parent
                val (sib, left) = richestSib (par, j)                    // richest sib and whether it's left
                println (s"delete: leaf sib = $sib")

                if sib.rich then borrow (n, sib, left, par, j)           // leaf borrow a key from rich sib
                else merge (n, sib, left, par, j)                        // leaf merge nodes n and sib, may cause parent to underflow
            end if

        else                                                             // handle INTERNAL node
            val dp = n.find (key)                                        // deletion position in INTERNAL node n
            delete (key, n.ref(dp).asInstanceOf [BpNode], n)             // recursive call to delete

            // upon underflow do an internal node borrow (borrowI) or merge (mergeI)

            if n != root && n.underflow then                             // unless root, check for underflow
                println (s"delete: needs to handle underflow of INTERNAL node $n")
                val j = par.find (key)                                   // j-th index position in parent
                val (sib, left) = richestSib (par, j)                    // richest sib and whether it's left
                println (s"delete: internal sib = $sib")

                if sib.rich then borrowI (n, sib, left, par, j)          // internal borrow a key from rich sib
                else mergeI (n, sib, left, par, j - 1)                   // internal merge nodes n and sib, may cause parent to underflow
            end if

        end if
    end delete
 
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return node n's richest sibling (and whether it is left/true or right/false)
     *  found from parent node.
     *  @param par  the parent node of node n that has underflowed
     *  @param i    the position of node n as a child of node par
     */
    private def richestSib (par: BpNode, i: Int): (BpNode, Boolean) =
        debug ("richSib", s"return node n's (@ $i) richest sibling (left or right)") 
        val leftn  = if i-1 >= 0 then par.ref(i-1).asInstanceOf [BpNode] else null
        val rightn = if i+1 <= par.keys then par.ref(i+1).asInstanceOf [BpNode] else null

        if leftn == null then (rightn, false)
        else if rightn == null then (leftn, true)
        else if leftn.keys >= rightn.keys then (leftn, true)
        else (rightn, false)
    end richestSib

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Borrow a key-value pair from a rich sibling, so LEAF node n won't underflow.
     *  For borrow LEFT, last key in left sib k2 moves to n, then k2 replaces k3 in par.
     *         [ ... k3 ... ]              [ ... k2 ... ]
     *  [ ... k1 k2 ]  [ k3 ... ]  TO  [ ... k1 ]  [ k2 k3 ... ]
     *  @param n     the current node that has underflowed
     *  @param sib   the rich sibling node
     *  @param left  the whether the sib is left or right
     *  @param par   the parent node
     *  @param j     the index position in the parent node
     */
    private def borrow (n: BpNode, sib: BpNode, left: Boolean, par: BpNode, j: Int): Unit =
        val i = if left then sib.keys-1 else 0
        val bkey = sib(i)
        debug ("borrow", s"key $bkey from rich sib = $sib node for node n = $n having par = $par at j = $j")
        val bref = sib.ref(i).asInstanceOf [V]
        sib.remove (i)
        add (n, bkey, bref)
        par(j-1) = if left then bkey else sib(0)                      // the divider key for parent node
    end borrow

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Borrow a key-value pair from a rich sibling, so INTERNAL node n won't underflow.
     *  For borrow LEFT, last key in left sib k2 rotates into par, whose key k3 rotates to n.
     *         [ ... k3 ... ]              [ ... k2 ... ]
     *  [ ... k1 k2 ]  [ k4 ... ]  TO  [ ... k1 ]  [ k3 k4 ... ]
     *  @param n     the current node that has underflowed
     *  @param sib   the rich sibling node
     *  @param left  the whether the sib is left or right
     *  @param par   the parent node
     *  @param j     the index position in the parent node
     */
    private def borrowI (n: BpNode, sib: BpNode, left: Boolean, par: BpNode, j: Int): Unit =
        val i = if left then sib.keys-1 else 0
//      val bkey = sib(i)
        val bkey = par(j-1)
        debug ("borrow", s"key $bkey from rich sib = $sib node for node n = $n having par = $par at j = $j")
        val bref = sib.ref(i).asInstanceOf [V]
        sib.remove (i)
        add (n, bkey, bref)
        par(j-1) = if left then bkey else sib(0)                      // the divider key for parent node
    end borrowI

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Merge LEAF node n with its sibling returning whether the parent node
     *  has underflowed.
     *  @param n     the current node that has underflowed
     *  @param sib   the sibling node
     *  @param left  the whether the sib is left or right
     *  @param par   the parent node
     *  @param j     the index position in the parent node
     */
    private def merge (n: BpNode, sib: BpNode, left: Boolean, par: BpNode, j: Int): Boolean =
        debug ("merge", s"LEAF node n = $n that underflows with sib = $sib having par = $par at j = $j")
        if left then sib.merge (n)
        else n.merge (sib)
        par.remove (j)                                                   // true means parent underflowed
    end merge

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Merge INTERNAL node n with its sibling returning whether the parent node
     *  has underflowed.
     *  @param n     the current node that has underflowed
     *  @param sib   the sibling node
     *  @param left  the whether the sib is left or right
     *  @param par   the parent node
     *  @param j     the index position in the parent node
     */
    private def mergeI (n: BpNode, sib: BpNode, left: Boolean, par: BpNode, j: Int): Boolean =
        debug ("mergeI", s"INTERNAL node n = $n that underflows with sib = $sib having par = $par at j = $j")
        if left then sib.mergeI (par.key(j), n)
        else n.mergeI (par.key(j), sib)
        par.remove (j)                                                   // true means parent underflowed
    end mergeI

//------------------------------------------------------------------------------
// Print/show the B+Tree
//------------------------------------------------------------------------------

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Show/print this B+Tree.
     */
    def show (): Unit =
        println ("BpTreeMap")
        printT (root, 0)
        println ("-" * 60)
    end show

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print this B+Tree map using a pre-order traversal and indenting each level.
     *  @param n      the current node to print
     *  @param level  the current level in the B+Tree
     */
    private def printT (n: BpNode, level: Int): Unit =
        if n != null then
            println ("\t" * level + n)
            if ! n.isLeaf then
                for j <- 0 to n.keys do printT (n.ref(j).asInstanceOf [BpNode], level + 1)
    end printT
 
end BpTreeMap


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bpTreeMapTest` main function used for testing the `BpTreeMap` class by
 *  inserting increasing key values.
 *  > runMain scalation.database.bptree.bpTreeMapTest
 */
@main def bpTreeMapTest (): Unit =

    banner ("Insert Increasing Integer Keys")
    val totKeys = 36
    val tree    = new BpTreeMap [Int] ()

    for i <- 1 until totKeys by 2 do
        banner (s"put ($i, ${i * i})")
        tree.put (i, i * i)
        tree.show ()
    end for

    banner ("Find Keys")
    for i <- 0 until totKeys do println (s"key = $i, value = ${tree.get(i)}")
    println ("-" * 60)

    banner ("Iterate Through the B+Tree")
    for it <- tree.iterator do println (it)
    println ("-" * 60)
    tree.foreach (println (_))

    banner ("Print Statistics")
    println (s"size = ${tree.size}")
    println (s"Average number of nodes accessed = ${tree.count / totKeys.toDouble}")

    banner ("Delete Keys")
    tree.show ()
    val toRemove = Array (29, 31, 33, 35, 27, 25)
    for key <- toRemove do
        banner (s"remove ($key)")
        tree.remove (key)
        tree.show ()
    end for

end bpTreeMapTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bpTreeMapTest2` main function used for testing the `BpTreeMap` class by
 *  inserting random key values.
 *  > runMain scalation.database.bptree.bpTreeMapTest2
 */
@main def bpTreeMapTest2 (): Unit =

    import java.util.Random

    banner ("Insert Random Integer Keys")
    val totKeys = 60
    val mx      = 10 * totKeys
    val seed    = 1
    val rng     = new Random (seed)
    val tree    = new BpTreeMap [Int] ()

    for i <- 1 to totKeys do
        val key = rng.nextInt (mx)
        banner (s"put ($key, ${2 * key})")
        tree.put (key, 2 * key)
        tree.show ()
    end for

    banner ("Print Statistics")
    println (s"size = ${tree.size}")
    println (s"Average number of nodes accessed = ${tree.count / totKeys.toDouble}")

end bpTreeMapTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `bpTreeMapTest3` main function used for testing the `BpTreeMap` class by
 *  inserting keys and values into B+Trees, one representing each of two lanes.
 *  > runMain scalation.database.bptree.bpTreeMapTest3
 */
@main def bpTreeMapTest3 (): Unit =

    import java.util.Random

    case class Car (vin: Int, dist: Double)

    banner ("Insert Random Integer Keys")
    val totKeys = 60
    val seed    = 1
    val rng     = new Random (seed)
    val lane1   = new BpTreeMap [Car] ()          // index for lane1
    val lane2   = new BpTreeMap [Car] ()          // index for lane2

    var dist = 0.0                                // distance from end of lane
    var ord  = 0                                  // rank order from end of lane
    for i <- 1 to totKeys do
        dist += rng.nextInt (5)
        val c_i = Car (i, dist)                   // the car being put into lane1's B+Tree
        ord += 10                                 // rank order of car toward end of lane
        banner (s"put ($ord, $c_i)")
        lane1.put (ord, c_i)
        lane1.show ()
    end for

    dist = 0.0                                    // distance from end of lane
    ord  = 0
    for i <- 1 to totKeys do
        dist += rng.nextInt (5)
        val c_i = Car (totKeys + i, dist)         // the car being put into lane2's B+Tree
        ord += 10                                 // rank order of car toward end of lane
        banner (s"put ($ord, $c_i)")
        lane2.put (ord, c_i)
        lane2.show ()
    end for

    // find the j-th car in lane1 call it car1
    // find the corresponding j-th car in lane2 call it car2
    // check whether car2 is behind car1 in the other lane
    // may need a doubly linked list of nodes at the leaf-level to search forward and backward
    // find the closest car in the other lane that is behind you
    // if its distance is large enough, make the lane change
    // may need gaps in ord so lane changing car can get an ord without making all care reassign theirs

end bpTreeMapTest3

