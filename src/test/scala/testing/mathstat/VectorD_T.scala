
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue Mar  8 18:30:36 EST 2016
 *  @see     LICENSE (MIT style license file).
 */

package testing
package mathstat

import org.junit.jupiter.api.Test

import scala.math.{abs, sqrt}

import scalation._
import scalation.mathstat._
import scalation.random.{Randi0, RandomVecD, Uniform}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/* Test all the methods for the `VectorD` class in the `scalation.mathstat` package.
 * Trivial and private methods are skipped (not tested).
 * @see `vectorDTest` in 'VectorD.scala' for additional tests.

    inline def length: Int = dim
    inline def nd          = dim.toDouble
    def expand (more: Int = dim): VectorD =
    def copy: VectorD = new VectorD (dim, copyOf (v, dim))
    def apply (i: Int): Double = v(i)
    def apply (r: Range): VectorD = 
    def apply (idx: IndexedSeq [Int]): VectorD = VectorD (for i <- idx.indices yield v(idx(i)))
    def apply (idx: IIndexedSeq [Int]): VectorD = VectorD (for i <- idx.indices yield v(idx(i)))
    def apply (idx: Array [Int]): VectorD = VectorD (for i <- idx.indices yield v(idx(i)))
    def not (ix: Int): VectorD =
    def not (idx: IndexedSeq [Int]): VectorD =
    def split (idx: IndexedSeq [Int]): (VectorD, VectorD) = (this(idx), not(idx))
    def split (idx: VectorI): (VectorD, VectorD) = { val idx_ = idx.toMuIndexedSeq; (this(idx_), not(idx_)) }
    def split (i: Int): (VectorD, VectorD) = (new VectorD (i, v.slice (0, i)),
    def chop (k: Int): Array [VectorD] =
    def update (i: Int, a: Double): Unit = v(i) = a
    def update (r: Range, a: Double): Unit = for i <- r do v(i) = a
    def update (r: Range, y: IndexedSeq [Double]): Unit = for i <- r do v(i) = y(i)
    def set (a: Double): Unit = for i <- v.indices do v(i) = a
    def set (y: IndexedSeq [Double]): Unit   = for i <- v.indices do v(i) = y(i)
    override def foreach [U] (f: Double => U): Unit = { var i = 0; while i < dim do { f (v(i)); i += 1 } }
    def map (f: FunctionS2S): VectorD = new VectorD (v.size, v.map (f))
    def tryCompareTo [B >: VectorD: AsPartiallyOrdered] (bb: B): Option [Int] =
    def findInfinity: IIndexedSeq [Int] = for i <- indices if v(i).isInfinite yield i
    def isNonnegative: Boolean = v.forall (_ >= 0.0)
    def ++ (y: IndexedSeq [Double]): VectorD = new VectorD (dim + y.size, v ++ y)
    def +: (a: Double): VectorD = new VectorD (dim + 1, a +: v)
    def :+ (a: Double): VectorD = new VectorD (dim + 1, v :+ a)
    def unary_- : VectorD = VectorD (for i <- v.indices yield -v(i))
    def + (y: IndexedSeq [Double]): VectorD = VectorD (for i <- v.indices yield v(i) + y(i))
    def - (y: IndexedSeq [Double]): VectorD = VectorD (for i <- v.indices yield v(i) - y(i))
    def * (y: IndexedSeq [Double]): VectorD = VectorD (for i <- v.indices yield v(i) * y(i))
    def / (y: IndexedSeq [Double]): VectorD = VectorD (for i <- v.indices yield v(i) / y(i))
    def + (a: Double): VectorD = VectorD (for i <- v.indices yield v(i) + a)
    def - (a: Double): VectorD = VectorD (for i <- v.indices yield v(i) - a)
    def * (a: Double): VectorD = VectorD (for i <- v.indices yield v(i) * a)
    def / (a: Double): VectorD = VectorD (for i <- v.indices yield v(i) / a)
    def += (y: IndexedSeq [Double]): VectorD = { for i <- v.indices do v(i) += y(i); this }
    def -= (y: IndexedSeq [Double]): VectorD = { for i <- v.indices do v(i) -= y(i); this }
    def *= (y: IndexedSeq [Double]): VectorD = { for i <- v.indices do v(i) *= y(i); this }
    def /= (y: IndexedSeq [Double]): VectorD = { for i <- v.indices do v(i) /= y(i); this }
    def += (a: Double): VectorD = { for i <- v.indices do v(i) += a; this }
    def -= (a: Double): VectorD = { for i <- v.indices do v(i) -= a; this }
    def *= (a: Double): VectorD = { for i <- v.indices do v(i) *= a; this }
    def /= (a: Double): VectorD = { for i <- v.indices do v(i) /= a; this }
    def + (ia: (Int, Double)): VectorD = { val c = copy; c.v(ia._1) += ia._2; c }
    def - (ia: (Int, Double)): VectorD = { val c = copy; c.v(ia._1) -= ia._2; c }
    def ~^ (a: Double): VectorD = VectorD (for i <- v.indices yield v(i) ~^ a)
    def =~ (y: VectorD): Boolean =
    def sqrt: VectorD = VectorD (for i <- v.indices yield math.sqrt (v(i)))
    def recip: VectorD = VectorD (for i <- v.indices yield 1 / v(i))
    def diff (y: IndexedSeq [Double]): VectorD = { val a = v diff y; new VectorD (a.size, a) }
    def intersect (y: IndexedSeq [Double]): VectorD = { val a = v intersect y; new VectorD (a.size, a) }
    override def distinct: VectorD = { val a = v.distinct; new VectorD (a.size, a) }
    override def reverse: VectorD = { val a = v.reverse; new VectorD (a.size, a) }
    def isSorted: Boolean =
    def sorted: VectorD = { val a = v.sorted; new VectorD (a.size, a) }
    override def sortWith (cmp: (Double, Double) => Boolean): VectorD =
    override def filter (p: Double => Boolean): VectorD = 
    override def filterNot (p: Double => Boolean): VectorD = 
    def filterPos (p: Double => Boolean): IIndexedSeq [Int] = for i <- v.indices if p(v(i)) yield i
    def dot (y: IndexedSeq [Double]): Double =
    def maxv (y: IndexedSeq [Double]): VectorD =
    def minv (y: IndexedSeq [Double]): VectorD =
    def argmax (e: Int = dim): Int =
    def argmax (s: Int, e: Int): Int =
    def argmin (e: Int = dim): Int =
    def argmin (s: Int, e: Int): Int =
    def argmag (e: Int = dim): Int =
    def mag: Double = math.max (math.abs (min), math.abs (max))
    def sums (k: Int): (Double, Double, Double) =
    def normSqs (k: Int): (Double, Double, Double) =
    def normSq: Double = v.fold (0.0)((s, e) => s + e*e)
    def norm: Double   = math.sqrt (normSq)
    def norm1: Double = v.fold (0.0)((s, e) => s + math.abs (e))
    def abs: VectorD = VectorD (for i <- v.indices yield math.abs (v(i)))
    def mids: VectorD = VectorD (for i <- 1 until dim yield 0.5 * (v(i) + v(i-1)))
    def cumulate: VectorD = { var s = 0.0; VectorD (for i <- v.indices yield { s += v(i); s })}
    def toInt: VectorI = new VectorI (dim, v.map (_.toInt))
    def toDouble: VectorD = this
    def toProbability: VectorD = this * (1.0 / v.sum)
    def normalize: VectorD = this * (1.0 / norm)
    def normalize1: VectorD = this * (1.0 / max)
    def countZero: Int =
    override def toString: String =
    def toString2: VectorS = new VectorS (dim, v.map (_.toString))
    def swap (i: Int, k: Int): Unit =
    private def median (rk: Array [Int], p: Int, r: Int, k: Int): Double =
    def median (k: Int = (dim+1)/2): Double =
    def quantile (fraction: Double): Double =
    def median_ : Double = if dim % 2 == 0 then (median () + median ((dim+2)/2)) / 2.0
    def reorder (rank: Array [Int]): VectorD = VectorD (for i <- indices yield v(rank(i)))
    def iqsort: Array [Int] = iqsort (Array.range (0, dim), 0, dim-1)
    private def iselsort (rk: Array [Int], p: Int, r: Int): Array [Int] =
    def iselsort: Array [Int] = iselsort (Array.range (0, dim), 0, dim-1)
    def iselsort (end: Int = dim): Array [Int] = iselsort (Array.range (0, end), 0, end-1)
    inline private def med3 (i: Int, j: Int, k: Int): Int =
    inline private def iswap (rk: Array [Int], i: Int, j: Int): Unit =
    inline def mean: Double = v.sum / nd
    def cnormSq: Double =
    def variance: Double   = cnormSq / (nd-1)
    def variance_ : Double = cnormSq / nd
    def variance (mu: Double): Double  = (normSq - mu * mu * nd) / (nd-1)
    def variance_ (mu: Double): Double = (normSq - mu * mu * nd) / nd
    def cov (y: IndexedSeq [Double]): Double  = ((this dot y) - v.sum * y.sum / nd) / (nd-1)
    def cov_ (y: IndexedSeq [Double]): Double = ((this dot y) - v.sum * y.sum / nd) / nd
    def ms: Double  = normSq / nd
    def rms: Double = math.sqrt (ms)
    def stdev: Double   = math.sqrt (variance)
    def stdev_ : Double = math.sqrt (variance_)
    def acov (k: Int = 1): Double =
    def acov_ (k: Int = 1): Double =
    def corr (y: VectorD): Double =
    def acorr (k: Int = 1): Double  = acov (k) / variance
    def acorr_ (k: Int = 1): Double =
    def scorr (y: VectorD): Double =
    def skew: Double   = ((this - mean)~^3).sum / (nd * stdev_ ~^3)
    def skew_ : Double = skew * math.sqrt (nd * (nd-1)) / (nd-2)
    def kurtosis: Double = ((this - mean)~^4).sum / (nd * variance_ ~^2)
    def kurtosis_ : Double = (nd-1) * ((nd+1) * kurtosis - 3 * (nd-1)) / ((nd-2) * (nd-3)) + 3
    def standardize: VectorD = (this - mean) / stdev
*/


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VectorD_T` driver class conducts unit testing on the `VectorD` class
 *  by invoking the `VectorD_T` testing object.  Run 'test-only' to test `VectorD`
 *  or 'test' to run all unit tests.
 *------------------------------------------------------------------------------
 *  > test-only testing.mathstat.VectorD_T
 *  > test
 */
class VectorD_T:

    @Test def testAll () = VectorD_T

end VectorD_T


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `VectorD_T` object conducts unit testing on the `VectorD` class using the
 *  `Tester` trait.  It compares correctness/performance of a method/operator 'call'
 *  to an 'oracle' and optionally a 'contender'.
 *------------------------------------------------------------------------------
 *  All methods except 'this', 'apply', 'update', 'foreach' and 'hashCode' should be tested.
 *  May skip '=op' if 'op' is tested, e.g., skip '+=' if '+' is tested.
 *  Also the 'equals' and 'toString' are tested implicitly.
 *  Depending on the 'CORRECT' flag, it will either test correctness or performance.
 *  Note, if the code for the 'contender' or even the 'oracle' is significantly faster,
 *  the method/operator may need be to re-coded.
 */
object VectorD_T extends Tester:

    // Reassign parameters from `Tester` trait as needed

    DEBUG   = false                                                // debug flag
    CORRECT = true                                                 // test correctness/performance
    FOCUS   = ""                                                   // method/operator to focus on, "" => all
    KLASS   = "VectorD"                                            // the class under test
    ITER    = 100                                                  // number of test iterations

    // Size parameter(s) used for variables in 'test' (customize per class)

    private val dim = 10                                           // vector dimension/size

    // Random variate generators (customize per class)

//  private val rvg = RandomVecD (dim)                             // random vector generator
    private val rvg = RandomVecD (dim = dim, density = 0.5)        // random vector generator
    private val rsg = Uniform (0.0, 100.0)                         // random scalar/double generator
    private val rig = Randi0 (0, dim - 1)                          // random integer/index generator

    // Variables used in 'test' (customize per class)

    private val x = new VectorD (dim)                              // first vector
    private val y = new VectorD (dim)                              // second vector
    private var s = 0.0                                            // scalar value
    private var j = 0                                              // first integer/index value
    private var k = 0                                              // second integer/index value

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomize all variables used in `Tester`s 'test' method.
     */
    def randomize (): Unit =
        x.set (rvg.gen)                                            // randomly reset variables
        y.set (rvg.gen)
        s = rsg.gen
        j = rig.igen
        k = rig.igen
    end randomize

    testClass ()

    println ("\nTest no argument methods/unary operators")
 
    test ("unary-",         -x,            
                            VectorD (for i <- x.indices yield -x(i)))
    test ("abs",            x.abs,
                            VectorD (for i <- x.indices yield abs (x(i))))
    test ("argmax",         x.argmax (),
                            x.indexOf (x.max))
    test ("argmin",         x.argmin (),
                            x.indexOf (x.min))
    test ("cumulate",       x.cumulate,
                            { var sum = 0.0; VectorD (for i <- x.indices yield { sum += x(i); sum }) })

    test ("expand",         x.expand (),
                            x ++ new VectorD (x.dim))
    test ("isNonnegative",  x.isNonnegative,
                            ! x.exists (_ < 0.0))
    test ("normSq",         x.normSq,
                            x dot x)
    test ("norm",           x.norm, 
                            sqrt (x.normSq))
    test ("norm1",          x.norm1,
                            x.abs.sum)
    test ("normalize",      x.normalize,
                            x * (1.0 / x.sum))
    test ("normalize1",     x.normalize1,
                            x * (1.0 / x.max))
    test ("recip",          x.recip,
                            VectorD.one (x.dim) / x)
    test ("reverse",        x.reverse.reverse,
                            x)
    test ("sorted",         { x.sorted; x.isSorted },
                            true)
    test ("swap",           { x.swap (j, k); x },
                            { val t = x(k); x(k) = x(j); x(j) = t; x })
    test ("toInt",          x.toInt,
                            VectorI (for i <- x.indices yield x(i).toInt))

    println ("\nTest methods/operators that take parameters")

    test ("+",              x + y,
                            VectorD (for i <- x.indices yield x(i) + y(i)))
    test ("+",              x + s,
                            VectorD (for i <- x.indices yield x(i) + s))
    test ("+",              x + (1, s),
                            { x(1) += s; x })
    test ("-",              x - y,
                            VectorD (for i <- x.indices yield x(i) - y(i)))
    test ("-",              x - s,
                            VectorD (for i <- x.indices yield x(i) - s))
    test ("-",              x - (1, s),
                            { x(1) -= s; x })
    test ("*",              x * y,
                            VectorD (for i <- x.indices yield x(i) * y(i)))
    test ("*",              x * s,
                            VectorD (for i <- x.indices yield x(i) * s))
    test ("/",              x / y,
                            VectorD (for i <- x.indices yield x(i) / y(i)))
    test ("/",              x / s,
                            VectorD (for i <- x.indices yield x(i) / s))
    test ("~^",             x ~^ s,
                            VectorD (for i <- x.indices yield x(i) ~^ s))          // FIX: may fail 
    test ("dot",            x dot y,
                            (x * y).sum)
    test ("filter",         x.filter (_ > s),
                            VectorD (for z <- x if z > s yield z))
    test ("filterPos",      VectorI (x.filterPos (_ > s)),
                            VectorI (x.zipWithIndex.filter (_._1 > s).map (_._2)))
    test ("map",            x.map ((z: Double) => z * s),
                            VectorD (for z <- x yield z * s))
    test ("maxv",           x maxv y,
                            VectorD (for i <- x.indices yield x(i) max y(i)))
    test ("minv",           x minv y,
                            VectorD (for i <- x.indices yield x(i) min y(i)))
    test ("set",            { x set s; x },
                            new VectorD (x.dim, Array.fill (x.dim)(s)))
    test ("set",            { x set y; x },
                            VectorD (for z <- y yield z))

end VectorD_T

