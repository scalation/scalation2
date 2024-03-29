
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Michael Cotterell, Hao Peng, Dong-Yu Yu
 *  @version 2.0
 *  @date    Thu Sep 22 21:45:58 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    B-Spline Basis Functions (e.g., used in Functional Data Analysis)
 *
 *  @see en.wikipedia.org/wiki/B-spline
 *  @see cran.r-project.org/web/packages/crs/vignettes/spline_primer.pdf
 *  @see http://web.mit.edu/hyperbook/Patrikalakis-Maekawa-Cho/node17.html
 *  @see open.uct.ac.za/bitstream/item/16664/thesis_sci_2015_essomba_rene_franck.pdf?sequence=1
 */

package scalation
package calculus

import scala.math.sqrt

import scalation.mathstat._

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `B_Spline` class provides B-Spline basis functions for various orders 'm',
 *  where the order is one more than the degree.  A spline function is a piecewise
 *  polynomial function where the pieces are stitched together at knots with the
 *  goal of maintaining continuity and differentiability. B-Spline basis functions
 *  form a popular form of basis functions used in Functional Data Analysis.
 *  @see http://web.mit.edu/hyperbook/Patrikalakis-Maekawa-Cho/node17.html
 *-----------------------------------------------------------------------------
 *  @param ττ     the time-points of the original knots in the time dimension
 *  @param mMax   the maximum order, allowing splines orders from 1 to mMax
 *  @param clamp  flag for augmenting ττ
 */
class B_Spline (ττ: VectorD, mMax: Int = 4, clamp: Boolean = true)
      extends BasisFunction:

    private val debug   = debugf ("B_Spline", true)                  // debug function
    private val flaw    = flawf ("B_Spline")                         // flaw function
    protected val l     = ττ.dim - 1                                 // the number of intervals
    protected val τ     = if clamp then B_Spline.clamp (mMax, ττ, true) // augment to accommodate knots
                          else ττ
    protected val head  = τ(0)
    protected val tail  = τ.last

    if mMax < 1 || mMax > 10 then flaw ("init", "B_Spline order restricted to 1 thru 10")

    debug ("init", s"B_Spline (ττ = $ττ, mMax = $mMax) with intervals l = $l, augmented τ = $τ")

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Range of "usable" splines when using the `bs` function. This is needed,
     *  since extra splines may be generated by the general B-spline recurrence.
     *  @param m  the order of the spline
     */
    def range (m: Int = mMax): Range = 0 to l + m - 2

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** The number of usable spline basis functions for a specified order,
     *  given the configured knot vector.
     *  @param m  the order of the spline
     */
    def size (m: Int = mMax): Int = l + m - 1

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order one 'm = 1' B-Spline basis functions (flat functions).
     *  @param j  indicates which spline function 0 to l-1
     *  @param t  the time parameter
     */
    private def bb1 (j: Int, t: Double): Double =
        if      j == size()-1 && t =~ tail then 1.0
        else if (τ(j) < t || τ(j) =~ t) && (t < τ(j+1)) then 1.0
        else    0.0
    end bb1

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Order 'm' B-Spline basis functions (general recurrence).
     *  @param m  the order of the spline function (degree = order - 1)
     *  @param j  indicates which spline function
     *  @param t  the time parameter
     */
    def bb (m: Int)(j: Int)(t: Double): Double =
        if mMax < m then flaw ("bb", s"mMax = $mMax can't be less than m = $m")
        if m == 1 then return bb1 (j, t)
        val f1 = bb (m-1)(j)(t)
        val f2 = bb (m-1)(j+1)(t)
        val a = if f1 =~ 0 then 0 else (t - τ(j)) * f1 / (τ(j+m-1) - τ(j))
        val b = if f2 =~ 0 then 0 else (τ(j+m) - t) * f2 / (τ(j+m) - τ(j+1))
        a + b
    end bb

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Adjusted order 'm' B-Spline basis functions (general recurrence). These
     *  are adjusted so that the first "usable" spline is at `j = 0`. The valid
     *  range of usable splines is defined in `range`.
     *  @param m  the order of the spline function (degree = order - 1)
     *  @param j  indicates which spline function
     *  @param t  the time parameter
     */
    def bfr (m: Int)(j: Int)(t: Double): Double = bb (m)(j+mMax-m)(t)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluate the 'j+1'-th order `m` B-spline basis function
     *  at `t` non-recursively, using an efficient dynamic programming approach.
     *  ==Example==
     *  {{{
     *  val m = 4                       // order m (degree m-1)
     *  val t = VectorD (1, 2, 3, 4)    // original time points
     *  val N = B_Spline (t, k)         // B_Spline instance
     *  val j = 0                       // spline j in N.range(m)
     *  val z = N.bf (m)(j)(t(2))       // evaluate at t(2)
     *  println ("order k basis function j at t(2) = z")
     *  }}}
     *  ==Implementation Details==
     *  Let `N(m)(i)` denote the `i`-th order `k` basis function evaluated at
     *  `t`. Then each `N(m)(i)` depends on the evaluation of `N(m-1)(i)` and
     *  `N(m-1)(i+1)`. Here is an example, given a set of order `m=4` B-spline
     *  basis functions and knot vector `τ` of length `n`:
     *  {{{
     *  N(1)(0), N(1)(1), N(1)(2), N(1)(3)
     *        |/       |/       |/
     *  N(2)(0), N(2)(1), N(2)(2)
     *        |/       |/
     *  N(3)(0), N(3)(1)
     *        |/
     *  N(4)(0)
     *  }}}
     *  This algorithm applies the procedure described above using O(k)
     *  storage and O(k*k) floating point operations.
     *  @param t  point to evaluate
     *  @author Michael Cotterell, Hao Peng
     *  @see Carl de Boor (1978). A Practical Guide to Splines. Springer-Verlag. ISBN 3-540-90356-9.
     */
    def bf (m: Int)(j: Int)(t: Double): Double =
        val N = Array.ofDim [Double] (m)                            // all evaluations; overwrite as needed
        for k <- 0 until m do                                       // build for each order 1 <= (k+1) <= m
            val nf = m - k                                          // number of basis functions for order (k+1)
            if      j == 0         && t =~ head then N(0) = 1.0     // trivial case: near first knot
            else if j == size(m)-1 && t =~ tail then N(0) = 1.0     // trivial case: near last knot
            else if k == 0 then                                     // evaluate order 1
                for i <- 0 until nf do
                    N(i) = if (τ(i+j) < t || τ(i+j) =~ t) && (t < τ(i+j+1)) then 1.0 else 0.0
                end for
            else if k > 0 then                                      // evaluate orders (k+1) <= k
                for i <- 0 until nf do
                    val bs1 = N(i)
                    val bs2 = N(i+1)
                    // using "== 0" instead of "=~ 0" should be safe
                    val bf1 = if bs1 == 0 then 0 else (t - τ(i+j)) * bs1 / (τ(i+j+k) - τ(i+j))
                    val bf2 = if bs2 == 0 then 0 else (τ(i+j+k+1) - t) * bs2 / (τ(i+j+k+1) - τ(i+j+1))
                    N(i) = bf1 + bf2
                end for
            end if
        end for
        N(0)
    end bf

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Evaluate each of the `τ.dim-1`-many order `m` B-spline basis functions
     *  at `t` non-recursively, using an efficient dynamic programming approach.
     *  ==Example==
     *  {{{
     *  val m = 4                       // order m (degree m-1)
     *  val t = VectorD (1, 2, 3, 4)    // original time points
     *  val N = B_Spline (t, k)         // B_Spline instance
     *  val z = N.abf_ (m)(t)           // vector of evaluations
     *  println ("order k basis functions at t = z")
     *  }}}
     *  ==Implementation Details==
     *  Let `N(m)(i)` denote the `i`-th order `k` basis function evaluated at
     *  `t`. Then each `N(m)(i)` depends on the evaluation of `N(m-1)(i)` and
     *  `N(m-1)(i+1)`. Here is an example, given a set of order `m=4` B-spline
     *  basis functions and knot vector `τ` of length `n`:
     *  {{{
     *  N(1)(0), N(1)(1), N(1)(2), N(1)(3), ..., N(1)(n-1)
     *        |/       |/       |/            |/
     *  N(2)(0), N(2)(1), N(2)(2), ..., N(2)(n-2)
     *        |/       |/            |/
     *  N(3)(0), N(3)(1), ..., N(3)(n-3)
     *        |/            |/
     *  N(4)(0), ..., N(4)(n-4)
     *  }}}
     *  This algorithm applies the procedure described above using O(n)
     *  storage and O(k*n) floating point operations.
     *  @param t  point to evaluate
     *  @author   Michael Cotterell
     *  @see      Carl de Boor (1978). A Practical Guide to Splines. Springer-Verlag. ISBN 3-540-90356-9.
     */
    override def abf_ (m: Int = mMax)(t: Double): VectorD =
        val N = Array.ofDim [Double] (τ.dim-1)           // all evaluations; overwrite as needed
        for k <- 0 until m do                            // build for each order 1 <= (k+1) <= m
            val nf = τ.dim - k - 1                       // number of basis functions for order (k+1)
            if      t =~ head then N(   0) = 1.0         // trivial case: near first knot
            else if t =~ tail then N(nf-1) = 1.0         // trivial case: near last knot
            else if k == 0 then                          // evaluate order 1
                for i <- 0 until nf-1 do
                    N(i) = if (τ(i) < t || τ(i) =~ t) && (t < τ(i+1)) then 1.0 else 0.0
                end for
            else if k > 0 then                           // evaluate orders (k+1) <= k
                for i <- 0 until nf do
                    val bs1 = N(i)
                    val bs2 = N(i+1)
                    // using "== 0" instead of "=~ 0" should be safe
                    val bf1 = if bs1 == 0 then 0 else (t - τ(i)) * bs1 / (τ(i+k) - τ(i))
                    val bf2 = if bs2 == 0 then 0 else (τ(i+k+1) - t) * bs2 / (τ(i+k+1) - τ(i+1))
                    N(i) = bf1 + bf2
                end for
            end if
        end for
        new VectorD (τ.dim - m, N)                       // do not allocate new storage
    end abf_

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the order of the this B_Spline.
     */
    def getOrder: Int = mMax

end B_Spline


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Companion object for `B_Spline` class provides functions for clamping the
 *  the ends of a spline and running timing benchmarks.
 */
object B_Spline:

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return a "clamped" version of the input vector, augmented to ensure
     *  that each end has `m`-many repeated points.  If `isInclusive` is true,
     *  then the first and last values of the vector are repeated; otherwise,
     *  the values `t(0)-sqrt(EPSILON)` and `t(t.dim-1)+sqrt(EPSILON)` are
     *  repeated for the beginning and end, respectively.
     *  @param m            intended B-spline order (degree = m - 1)
     *  @param t            non-decreasing vector of time points
     *  @param isInclusive  repeat end points (default = true)
     *  @author             Michael Cotterell
     */
    def clamp (m: Int, t: VectorD, isInclusive: Boolean = true): VectorD =
        val e    = sqrt (EPSILON)
        val head = t(0)
        val tail = t.last
        if isInclusive then VectorD.fill (m-1)(head-e) ++ t ++ VectorD.fill (m-1)(tail+e)
        else VectorD.fill (m)(head-e) ++ t ++ VectorD.fill (m)(tail+e)
    end clamp

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Gather timing statistics for repeated executions of a block, prints the
     *  statistics, and returns the value produced by the block.
     *  @tparam R          result type of block
     *  @param reps        number of replications
     *  @param useSeconds  record time as seconds instead of milliseconds (default = false)
     *  @param title       title for statistic
     *  @param block       block of code to execute
     */
    def benchmark [R] (reps: Int = 100, useSeconds: Boolean = false, title: String = "benchmark") (block: => R): R =
        val (r, stat) = benchmarked (reps, useSeconds, title) (block)
        println (Statistic.labels)
        println (stat)
        r
    end benchmark

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Gather timing statistics for repeated executions of a block and returns
     *  a tuple containing the return value produced by the block and the
     *  `Statistic` instance used to gather the statistics.
     *  @tparam R          result type of block
     *  @param reps        number of replications
     *  @param useSeconds  record time as seconds instead of milliseconds (default = false)
     *  @param title       title for statistic
     *  @param block       block of code to execute
     */
    def benchmarked [R] (reps: Int = 100, useSeconds: Boolean = false, title: String = "benchmark") (block: => R):
        (R, Statistic) =
        val stat = new Statistic (title)
        for i <- 0 until reps-1 do
            val (r, ms) = timed (block)
            stat.tally (if useSeconds then ms / 1000 else ms)
        end for
        val (r, ms) = timed (block)
        stat.tally (if useSeconds then ms / 1000 else ms)
        (r, stat)
    end benchmarked

end B_Spline


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `b_SplineTest` object is used to test the `B_Spline` class.
 *  It tests the B-Spline functions using the general recurrence.
 *  > runMain scalation.calculus.b_SplineTest
 */
@main def b_SplineTest (): Unit =

    val mM = 4                                               // maximum order to test
    val τ  = VectorD (0.0, 20.0, 40.0, 60.0, 80.0, 100.0)    // knot time-points
    val bs = new B_Spline (τ, mM)                            // B-Spline generator
    val n  = 100                                             // number of time-points for plotting
    val t  = VectorD.range (0, n)                            // time-points for plotting

    for m <- 1 to mM do
        //---------------------------------------------------------------------
        // order m B-Splines (polynomial functions)

        val y1 = new VectorD (n)
        val y2 = new VectorD (n)

        for i <- 0 until n do
            y1(i) = bs.bf (m)(1)(t(i))                     // first "interesting" B-Spline
            y2(i) = bs.bf (m)(2)(t(i))                     // next B-Spline
        end for
        new Plot (t, y1, y2, "B-Spline order " + m)
    end for

end b_SplineTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `b_SplineTest2` object is used to test the `B_Spline` class.
 *  It tests the B-Spline functions using the general recurrence and plots
 *  several basis functions using `PlotM`.
 *  > runMain scalation.calculus.b_SplineTest2
 */
@main def b_SplineTest2 (): Unit =

    val mM = 5                                               // maximum order to test
    val τ  = VectorD (0.0, 50.0, 100.0)                      // knot time-points
    val bs = new B_Spline (τ, mM)                            // B-Spline generator
    val n  = 100                                             // number of time-points for plotting
    val t  = VectorD.range (0, n)/(n-1)*n                    // time-points for plotting

    for m <- 2 to 4 do
        //---------------------------------------------------------------------
        // order m B-Splines (polynomial functions)
        val y = new MatrixD (τ.dim + mM, n)                  // matrix to hold initial B-Splines
        for i <- 0 until n; j <- 0 to bs.range(m).last do
            y(j, i) = bs (m)(j)(t(i))
        end for
        new PlotM (t, y, null, "B-Spline order " + m)
    end for

end b_SplineTest2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `b_SplineTest3` object is used to test the `B_Spline` class.
 *  It tests the B-Spline functions using the general recurrence.
 *  > runMain scalation.calculus.b_SplineTest3
 */
@main def b_SplineTest3 (): Unit =

    import scalation.calculus.BasisFunction.plot
    val mM = 4                                        // maximum order to test
    val n  = 100                                      // number of time points
    val τ  = VectorD (0.0, 0.5 * (n-1), (n-1)) / (n)  // knot vector (unaugmented)
    val bs = new B_Spline (τ, mM)                     // B-Spline generator
    val t  = VectorD.range (0, n) / n                 // time-points for plotting
    for m <- 1 to mM do plot (bs, m, t)               // plot

end b_SplineTest3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `b_SplineTest4` object is used to test the `B_Spline` class.
 *  It tests the B-Spline functions using the general recurrence.
 *  > runMain scalation.calculus.b_SplineTest4
 */
@main def b_SplineTest4 (): Unit =

    val mM = 4                                        // maximum order to test
    val t  = VectorD.range (1, 11) / 10               // time-points for plotting
    val bs = new B_Spline (t, mM)                     // B-Spline generator

    for j <- bs.range (mM) do
        println (s"j = $j")
        val m1 = VectorD (for i <- t.indices yield bs.bf (mM)(j)(t(i)))
        val m2 = VectorD (for i <- t.indices yield bs.bfr(mM)(j)(t(i)))
        println (s"m1 = $m1")
        println (s"m2 = $m2")
        val diff = m1 - m2
        val e2 = diff dot diff
        println (s"e2 = $e2")
    end for

end b_SplineTest4

