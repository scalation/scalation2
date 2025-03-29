
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Hao Peng
 *  @version 2.0
 *  @date    Thu May 10 15:50:15 EDT 2018
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Tensor (3D) Algebra
 *
 *  @see www.stat.uchicago.edu/~lekheng/work/icm1.pdf
 *  @see www.math.ias.edu/csdm/files/13-14/Gnang_Pa_Fi_2014.pdf
 *  @see www.kolda.net/publication/TensorReview.pdf
 *  @see tspace.l
 */

package scalation
package mathstat

import scala.collection.mutable.IndexedSeq
import scala.math.round
import scala.runtime.ScalaRunTime.stringOf

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Tensorize a vector function (V2V) by applying it to each (row, column) of a tensor.
 *  @param f  the vector function to tensorize
 *  @param x  the tensor to apply the function to
 */
def tensorize (f: FunctionV2V)(x: TensorD): TensorD =
    TensorD (x.dim, for i <- x.indices; j <- x.indices2 yield f(x(i, j)))


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Return the complement of index positions idx, e.g.,
 *  comple (Array (1, 3), 5) = Array (0, 2, 4).
 *  param idx  the index positions to be complemented 
 *  param dim  the exclusive upper bound
 */
def comple (idx: Array [Int], dim: Int): Array [Int] =
    val a = Array.ofDim [Int] (dim - idx.size)
    var j, l = 0
    for i <- idx do
        while j < i do
            a(l) = j; j += 1; l += 1
        end while
        j += 1;
    end for
    a
end comple


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TensorD` class is a simple implementation for 3-dimensional tensors.
 *  The names of the dimensions corresponds to MATLAB (row, column, sheet).
 *  @see www.kolda.net/publication/TensorReview.pdf for details on layout
 *  @see `RTensorD` for non-rectangular (ragged) tensors.
 *  @param dim   size of the 1st level/dimension (row) of the tensor (height)
 *  @param dim2  size of the 2nd level/dimension (column) of the tensor (width)
 *  @param dim3  size of the 3rd level/dimension (sheet) of the tensor (depth)
 *  @param v     the 3D array for holding the tensor elements
 */
class TensorD (val dim: Int, val dim2: Int, val dim3: Int,
              private [mathstat] var v: Array [Array [Array [Double]]] = null)
      extends Serializable:

    private val flaw =  flawf ("TensorD")                     // flaw flag
    private val TAB  = "\t\t"                                 // use "\t" for scala and "\t\t" for sbt

    val indices  = 0 until dim                                // index range for the first level/dimension
    val indices2 = 0 until dim2                               // index range for the second level/dimension
    val indices3 = 0 until dim3                               // index range for the third level/dimension

    /** Multi-dimensional array storage for tensor
     */
    if v == null then
        v = Array.ofDim [Double] (dim, dim2, dim3)
    else if dim != v.length || dim2 != v(0).length || dim3 != v(0)(0).length then
        flaw ("init", "dimensions are wrong")
    end if

    /** Format string used for printing vector values (change using setFormat)
     */
    protected var fString = "%g,\t"

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a dim by dim by dim cubic tensor.
     *  @param dim  the row and column dimension
     */
    def this (dim: Int) = { this (dim, dim, dim) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Construct a tensor from three dimensional array.
     *  @param u  the three dimensional array
     */
    def this (u: Array [Array [Array [Double]]]) = { this (u.size, u(0).size, u(0)(0).size, u) }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the row, column and sheet dimensions of this tensor.
     */
    inline def dims: (Int, Int, Int) = (dim, dim2, dim3)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the format to the newFormat.
     *  @param newFormat  the new format string
     */
    def setFormat (newFormat: String): Unit = fString = newFormat

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the i, j, k-th SCALAR element from the tensor x_ijk.
     *  @param i  the 1st dimension (row) index of the tensor
     *  @param j  the 2nd dimension (column) index of the tensor
     *  @param k  the 3rd dimension (sheet) index of the tensor
     */
    def apply (i: Int, j: Int, k: Int): Double = v(i)(j)(k)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the i, j-th VECTOR from the tensor x_ij:.
     *  @param i  the 1st dimension (row) index of the tensor
     *  @param j  the 2nd dimension (column) index of the tensor
     */
    def apply (i: Int, j: Int): VectorD = VectorD (v(i)(j).toIndexedSeq)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the i, k-th VECTOR from the tensor x_i:k.
     *  @param i    the 1st dimension (row) index of the tensor
     *  @param all  use the all columns indicator ?
     *  @param k    the 3rd dimension (sheet) index of the tensor
     */
    def apply (i: Int, all: Char, k: Int): VectorD = 
        val a = Array.ofDim [Double] (dim2)
        for j <- 0 until dim2 do a(j) = v(i)(j)(k)
        new VectorD (dim2, a)
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the j, k-th VECTOR from the tensor x_:jk.
     *  @param all  use the all rows indicator ?
     *  @param j    the 2nd dimension (column) index of the tensor
     *  @param k    the 3rd dimension (sheet) index of the tensor
     */
    def apply (all: Char, j: Int, k: Int): VectorD = 
        val a = Array.ofDim [Double] (dim)
        for i <- 0 until dim do a(i) = v(i)(j)(k)
        new VectorD (dim, a)
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the i-th ROW FIXED MATRIX from the tensor (horizontal slice x_i::).
     *  @see www.kolda.net/publication/TensorReview.pdf
     *  @param i  the 1st dimension (row) index of the tensor
     */
    def apply (i: Int): MatrixD = new MatrixD (dim2, dim3, v(i))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the j-th COLUMN FIXED MATRIX from the tensor (lateral slice x_:j:).
     *  @see www.kolda.net/publication/TensorReview.pdf
     *  @param all  use the all rows indicator ?
     *  @param j    the 2nd dimension (column) index of the tensor
     */
    def apply (all: Char, j: Int): MatrixD =
        val a = Array.ofDim [Double] (dim, dim3)
        for i <- 0 until dim; k <- 0 until dim3 do a(i)(k) = v(i)(j)(k)
        new MatrixD (dim, dim3, a)
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the k-th SHEET FIXED MATRIX from the tensor (frontal slice x_::k).
     *  @see www.kolda.net/publication/TensorReview.pdf
     *  @param all   use the all rows indicator ?
     *  @param all2  use the all columns indicator ?
     *  @param k     the 3rd dimension (sheet) index of the tensor
     */
    inline def apply (all: Char, all2: Char, k: Int): MatrixD =
        val a = Array.ofDim [Double] (dim, dim2)
        for i <- 0 until dim; j <- 0 until dim2 do a(i)(j) = v(i)(j)(k)
        new MatrixD (dim, dim2, a)
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the ii._1 to ii._2 row slice of the tensor.
     *  @param ii  1st dimension (row) indices of the tensor
     */
    def apply (ii: (Int, Int)): TensorD = new TensorD (v.slice (ii._1, ii._2))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the ii._1 to ii._2, jj._1 to jj._2 row-column slice of the tensor.
     *  @param ii  1st dimension (row) indices of the tensor (null => all)
     *  @param jj  2nd dimension (column) indices of the tensor
     */
    def apply (ii: (Int, Int), jj: (Int, Int)): TensorD =
        val (i1, i2) = if ii == null then (0, dim) else ii
        val u = v.slice (i1, i2)
        for i <- u.indices do u(i) = u(i).slice (jj._1, jj._2)
        new TensorD (u)
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the ii._1 to ii._2, jj._1 to jj._2, kk._1 to kk._2
     *  row-column-sheet slice of the tensor.
     *  @param ii  1st dimension (row) indices of the tensor (null => all)
     *  @param jj  2nd dimension (column) indices of the tensor (null => all)
     *  @param kk  3rd dimension (sheet) indices of the tensor
     */
    def apply (ii: (Int, Int), jj: (Int, Int), kk: (Int, Int)): TensorD =
        val (i1, i2) = if ii == null then (0, dim) else ii
        val (j1, j2) = if jj == null then (0, dim2) else jj
        val u = v.slice (i1, i2)
        for i <- u.indices do u(i) = u(i).slice (j1, j2)
        for i <- u.indices; j <- u(i).indices do u(i)(j) = u(i)(j).slice (kk._1, kk._2)
        new TensorD (u)
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the is row selections from the tensor.
     *  @param is  1st dimension (row) indices of the tensor
     */
    def apply (is: Array [Int]): TensorD = 
        val u = Array.ofDim [Double] (is.size, dim2, dim3)
        for i <- is.indices; j <- indices2; k <- indices3 do u(i)(j)(k) = v(is(i))(j)(k)
        new TensorD (u)
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the is, js row-column selections from the tensor.
     *  @param is  1st dimension (row) indices of the tensor (null => all)
     *  @param js  2nd dimension (column) indices of the tensor
     */
    def apply (is: Array [Int], js: Array [Int]): TensorD = 
        if is == null then
            val u = Array.ofDim [Double] (dim, js.size, dim3)
            for i <- indices; j <- js.indices; k <- indices3 do u(i)(j)(k) = v(i)(js(j))(k)
            new TensorD (u)
        else
            val u = Array.ofDim [Double] (is.size, js.size, dim3)
            for i <- is.indices; j <- js.indices; k <- indices3 do u(i)(j)(k) = v(is(i))(js(j))(k)
            new TensorD (u)
        end if
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the is, js, ks row-column-sheet selections from the tensor.
     *  @param is  1st dimension (row) indices of the tensor (null => all)
     *  @param js  2nd dimension (column) indices of the tensor (null => all)
     *  @param ks  3rd dimension (sheet) indices of the tensor
     */
    def apply (is: Array [Int], js: Array [Int], ks: Array [Int]): TensorD = 
        if is == null && js == null then
            val u = Array.ofDim [Double] (dim, dim2, ks.size)
            for i <- indices; j <- indices2; k <- ks.indices do u(i)(j)(k) = v(i)(j)(ks(k))
            new TensorD (u)
        else if is == null then
            val u = Array.ofDim [Double] (dim, js.size, ks.size)
            for i <- indices; j <- js.indices; k <- ks.indices do u(i)(j)(k) = v(i)(js(j))(ks(k))
            new TensorD (u)
        else if js == null then
            val u = Array.ofDim [Double] (is.size, dim2, ks.size)
            for i <- is.indices; j <- indices2; k <- ks.indices do u(i)(j)(k) = v(is(i))(j)(ks(k))
            new TensorD (u)
        else
            val u = Array.ofDim [Double] (is.size, js.size, ks.size)
            for i <- is.indices; j <- js.indices; k <- ks.indices do u(i)(j)(k) = v(is(i))(js(j))(ks(k))
            new TensorD (u)
        end if
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the complement of the is row selections from the tensor.
     *  @param is  1st dimension (row) indices of the tensor
     */
    def not (is: Array [Int]): TensorD = apply (Array.range (0, dim) diff is)

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the complement of the is row selections from the tensor.
     *  @param is  1st dimension (row) indices of the tensor
     *  @param js  2nd dimension (column) indices of the tensor
     */
    def not (is: Array [Int], js: Array [Int]): TensorD = apply (comple (is, dim), comple (js, dim2))

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Retrieve the complement of the is row selections from the tensor.
     *  @param is  1st dimension (row) indices of the tensor
     *  @param js  2nd dimension (column) indices of the tensor
     *  @param ks  3rd dimension (sheet) indices of the tensor
     */
    def not (is: Array [Int], js: Array [Int], ks: Array [Int]): TensorD =
        apply (comple (is, dim), comple (js, dim2), comple (ks, dim3))
    end not

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update a single SCALAR element of the tensor to the given value.
     *  Usage: z(i, j, k) = x
     *  @param i  1st dimension (row) index of the tensor
     *  @param j  2nd dimension (column) index of the tensor
     *  @param k  3rd dimension (sheet) index of the tensor
     *  @param x  the value for updating the tensor at the above position
     */
    def update (i: Int, j: Int, k: Int, x: Double): Unit = v(i)(j)(k) = x

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update a single VECTOR of the tensor to the given vector.
     *  Usage: z(i, j) = x
     *  @param i  1st dimension (row) index of the tensor
     *  @param j  2nd dimension (column) index of the tensor
     *  @param x  the vector for updating the tensor at the above position
     */
    def update (i: Int, j: Int, x: VectorD): Unit = v(i)(j) = x.toArray

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update a single VECTOR of the tensor to the given vector.
     *  Usage: z(i, ?, k) = x
     *  @param i    1st dimension (row) index of the tensor
     *  @param all  use the all columns indicator ?
     *  @param k    3rd dimension (sheet) index of the tensor
     *  @param x    the vector for updating the tensor at the above position
     */
    def update (i: Int, all: Char, k: Int, x: VectorD): Unit =
        for j <- indices2 do v(i)(j)(k) = x(j)
    end update

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update a single VECTOR of the tensor to the given vector.
     *  Usage: z(?, j, k) = x
     *  @param all  use the all rows indicator ?
     *  @param j    2nd dimension (column) index of the tensor
     *  @param k    3rd dimension (sheet) index of the tensor
     *  @param x    the vector for updating the tensor at the above position
     */
    def update (all: Char, j: Int, k: Int, x: VectorD): Unit =
        for i <- indices do v(i)(j)(k) = x(i)
    end update

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update a single MATRIX of the tensor (for ROW i) to the given matrix.
     *  Usage: z(i) = x
     *  @param i  1st dimension (row) index of the tensor
     *  @param x  the matrix for updating the tensor at the above position
     */
    def update (i: Int, x: MatrixD): Unit =
        for j <- indices2 do v(i)(j) = x(j).toArray
    end update

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update a single MATRIX of the tensor (for COLUMN j) to the given matrix.
     *  Usage: z(?, j) = x
     *  @param all  use the all rows indicator ?
     *  @param j    2nd dimension (column) index of the tensor
     *  @param x    the matrix for updating the tensor at the above position 
     */
    def update (all: Char, j: Int, x: MatrixD): Unit =
        for i <- indices; k <- indices3 do v(i)(j)(k) = x(i, k)
    end update

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Update a single MATRIX of the tensor (for SHEET k) to the given matrix.
     *  Usage: z(?, ?, k) = x
     *  @param all   use the all rows indicator ?
     *  @param all2  use the all columns indicator ?
     *  @param k     the 3rd dimension (sheet) index of the tensor
     *  @param x     the matrix for updating the tensor at the above position
     */
    def update (all: Char, all2: Char, k: Int, x: MatrixD): Unit =
        for i <- indices; j <- indices2 do v(i)(j)(k) = x(i, j)
    end update

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set all the tensor element values to x.
     *  @param x  the value to set all elements to
     */
    def set (x: Double): Unit = 
        for i <-indices; j <- indices2; k <- indices3 do v(i)(j)(k) = x
    end set

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this tensor and tensor b.
     *  @param b  the tensor to add (requires leDimensions)
     */
    def + (b: TensorD): TensorD =
        val c = new TensorD (dim, dim2, dim3)
        for i <- indices; j <- indices2; k <- indices3 do
            c.v(i)(j)(k) = v(i)(j)(k) + b.v(i)(j)(k)
        end for
        c
    end +

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Add this tensor and scalar s.
     *  @param s  the scalar to add
     */
    def + (s: Double): TensorD =
        val c = new TensorD (dim, dim2, dim3)
        for i <- indices; j <- indices2; k <- indices3 do
            c.v(i)(j)(k) = v(i)(j)(k) + s
        end for
        c
    end +

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this tensor subtract tensor b.
     *  @param b  the tensor to add (requires leDimensions)
     */
    def - (b: TensorD): TensorD =
        val c = new TensorD (dim, dim2, dim3)
        for i <- indices; j <- indices2; k <- indices3 do
            c.v(i)(j)(k) = v(i)(j)(k) - b.v(i)(j)(k)
        end for
        c
    end -

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** From this tensor subtract scalar s.
     *  @param s  the scalar to add
     */
    def - (s: Double): TensorD =
        val c = new TensorD (dim, dim2, dim3)
        for i <- indices; j <- indices2; k <- indices3 do
            c.v(i)(j)(k) = v(i)(j)(k) - s
        end for
        c
    end -

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply this tensor by scalar s.
     *  @param s  the scalar to multiply by
     */
    def * (s: Double): TensorD =
        val c = new TensorD (dim, dim2, dim3)
        for i <- indices; j <- indices2; k <- indices3 do
            c.v(i)(j)(k) = v(i)(j)(k) * s
        end for
        c
    end *

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply (multi-linear product) this tensor by three matrices b, c and d.
     *      this * (a, b, c)
     *  @see www.stat.uchicago.edu/~lekheng/work/icm1.pdf - equation 15.1
     *  @param b  the first matrix to multiply by (requires leDimensions)
     *  @param c  the second matrix to multiply by (requires leDimensions)
     *  @param d  the third matrix to multiply by (requires leDimensions)
     */
    def * (b: MatrixD, c: MatrixD, d: MatrixD): TensorD =
        val (m1, n1) = (b.dim, b.dim2)
        val (m2, n2) = (c.dim, c.dim2)
        val (m3, n3) = (d.dim, d.dim2)
        if n1 > dim2 || n2 > dim2 || n3 > dim3 then flaw ("*", "dimensions don't match")

        val e = new TensorD (m1, m2, m3)
        for i <- b.indices; j <- c.indices; k <- d.indices do
            var sum = 0.0
            for l1 <- b.indices2; l2 <- c.indices2; l3 <- d.indices2 do
                sum += b(i, l1) * c(j, l2) * d(k, l3) * v(l1)(l2)(l3)
            end for
            e.v(i)(j)(k) = sum
        end for
        e
    end *

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Multiply element-wise (Hadamard product) this tensor by tensor b.
     *  @param b  the tensor to add (requires leDimensions)
     */
    def *~ (b: TensorD): TensorD =
        val c = new TensorD (dim, dim2, dim3)
        for i <- indices; j <- indices2; k <- indices3 do
            c.v(i)(j)(k) = v(i)(j)(k) * b.v(i)(j)(k)
        end for
        c
    end *~ 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map each row of this tensor by applying function f to each row matrix and
     *  returning the collected result as a matrix.
     *  @param f  the matrix to vector function to apply
     */
    def map (f: FunctionM2V): MatrixD =
        MatrixD (for i <- indices yield f(apply(i)))
    end map

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map each row of this tensor by applying function f to each row matrix and
     *  returning the collected result as a tensor.
     *  @param f  the matrix to matrix function to apply
     */
    def mmap (f: FunctionM2M): TensorD =
        TensorD (for i <- indices yield f(apply(i)))
    end mmap

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Map each element of this tensor by applying function f to each element and
     *  returning the collected result as a tensor.
     *  @param f  the scalar to scalar function to apply
     */
    def map_ (f: FunctionS2S): TensorD =
        val x = new TensorD (dim, dim2, dim3)
        for i <- indices; j <- indices2; k <- indices3 do x.v(i)(j)(k) =  f(v(i)(j)(k))
        x
    end map_

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Flatten this tensor in row-major fashion, returning a matrix containing
     *  all the elements from the tensor.
     */
    def flatten: MatrixD =
        val a = Array.ofDim [Double] (dim * dim2, dim3)
        var k = 0
        for i <- indices do
            val v_i = v(i)
            var j = 0
            cfor (j < dim2, j += 1) { a(k) = v_i(j); k += 1 }
        end for
        new MatrixD (a.length, a(0).length, a)
    end flatten

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Check whether the dimensions of this tensor are less than or equal to
     *  le those of the other tensor b.
     *  @param b  the other matrix
     */
    def leDimensions (b: TensorD): Boolean =
        dim <= b.dim && dim2 <= b.dim2 && dim3 <= b.dim3
    end leDimensions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this tensor to a matrix where all the elements have integer values.
     */
    def toInt: TensorD =
        val x = new TensorD (dim, dim2, dim3)
        for i <- indices; j <- indices2; k <- indices3 do x.v(i)(j)(k) = round (v(i)(j)(k)).toDouble
        x
    end toInt

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this tensor to a string with a double line break after each sheet
     *  and a single line break after each row.
     */
    override def toString: String = 
        val sb = new StringBuilder ("\nTensorD (")
        if dim == 0 then return sb.append (")").mkString
        for k <- indices3 do
            for i <- indices do
                for j <- indices2 do sb.append (s"${v(i)(j)(k)}, ")
                sb.append ("\n" + TAB)
            end for
            sb.append ("\n" + TAB)
        end for
        sb.replace (sb.length-5, sb.length, ")").mkString
    end toString

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert this tensor to a string with a line break after each sheet.
     */
    def toString2: String = 
        val sb = new StringBuilder ("\nTensorD( ")
        if dim == 0 then return sb.append (")").mkString
        for i <- indices; j <- indices2 do
            sb.append (stringOf (v(i)(j)) + ", ")
            if j == dim2-1 then sb.replace (sb.length-1, sb.length, "\n\t")
        end for
        sb.replace (sb.length-3, sb.length, ")").mkString
    end toString2

end TensorD


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TensorD` companion object provides factory methods for the `TensorD` class.
 */
object TensorD:

//  private val flaw = flawf ("TensorD")                               // flaw function

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a tensor from the scaler argument list x.
     *  @param n1  the first dimension
     *  @param n2  the second dimension
     *  @param n3  the third dimension
     *  @param x   the list/vararg of scacollection.immutable.IndexedSeq [MatrixD]lars
     */
    def apply (n: (Int, Int, Int), x: Double*): TensorD =
        val t = new TensorD (n._1, n._2, n._3)
        var l = 0
        for k <- 0 until n._3; i <- 0 until n._1; j <- 0 until n._2 do
            t(i, j, k) = x(l)
            l += 1
        end for
        t
    end apply 

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a tensor from the vector argument list x.
     *  @param n   the first dimension
     *  @param vs  the list/vararg of vectors
     */
    def apply (n: Int, vs: VectorD*): TensorD =
        val t = new TensorD (n, vs.length, vs(0).dim)
        var l = 0
        for i <- t.indices; j <- t.indices2 do
            t(i, j) = vs(l)
            l += 1
        end for
        t
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a tensor from the vector argument list x.
     *  @param n   the first dimension
     *  @param vs  the indexed sequence of vectors
     */
    def apply (n: Int, vs: IndexedSeq [VectorD]): TensorD =
        val t = new TensorD (n, vs.length, vs(0).dim)
        var l = 0
        for i <- t.indices; j <- t.indices2 do
            t(i, j) = vs(l)
            l += 1
        end for
        t
    end apply

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Build a tensor from the vector argument list x.
     *  @param n   the first dimension
     *  @param vs  the indexed sequence of vectors
     */
    def apply (n: Int, vs: collection.immutable.IndexedSeq [VectorD]): TensorD =
        val t = new TensorD (n, vs.length, vs(0).dim)
        var l = 0
        for i <- t.indices; j <- t.indices2 do
            t(i, j) = vs(l)
            l += 1
        end for
        t
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a tensor from a variable argument list of matrices (row-wise).
     *  Use transpose to make it column-wise.
     *  @param vs  the vararg list of matrices
     */
    def apply (vs: MatrixD*): TensorD =
        val (m, n, p) = (vs.length, vs(0).dim, vs(0).dim2)
        val a = Array.ofDim [Array [Array [Double]]] (m)
        for i <- vs.indices do a(i) = vs(i).v
        new TensorD (m, n, p, a)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a tensor from an mutable `IndexedSeq` of matrices (row-wise).
     *  Use transpose to make it column-wise.
     *  @param vs  the indexed sequence of matrices
     */
    def apply (vs: IndexedSeq [MatrixD]): TensorD =
        val (m, n, p) = (vs.length, vs(0).dim, vs(0).dim2)
        val a = Array.ofDim [Array [Array [Double]]] (m)
        for i <- vs.indices do a(i) = vs(i).v
        new TensorD (m, n, p, a)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a tensor from an immutable `IndexedSeq` of matrices (row-wise),
     *  as produce by for yield.  Use transpose to make it column-wise.
     *  @param vs  the indexed sequence of matrices
     */
    def apply (vs: collection.immutable.IndexedSeq [MatrixD]): TensorD =
        val (m, n, p) = (vs.length, vs(0).dim, vs(0).dim2)
        val a = Array.ofDim [Array [Array [Double]]] (m)
        for i <- vs.indices do a(i) = vs(i).v
        new TensorD (m, n, p, a)
    end apply

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Create a tensor of dimensions dim by dim2 by dim3 where all elements equal
     *  to the given value.
     *  @param dim    the row dimension
     *  @param dim2   the column dimension
     *  @param dim2   the sheet dimension
     *  @param value  the given value to assign to all elements
     */
    def fill (dim: Int, dim2: Int, dim3: Int, value: Double): TensorD =
        val a = Array.fill (dim, dim2, dim3)(value)
        new TensorD (dim, dim2, dim3, a)
    end fill

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the cross-correlation tensor for the given data matrix for up to
     *  maxLags.
     *  @param x        the given data matrix (row are instances, columns are variables)
     *  @param maxLags  the maximum number of lags to consider
     *
    def crossCorr (x: MatrixD, maxLags: Int = 10): TensorD =
        val n = x.dim2
        if 2 * maxLags >= x.dim then flaw ("crossCorr", "not enough data for maxLags = $maxLags") 
        val ccorr = new TensorD (maxLags+1, n, n)
        for l <- 0 to maxLags do ccorr(l) = x.laggedCorr (l)
        ccorr
    end crossCorr
     */

end TensorD


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `tensorDTest` main function is used to test the `TensorD` class.
 *  > runMain scalation.mathstat.tensorDTest
 */
@main def tensorDTest (): Unit =

    val s = 2.0
    val a = new TensorD (2, 3, 2)
    val b = new TensorD (2, 3, 2)
    //                                            row column sheet
    val c = TensorD ((2, 3, 2), 1,  2,  3,       // 0   0-2     0
                                4,  5,  6,       // 1   0-2     0

                                7,  8,  9,       // 0   0-2     1
                               10, 11, 12)       // 1   0-2     1

    for i <- 0 until 2; j <- 0 until 3; k <- 0 until 2 do
        val sum = i + j + k
        a(i, j, k) = sum
        b(i, j, k) = sum
    end for

    println ("s          = " + s)
    println ("a          = " + a)
    println ("b          = " + b)
    println ("c          = " + c)
    println ("c(0)       = " + c(0))
    println ("c(0, 0)    = " + c(0, 0))
    println ("c(0, 0, 0) = " + c(0, 0, 0))

    banner ("Test operators")
    println ("a + b   = " + (a + b))
    println ("a + s   = " + (a + b))
    println ("a - b   = " + (a - b))
    println ("a - s   = " + (a - s))
    println ("c * s   = " + c * s)
    println ("a *~ c  = " + a *~ c)

    val x = MatrixD ((2, 2), 1, 2,
                             3, 4)
    val y = MatrixD ((2, 3), 1, 2, 3,
                             4, 5, 6)
    val z = MatrixD ((2, 2), 5, 6,
                             7, 8)

    println ("c * (x, y, z) = " + c * (x, y, z))

    banner ("Test slice")
    println ("c = " + c)
    println ("slice row 0:1 = " + c((0, 1)))

    println ("slice row col: 0:1, 0:2 = " + c((0, 1), (0, 2)))
    println ("slice col:    null, 0:2 = " + c(null,   (0, 2)))

    println ("slice row col sheet: 0:1, 0:2,  0:1 = " + c((0, 1), (0, 2), (0, 1)))
    println ("slice sheet:        null, null, 0:1 = " + c(null,   null,   (0, 1)))
    println ("slice row sheet:     0:1, null, 0:1 = " + c((0, 1), null,   (0, 1)))
    println ("slice col sheet     null, 0:2,  0:1 = " + c(null,   (0, 2), (0, 1)))

    banner ("Test select")
    println ("c = " + c)
    println ("select row 0 = " + c(Array [Int] (0)))

    println ("select row col: 0, 0,2 = " + c(Array [Int] (0), Array [Int] (0, 2)))
    println ("select col:  null, 0,2 = " + c(null,     Array [Int] (0, 2)))

    println ("select row col sheet: 0,  0,2, 1 = " + c(Array [Int] (0), Array [Int] (0, 2), Array [Int] (1)))
    println ("select sheet:      null, null, 1 = " + c(null,     null,        Array [Int] (1)))
    println ("select row sheet:     0, null, 1 = " + c(Array [Int] (0), null,        Array [Int] (1)))
    println ("select col sheet   null,  0,2, 1 = " + c(null,     Array [Int] (0, 2), Array [Int] (1)))

    banner ("Test not")
    println ("c = " + c)
    println ("not row 0 = " + c.not(Array [Int] (0)))
    println ("not row col: 0, 0,2 = " + c.not(Array [Int] (0), Array [Int] (0, 2)))
    println ("not row col sheet: 0, 0,2, 1 = " + c.not(Array [Int] (0), Array [Int] (0, 2), Array [Int] (1)))

end tensorDTest


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TensorDTest2` main function is used to test the `TensorD` class.
 *  It tests pulling matrices and vectors from the tensor.
 *  > runMain scalation.mathstat.tensorDTest2
 */
@main def tensorDTest2 (): Unit =

    // 4 rows, 3 columns, 2 sheets - x_ijk
    //                                              row columns sheet
    val x = TensorD ((4, 3, 2), 1,  2,  3,       //  0   0,1,2    0
                                4,  5,  6,       //  1   0,1,2    0
                                7,  8,  9,       //  2   0,1,2    0
                               10, 11, 12,       //  3   0,1,1    0

                               13, 14, 15,       //  0   0,1,2    1
                               16, 17, 18,       //  1   0,1,2    1
                               19, 20, 21,       //  2   0,1,2    1
                               22, 23, 24)       //  3   0,1,2    1

     banner ("Tensor with dimensions (rows, columns, sheets) = (4, 3, 2)")
     println ("x          = " + x)

     // SCALARS
     banner ("Scalar element at index position (i, j, k) = (0, 0, 0)")
     println ("x(0, 0, 0) = " + x(0, 0, 0))                      // x_000  - element i=0, j=0, k=0

     // VECTORS
     banner ("Vector at index position (i, j) = (0, 0)")
     println ("x(0, 0)    = " + x(0, 0))                         // x_00:  - vector  i=0, j=0, k=all
     banner ("Vector at index position (i, ?, k) = (0, all, 0)")
     println ("x(0, ?, 0) = " + x(0, ?, 0))                      // x_0:0  - vector  i=0, j=all, k=0
     banner ("Vector at index position (?, j, k) = (all, 0, 0)")
     println ("x(?, 0, 0) = " + x(?, 0, 0))                      // x_:00  - vector  i=all, j=0, k=0

     // MATRICES
     banner ("Matrix from tensor with row i fixed at 0")
     println ("x(0)       = " + x(0))                            // x_0::  - matrix with row i fixed
     banner ("Matrix from tensor with column j fixed at 0")
     println ("x(?, 0)    = " + x(?, 0))                         // x_:0:  - matrix with column j fixed
     banner ("Matrix from tensor with sheet k fixed at 0")
     println ("x(?, ?, 0) = " + x(?, ?, 0))                      // x_::0  - matrix with sheet k fixed

end tensorDTest2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `TensorDTest3` main function is used to test the `TensorD` class.
 *  It tests the use of tensors and matrices for convolutional operation needed in
 *  Convolutional Nets.
 *  > runMain scalation.mathstat.tensorDTest3
 */
@main def tensorDTest3 (): Unit =

    val a = new TensorD (2, 9, 9)
    for i <- a.indices; j <- a.indices2; k <- a.indices3 do a(i, j, k) = i + j + k
    println (s"a = $a")

    val image0 = a(0)
    val image1 = a(1)
    println (s"image0 = $image0")
    println (s"image1 = $image1")

    val kernel = MatrixD ((3, 3), 1, 2, 1,
                                  2, 3, 2,
                                  1, 2, 1)
    println (s"kernel = $kernel")

    val sp = new MatrixD (image0.dim - kernel.dim2 + 1, image0.dim2 - kernel.dim2 + 1)
//  for i <- sp.indices; j <- sp.indices2 do sp(i, j) = kernel **+ (image0, i, j)           // FIX **+ only in MatrixD.scala.sav
    println (s"sp = $sp")

end tensorDTest3

