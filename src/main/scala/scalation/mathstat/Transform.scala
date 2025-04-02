
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Yousef Fekri Dabanloo
 *  @version 2.0
 *  @date    Thu Mar 13 14:06:11 EDT 2025
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Support for Transformation Functions with their Inverse
 *
 *  https://www.infoq.com/news/2023/10/foreign-function-and-memory-api/
 */

package scalation
package mathstat

import scala.math._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Thee `⚬` extension method performs function composition f ⚬ g.
 *  @see www.scala-lang.org/api/current/scala/Function1.html
 *  @tparam A  the type to which function `g` can be applied
 *  @tparam B  the type to which function `f` can be applied
 *  @tparam R  the return type for f ⚬ g
 */
extension [A, B, R](f: B => R)

    def ⚬ (g: A => B): A => R = f compose g


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Transform` trait supports the use of transformation functions, such that it
 *  is easy to take the inverse transform.  When a transformation uses arguments,
 *  they are remembered for use by the inverse transformation.
 *  @param x  the vector or matrix being transformed
 */
trait Transform (x: VectorD | MatrixD = null):

    protected var lu: VectorD = VectorD (1, 2)                           // optional default range/bounds [l .. u]
    protected var b: MatrixD = null                                      // optional argument matrix

    if x != null then
        x match
            case xM: MatrixD => setB (xM)
            case xV: VectorD => setB (MatrixD (xV).transpose)

    def setLU (_lu: VectorD): Unit = lu = _lu                            // set the default bounds
    def setB (x: MatrixD): Unit = b = x                                  // set the argument matrix 
    def f  (x: MatrixD): MatrixD                                         // transformation function
    def fi (y: MatrixD): MatrixD                                         // inverse transformation function
    val f:  FunctionV2V = (x: VectorD) => f(MatrixD(x).transpose)(?, 0)
    val fi: FunctionV2V = (y: VectorD) => fi(MatrixD(y).transpose)(?, 0)

end Transform


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `zForm` class applies the z-transformation (subtract mean and divide by standard deviation).
 */
class zForm (x: VectorD | MatrixD) extends Transform (x):
    override def setB (x: MatrixD): Unit = b = x.mu_sig
    def f (x: MatrixD): MatrixD  = (x - b(0)) / b(1)
    def fi (y: MatrixD): MatrixD = (y *~ b(1)) + b(0)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `rangeForm` class transforms values to the default range/bounds lu.
 */
class rangeForm (x: VectorD | MatrixD) extends Transform (x):
    override def setB (x: MatrixD): Unit = b = x.min_max
    def f (x: MatrixD): MatrixD  = (x - b(0)) * (lu(1) - lu(0)) / (b(1) - b(0))  + lu(0)
    def fi (y: MatrixD): MatrixD = (y - lu(0)) *~ (b(1) - b(0)) /(lu(1) - lu(0)) + b(0)

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `logForm` object applies the log-transformation.
 */
object logForm extends Transform ():
    def f (x: MatrixD): MatrixD  = x.log
    def fi (y: MatrixD): MatrixD = y.exp

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `log1pForm` object applies the log1p-transformation (log (z+1)).
 */
object log1pForm extends Transform ():
    def f (x: MatrixD): MatrixD  = x.log1p
    def fi (y: MatrixD): MatrixD = y.expm1

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `cosForm` class applies the cosine-transformation.
 */
class cosForm (x: VectorD) extends Transform (x):
    def f (x: MatrixD): MatrixD  = x.map_ (z => cos (b(0, 0) * Piby2 * z))
    def fi (y: MatrixD): MatrixD = y.map_ (z => acos (z) / (b(0, 0) * Piby2))

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `sinForm` class applies the sine-transformation.
 */
class sinForm (x: VectorD) extends Transform (x):
    def f (x: MatrixD): MatrixD  = x.map_ (z => sin (b(0, 0) * Piby2 * z))
    def fi (y: MatrixD): MatrixD = y.map_ (z => asin (z) / (b(0, 0) * Piby2))

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `powForm` class applies the power-transformation x^p for power p > 1.
 */
class powForm (x: VectorD) extends Transform (x):
    def f (x: MatrixD): MatrixD  = x ~^ b(0, 0)
    def fi (y: MatrixD): MatrixD = y ~^ (1/b(0, 0))


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `transformTest` tests the `Transform` class at the vector level.
 *  > runMain scalation.mathstat.transformTest
 */
@main def transformTest (): Unit =

    val x = VectorD (3, 5, 6)
    println (s"x = $x")

    banner ("zForm Transformation")
    val zForm1 = zForm (x)                                                    // set the argument vector
    var y = zForm1.f (x)
    var z = zForm1.fi (y)
    println (s"y = $y, z = $z")

    banner ("rangeForm Transformation")
    val rangeForm1 = rangeForm (x)                                            // set the argument vector
    y = rangeForm1.f (x)
    z = rangeForm1.fi (y)
    println (s"y = $y, z = $z")

    banner ("logForm Transformation")
    y = logForm.f (x)
    z = logForm.fi (y)
    println (s"y = $y, z = $z")

    banner ("log1pForm Transformation")
    y = log1pForm.f (x)
    z = log1pForm.fi (y)
    println (s"y = $y, z = $z")

    banner ("cosForm Transformation")
    val cosForm1 = cosForm (VectorD (0.25))                                   // set the argument vector
    y = cosForm1.f (x)
    z = cosForm1.fi (y)
    println (s"y = $y, z = $z")

    banner ("sinForm Transformation")
    val sinForm1 = sinForm (VectorD (0.25))                                   // set the argument vector
    y = sinForm1.f (x)
    z = sinForm1.fi (y)
    println (s"y = $y, z = $z")

    banner ("powForm Transformation")
    val powForm1 = powForm (VectorD (1.5))                                    // set the argument vector
    y = powForm1.f (x)
    z = powForm1.fi (y)
    println (s"y = $y, z = $z")

end transformTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `transformTest2` tests the `Transform` class at the matrix level.
 *  > runMain scalation.mathstat.transformTest2
 */
@main def transformTest2 (): Unit =

    val x = MatrixD ((3, 2), 3, 1,
        5, 2,
        6, 3)
    println (s"x = $x")

    banner ("zForm Transformation")
    val zForm1 = zForm (x)                                                    // set the argument vector
    var y = zForm1.f (x)
    var z = zForm1.fi (y)
    println (s"y = $y, z = $z")

    banner ("rangeForm Transformation")
    val rangeForm1 = rangeForm (x)                                            // set the argument vector
    y = rangeForm1.f (x)
    z = rangeForm1.fi (y)
    println (s"y = $y, z = $z")

    banner ("logForm Transformation")
    y = logForm.f (x)
    z = logForm.fi (y)
    println (s"y = $y, z = $z")

    banner ("log1pForm Transformation")
    y = log1pForm.f (x)
    z = log1pForm.fi (y)
    println (s"y = $y, z = $z")

    banner ("cosForm Transformation")
    val cosForm1 = cosForm (VectorD (0.25))                                   // set the argument vector
    y = cosForm1.f (x)
    z = cosForm1.fi (y)
    println (s"y = $y, z = $z")

    banner ("sinForm Transformation")
    val sinForm1 = sinForm (VectorD (0.25))                                   // set the argument vector
    y = sinForm1.f (x)
    z = sinForm1.fi (y)
    println (s"y = $y, z = $z")

    banner ("powForm Transformation")
    val powForm1 = powForm (VectorD (1.5))                                    // set the argument vector
    y = powForm1.f (x)
    z = powForm1.fi (y)
    println (s"y = $y, z = $z")

end transformTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `transformTest3` tests the `Transform` class at the matrix level.
 *  > runMain scalation.mathstat.transformTest3
 */
@main def transformTest3 (): Unit =

    val x = VectorD (3, 5, 6, 2, 1, 3, 2, 4, 6, 87, 1000)
    println (s"x = $x")

    banner ("zForm Transformation")
    val zForm1 = zForm (x)                                                    // set the argument vector
    var y = zForm1.f (x)
    var z = zForm1.fi (y)
    println (s"y = $y, \nz = $z")

    banner ("powForm Transformation")
    val powForm1 = powForm (VectorD (1.5))                                    // set the argument vector
    y = powForm1.f (x)
    z = powForm1.fi (y)
    println (s"y = $y, \nz = $z")

    val fsc = (zForm1.f(_: VectorD)) ⚬ (powForm1.f(_: VectorD)) ⚬ (zForm1.fi(_: VectorD))
    val ysc = fsc(y)
    println(s"ysc = ${ysc}")

    val ysc2 = fsc(y(0 until 3))
    println(s"ysc = ${ysc2}")

end transformTest3

