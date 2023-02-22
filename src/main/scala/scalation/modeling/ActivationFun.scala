
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Dec 28 12:00:07 EST 2014
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model Support: Activation Functions
 */

package scalation
package modeling

import scala.math.{exp, log, max, min, tanh}

import scalation.mathstat._

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `AFF` class holds an Activation Function Family (AFF).
 *  @param name    the name of the activation function
 *  @param f       the activation function itself (scalar version)
 *  @param f_      the vector version of the activation function
 *  @param d       the vector version of the activation function derivative
 *  @param bounds  the (lower, upper) bounds on the range of the activation function
 */
case class AFF (name: String, f: FunctionS2S, f_ : FunctionV2V, d: FunctionV2V,
                bounds: (Double, Double) = null):

    val fM = matrixize (f_)             // the matrix version of the activation function
    val dM = matrixize (d)              // the matrix version of the activation function derivative

end AFF


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `ActivationFun` object contains common Activation functions and provides
 *  both scalar and vector versions.
 *  @see en.wikipedia.org/wiki/Activation_function
 *  Convention: fun   activation function (e.g., sigmoid)
 *              fun_  vector version of activation function (e.g., sigmoidV_)
 *              funD  vector version of dervivative (e.g., sigmoidD)
 *----------------------------------------------------------------------------------
 * Supports: id, reLU, lreLU, eLU, tanh, sigmoid, gaussian, softmax
 * Related functions: logistic, logit
 */
object ActivationFun:

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// id: Identity functions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the Identity id function at scalar t.
     *  @param t  the id function argument
     */
    def id (t: Double): Double    = t
    def id_ (t: VectorD): VectorD = t

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative vector for id function at vector yp where
     *  yp is pre-computed by yp = id (t).
     *  @param yp  the derivative function vector argument
     */
    def idD (yp: VectorD): VectorD = VectorD.one (yp.dim)

    val f_id = AFF ("id", id, id_, idD)                                           // id family

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// reLU: Rectified Linear Unit functions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the Rectified Linear Unit reLU function at scalar t.
     *  @param t  the reLU function argument
     */
    def reLU (t: Double): Double = max (0.0, t)
    val reLU_ : FunctionV2V = vectorize (reLU _)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative vector for reLU function at vector yp where
     *  yp is pre-computed by yp = reLU_ (t).
     *  @param yp  the derivative function vector argument
     */
    def reLUD (yp: VectorD): VectorD = yp.map (y => if y >= 0.0 then 1.0 else 0.0)

    val f_reLU = AFF ("reLU", reLU, reLU_, reLUD)                                 // reLU family

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// lreLU: Leaky Rectified Linear Unit functions

//  private var a = 0.01             // the lreLU alpha parameter (0, 1] indicating how leaky the function is
    private var a = 0.3              // the lreLU alpha parameter (0, 1] default values used in Keras
                                     // @see keras.io/layers/advanced-activations

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the lreLU a (alpha) parameter for the Leaky Rectified Linear Unit functions.
     *  @param a  the rleLU alpha parameter (0, 1] indicating how leaky the function is
     */
    def setA (a_ : Double): Unit = min (a_, 1.0)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the Leaky Rectified Linear Unit lreLU function at scalar t.
     *  @param t  the lreLU function argument
     */
    def lreLU (t: Double): Double = max (a * t, t)
    val lreLU_ : FunctionV2V = vectorize (lreLU _)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative vector for lreLU function at vector yp where
     *  yp is pre-computed by yp = lreLU_ (t).
     *  @param yp  the derivative function vector argument
     */
    def lreLUD (yp: VectorD): VectorD = yp.map (y => if y >= 0.0 then 1.0 else a)

    val f_lreLU = AFF ("lreLU", lreLU, lreLU_, lreLUD)                            // lreLU family

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// eLU: Exponential Linear Unit functions
// @see arxiv.org/pdf/1511.07289.pdf

    private var a2 = 1.0             // the eLU alpha parameter (0, infinity) indicating how leaky the function is

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the eLU a2 (alpha) parameter for the Exponential Linear Unit functions.
     *  @param a_  the eLU alpha parameter (0, infinity) indicating how leaky the function is
     */
    def setA2 (a_ : Double): Unit = a2 = a_ 

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the Exponential Linear Unit eLU function at scalar t.
     *  @param t  the eLU function argument
     */
    def eLU (t: Double): Double = if t > 0.0 then t else a2 * (exp (t) - 1)
    val eLU_ : FunctionV2V = vectorize (eLU _)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative vector for eLU function at vector yp where
     *  yp is pre-computed by yp = eLU_ (t).
     *  @param yp  the derivative function vector argument
     */
    def eLUD (yp: VectorD): VectorD = yp.map (y => if y > 0.0 then 1.0 else y + a2)
 
    val f_eLU = AFF ("eLU", eLU, eLU_, eLUD)                                      // eLU family

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// tanh: Hyperbolic Tangent functions
  
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /*  Compute the value of the tanh function at scalar t.
     *  @param t  the tanh function argument
     */
    //  @see scala.math.tanh

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the vector of values of the tanh function applied to vector t.
     *  @param t  the tanh function vector argument
     */
    def tanh_ (t: VectorD): VectorD = t.map (tanh (_))

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative vector for tanh function at vector yp where
     *  yp is pre-computed by yp = tanh_ (t).
     *  @param yp  the derivative function vector argument
     */
    def tanhD (yp: VectorD): VectorD = VectorD.one (yp.dim) - yp~^2

    val f_tanh = AFF ("tanh", tanh, tanh_, tanhD, (-1, 1))                        // tanh family

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// sigmoid: Sigmoid functions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the Sigmoid function at t.  This is a special case of
     *  the logistic function, where a = 0 and b = 1.  It is also referred to as
     *  the standard logistic function.  It is also the inverse of the logit function.
     *  @param t  the sigmoid function argument
     */
    def sigmoid (t: Double): Double = 1.0 / (1.0 + exp (-t))
    val sigmoid_ : FunctionV2V = vectorize (sigmoid _)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative vector for sigmoid function at vector yp where
     *  yp is pre-computed by yp = sigmoid_ (t).
     *  @param yp  the derivative function vector argument
     */
    def sigmoidD (yp: VectorD): VectorD = yp * (VectorD.one (yp.dim) - yp)

    val f_sigmoid = AFF ("sigmoid", sigmoid, sigmoid_, sigmoidD, (0, 1))          // sigmoid family

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// Common Activation Function Families

    val f_aff = Array (f_id, f_reLU, f_lreLU, f_eLU, f_tanh, f_sigmoid)

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// gaussian: Gaussian functions

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the Gaussian function at scalar t.
     *  @param t  the Gaussian function argument
     */
    def gaussian (t: Double): Double = exp (-t * t)
    val gaussian_ : FunctionV2V = vectorize (gaussian _)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative vector for Gaussian function at vector yp where
     *  yp is pre-computed by yp = gaussian_ (t).
     *  @param yp  the derivative function vector argument
     *  @param t   the domain value for the function
     */
    def gaussianD (yp: VectorD, t: VectorD): VectorD = t * yp * -2.0              // non-standard signature

//  val f_gaussain = AFF ("gaussian", gaussian, gaussian_, gaussianD)             // gaussian family

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// softmax: Softmax functions - FIX

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the vector of values of the Softmax function applied to vector t.
     *  @see https://en.wikipedia.org/wiki/Softmax_function
     *  Note, scalar function version softmax is not needed.
     *  @param t  the softmax function vector argument
     */
    def softmax_ (t: VectorD): VectorD = 
        val ts = t - t.max                                  // shift for numeric stability
        val et = ts.map (exp (_))
        et / et.sum
    end softmax_

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative vector for the Softmax function at vector yp where
     *  yp is pre-computed by yp = softmax_ (t).
     *  @param yp  the derivative function vector argument
     */
    def softmaxD (yp: VectorD): VectorD =
        VectorD (for i <- yp.indices yield yp(i) * (1.0 - yp(i)))
    end softmaxD

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the derivative matrix (Jacobian) for Softmax function at vector yp where
     *  yp is pre-computed by yp = softmax_ (t).
     *  @see https://eli.thegreenplace.net/2016/the-softmax-function-and-its-derivative/
     *  @param yp  the derivative function vector argument
     */
    def softmaxDM (yp: VectorD): MatrixD = 
        val z = new MatrixD (yp.dim, yp.dim)
        for i <- yp.indices; j <- yp.indices do
            z(i, j) = if i == j then yp(i) * (1.0 - yp(j))
                      else          -yp(i) * yp(j)
        z
    end softmaxDM

    val f_softmax = AFF ("softmax", null, softmax_, softmaxD)                     // softmax family

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// logistic: Logistic functions - related function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the value of the Logistic function at scalar t.
     *  With the default settings, it is identical to sigmoid.
     *  Note, it is not typically used as an activation function
     *  @see www.cs.xu.edu/math/math120/01f/logistic.pdf
     *  @param t  the logistic function argument
     *  @param a  the shift parameter (1 => mid at 0, <1 => mid shift left, >= mid shift right
     *  @param b  the spread parameter (1 => sigmoid rate, <1 => slower than, >1 => faster than)
     *            althtough typically positive, a negative b will cause the function to decrease
     *  @param c  the scale parameter (range is 0 to c)
     */
    def logistic (t: Double, a: Double = 1.0, b: Double = 1.0, c: Double = 1.0): Double =
        c / (1.0 + a * exp (-b*t))
    end logistic

    def logistic_ (t: VectorD, a: Double = 1.0, b: Double = 1.0, c: Double = 1.0): VectorD =
        t.map (t => c / (1.0 + a * exp (-b*t)))
    end logistic_

//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// logit: Logit functions - related function

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Compute the log of the odds (Logit) of an event occurring (e.g., success, 1).
     *  The inverse of the logit function is the standard logistic function
     *  (sigmoid function).
     *  Note, it is not typically used as an activation function
     *  @param p  the probability, a number between 0 and 1.
     */
    def logit (p: Double): Double = log (p / (1.0 - p))

    val logit_ : FunctionV2V = vectorize (logit _)                 // vector version

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Rescale the input/data matrix x to the bounds of the "first" activation
     *  function f; otherwise normalize.  Return the rescaled matrix.
     *  @param x  the input/data matrix
     *  @param f  the activation function family (first)
     */
    def rescaleX (x: MatrixD, f: AFF): MatrixD =
        if f.bounds != null then                                // scale to bounds of f
            val (min_x, max_x) = (x.min, x.max)
            scale ((min_x, max_x), f.bounds) (x)
        else                                                    // normalize: Normal (0, 1)
            val (mu_x, sig_x) = (x.mean, x.stdev)
            normalize ((mu_x, sig_x)) (x)
        end if
    end rescaleX

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Rescale the output/response vector y to the bounds of the "last" activation
     *  function f; otherwise normalize.  Return the rescaled vector and the
     *  rescaling inverse function.
     *  @param y  the output/response vector
     *  @param f  the activation function family (last)
     */
    def rescaleY (y: VectorD, f: AFF): (VectorD, FunctionV2V) =
        if f.bounds != null then                                // scale to bounds of f
            val (min_y, max_y) = (y.min, y.max)
            (scaleV ((min_y, max_y), f.bounds) (y),
             unscaleV ((min_y, max_y), f.bounds) _)             // rescaling inverse
        else                                                    // normalize: Normal (0, 1)
            val (mu_y, sig_y) = (y.mean, y.stdev)
            (normalizeV ((mu_y, sig_y)) (y),
             denormalizeV ((mu_y, sig_y)) _)                    // rescaling inverse
        end if
    end rescaleY

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Rescale the output/response matrix y to the bounds of the "last" activation
     *  function f; otherwise normalize.  Return the rescaled matrix and the
     *  rescaling inverse function.
     *  @param y  the output/response matrix
     *  @param f  the activation function family (last layer)
     */
    def rescaleY (y: MatrixD, f: AFF): (MatrixD, FunctionM2M) =
        if f.bounds != null then                                // scale to bounds of f
            val (min_y, max_y) = (y.min, y.max)
            (scale ((min_y, max_y), f.bounds) (y),
             unscale ((min_y, max_y), f.bounds) _)              // rescaling inverse
        else                                                    // normalize: Normal (0, 1)
            val (mu_y, sig_y) = (y.mean, y.stdev)
            (normalize ((mu_y, sig_y)) (y),
             denormalize ((mu_y, sig_y)) _)                     // rescaling inverse
        end if
    end rescaleY

end ActivationFun


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `activationFunTest` is used to test the `ActivationFun` object.
 *  > runMain scalation.modeling.activationFunTest
 */
@main def activationFunTest (): Unit =

    import ActivationFun._

    val t = VectorD.range (-30, 30) / 6.0
    val p = VectorD.range (1, 59) / 60.0

    // Test the vector version of activation functions
    val ident  = id_ (t);       new Plot (t, ident,  null, "t vs. ident") 
    val reluf  = reLU_ (t);     new Plot (t, reluf,  null, "t vs. reluf") 
    val lreluf = lreLU_ (t);    new Plot (t, lreluf, null, "t vs. lreluf") 
    val eluf   = eLU_ (t);      new Plot (t, eluf,   null, "t vs. eluf") 
    val tanhh  = tanh_ (t);     new Plot (t, tanhh,  null, "t vs. tanhh") 
    val sigmo  = sigmoid_ (t);  new Plot (t, sigmo,  null, "t vs. sigmo")
    val gauss  = gaussian_ (t); new Plot (t, gauss,  null, "t vs. gauss")
    val softmo = softmax_ (t);  new Plot (t, softmo, null, "t vs. softmo")

    // Test the vector version of related functions
    val logit  = logit_ (p);    new Plot (p, logit,  null, "p vs. logit")
    val logist = logistic_ (t); new Plot (t, logist, null, "t vs. logist")

    // Test the vector version of activation function derivatives
    val identD = idD (ident);          new Plot (t, identD, null, "t vs. identD") 
    val relufD = reLUD (reluf);        new Plot (t, relufD, null, "t vs. relufD") 
    val lrlufD = lreLUD (lreluf);      new Plot (t, lrlufD, null, "t vs. lrlufD") 
    val elufD  = eLUD (eluf);          new Plot (t, elufD, null,  "t vs. elufD") 
    val tanhhD = tanhD (tanhh);        new Plot (t, tanhhD, null, "t vs. tanhhD") 
    val sigmoD = sigmoidD (sigmo);     new Plot (t, sigmoD, null, "t vs. sigmoD")
    val gaussD = gaussianD (gauss, t); new Plot (t, gaussD, null, "t vs. gaussD")
//  val softmD = softmaxD (softmo);    new Plot (t, softmD, null, "t vs. softmD")

end activationFunTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `activationFunTest2` is used to test the `ActivationFun` object.
 *  @see en.wikipedia.org/wiki/Softmax_function
 *  > runMain scalation.modeling.activationFunTest2
 */
@main def activationFunTest2 (): Unit =

    import ActivationFun.softmax_

    val t = VectorD (1.0, 2.0, 3.0, 4.0, 1.0, 2.0, 3.0)
    println (s"softmax_ ($t) = \n ${softmax_ (t)}")

end activationFunTest2

