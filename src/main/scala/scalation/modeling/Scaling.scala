
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Sun Jun  9 16:42:16 EDT 2019
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Model Framework: Scaling of Data Values
 */

package scalation
package modeling

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Scaling` trait supports rescaling of data values.  When the scale flag is on/true,
 *  the companion object factory apply/rescale functions should rescale or normalize
 *  the data appropriately to the particular modeling technique (or even to the level
 *  of the activation function used).
 *  @see `ActivationFun`.
 *  In ScalaTion, model constructors do not rescale, but apply/rescale functions
 *  that call model constructors need to provide this option.
 */
trait Scaling:

    /** The 'scale' flag indicated whether the data is to be rescaled/normalized
     */
    protected var scale = true                       // by default rescaling is on

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the scale flag to the given value.
     *  @param scale_  the new value for the scale flag
     */
    def setScale (scale_ : Boolean): Unit = scale = scale_

end Scaling

