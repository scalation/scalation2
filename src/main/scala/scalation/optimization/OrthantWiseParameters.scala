
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  André Filipe Caldas Laranjeira
 *  @version 2.0
 *  @note    Tue Oct 31 15:27:39 EDT 2023
 *  @see     LICENSE (MIT style license file).
 *------------------------------------------------------------------------------
 *  Group of parameters from the Orthant-Wise method for optimizing the
 *  objective function value in the optimization process of the Limited memory
 *  Broyden–Fletcher–Goldfarb–Shanno (BFGS) for Bound constrained optimization
 *  (L-BFGS-B) algorithm.
 */

// Package definition.
package scalation.optimization

// General imports.
import scala.math.abs

// Project imports.
import scalation.mathstat.VectorD

// Case class.
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `OrthantWiseParameters` class is used to group together all parameters
 *  that are control the Orthant-Wise method for minimizing the objective
 *  function value during the L-BFGS optimization process.
 *  
 *  @param c        Coefficient for the L1 norm of variables. Must be set to a
 *                  positive value to activate the Orthant-Wise Limited-memory
 *                  Quasi-Newton (OWL-QN) method, which minimizes the objective
 *                  function F(x) combined with the L1 norm |x| of the
 *                  variables: F(x) + C|x|. This parameter is the coefficient
 *                  ''C'' for the |x| term. As the L1 norm |x| is not
 *                  differentiable at zero, the code modifies function and
 *                  gradient evaluations from a client program suitably. Thus, a
 *                  client program only has to return the function value F(x)
 *                  and gradients G(x) as usual.
 *  @param start    Start index for computing L1 norm of the variables. This
 *                  parameter, which we shall henceforth call ''b'', must be
 *                  selected such that 0 &le; ''b'' &lt; N. It specifies the
 *                  index number from which the L1 norm of the variables `x`
 *                  will be computed:
 *                  |x| = |x,,''b'',,| + |x,,''b''+1,,| + ... + |x,,N,,|.
 *                  In other words, variables x,,1,,, ..., x,,''b''-1,, are not
 *                  used for computing the L1 norm. Setting ''b'' to a non-zero
 *                  value can protect variables x,,1,,, ..., x,,''b''-1,, from
 *                  being regularized (e.g.: if they represent a bias term of
 *                  logistic regression). The default value is 0.
 *  @param end      End index [[Option]] for computing L1 norm of the variables.
 *                  This parameter, which we shall henceforth call ''e'', must
 *                  be selected such that 0 &lt; ''e'' &le; N. It specifies the
 *                  index number at which the code stops computing the L1 norm
 *                  of the variables `x`. Setting this parameter to [[None]] or
 *                  [[Some]] with a negative value will compute the L1 norm for
 *                  all the variables `x`, which is useful when the number of
 *                  variables `x` (''N'') is not known.
 */
case class OrthantWiseParameters(
    c: Double,
    start: Int = 0,
    end: Option[Int] = None
):
    def project(
       d: VectorD,
       sign: VectorD
    ): VectorD =
        val result = d.copy
        val adjustedEnd = end match
            case None => d.length
            case Some(index) => if index > 0 then index else d.length

        for i <- start until adjustedEnd do
            if d(i) * sign(i) <= 0 then
                result(i) = 0
            end if
        end for

        result

    def pseudoGradient(
        x: VectorD,
        g: VectorD
    ): VectorD =
        val n = x.length
        val pg = new VectorD(n)
        val adjustedEnd = end match
            case None => n
            case Some(index) => if index > 0 then index else n

        /* Compute the negative of gradients. */
        for i <- 0 until start do
            pg(i) = g(i)
        end for

        /* Compute the psuedo-gradients. */
        for i <- start until adjustedEnd do
            if x(i) < 0.0 then
            /* Differentiable. */
                pg(i) = g(i) - c
            else if 0.0 < x(i) then
            /* Differentiable. */
                pg(i) = g(i) + c
            else if g(i) < -c then
            /* Take the right partial derivative. */
                pg(i) = g(i) + c
            else if c < g(i) then
            /* Take the left partial derivative. */
                pg(i) = g(i) - c
            else
                pg(i) = 0.0
            end if
        end for

        for i <- adjustedEnd until n do
            pg(i) = g(i)

        pg


    def x1Norm(x: VectorD): Double =
        var norm = 0.0
        val adjustedEnd = end match
            case None => x.length
            case Some(index) => if index > 0 then index else x.length

        for i <- start until adjustedEnd do
            norm += abs(x(i))

        norm
end OrthantWiseParameters
