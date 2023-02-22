
//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author Yulong
  * @version 1.0
  * @date Thursday Feb 17 13:32:52 EDT 2022
  * @see LICENSE (MIT style license file).
  * @title Simultaneous perturbation stochastic approximation
  */

import scalation.mathstat.*
import scalation.random.Bernoulli
import scalation.random.Normal
import scalation.~^
import scala.math.pow



//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

//link1: https://www.jhuapl.edu/spsa/PDF-SPSA/Matlab-SPSA_Alg.pdf

class SPSA(f: FunctionV2S,theta:VectorD, nSteps: Int = 20): //extends Minimizer:

  private val alpha  = 0.602
  private val gamma = 0.101
  private val A = 100
  private val a = 0.16  //these numbers are from Spall (1998) DOI: 10.1109/7.705889
  private val c = 1
  private val bernoully = Bernoulli(0.5,5)

  var x_best = theta  //best theta or parameter to get the lowest error from loss function
  var f_best = Double.MaxValue
  var x = theta.copy //copy by value




  def basic(): Double=
    for k <- 0 to nSteps do
      val ak = a / pow(A + k + 1, alpha)
      val ck = c /pow(k + 1,gamma)
      val delta = 2 * bernoully.igen - 1 // -1 or 1
      val thetaplus = x + ck * delta
      val thetaminus = x - ck * delta
      val yplus = f(thetaplus)
      val yminus = f(thetaminus)
      val ghat = (yplus - yminus) / (2 * ck * delta)
      x -= ak * ghat
      if f(x) < f_best then
        x_best = x.copy //copy by value
        f_best = f(x_best)
      end if

    end for

    println(s"x_last is $x and y(x_last) at the end  is ${f(x)} and \n " +
      s"lowest is $x_best and $f_best")
    f_best
  end basic




end SPSA



@main def SPSATest (): Unit =

  // banner ("Minimize: (x_0 - 3)^2 + (x_1 - 4)^2 + 1")
  val noise=Normal(0,0.1)
  def f (x: VectorD): Double = (x(0) - 3.0)~^2 + (x(1) - 4.0)~^2 + 1.0 + noise.gen
  val n= VectorD(1,2)
  val optimizer = new SPSA(f,n,1200 )
  val opt = optimizer.basic()
  //val opt = optimizer.fastconvergence2() //
  println (s"][ optimal solution (f(x), x) = $opt")



end SPSATest

