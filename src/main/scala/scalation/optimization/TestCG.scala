
//package scalation
//package optimization

package scalation
package optimization


import scalation.mathstat._

//import scalation.optimization.ConjugateGradient

def f7 (x: VectorD): Double = 1/x(0) + x(0)~^4 + (x(0) - 3)~^2 + (x(1) - 4)~^2 + 1.0

val optimizer = new ConjugateGradient (f7)
