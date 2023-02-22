
package scalation

import java.util.Random

case class SimpleUniform (a: Double, b: Double):

    private val rng = new Random ()

    def gen: Double = a + (b - a) * rng.nextDouble

end SimpleUniform

