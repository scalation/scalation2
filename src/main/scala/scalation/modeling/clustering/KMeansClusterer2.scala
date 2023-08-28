
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Tue May 29 14:45:32 EDT 2012
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Model: K-Means Clustering (pick random points as initial centroids)
 */

package scalation
package modeling
package clustering

import scalation.mathstat._
import scalation.random.RandomVecI

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `KMeansClusterer2` class cluster several vectors/points using k-means
 *  clustering.  Randomly pick 'k' points as initial centroids (secondary technique).
 *  Iteratively, reassign each point to the cluster containing the closest centroid.
 *  Stop when there are no changes to the clusters.
 *  @see `KMeansClusterer` for primary technique.
 *-----------------------------------------------------------------------------
 *  @param x      the vectors/points to be clustered stored as rows of a matrix
 *  @param k      the number of clusters to make
 *  @param flags  the flags used to adjust the algorithm
 */
class KMeansClusterer2 (x: MatrixD, k: Int, flags: Array [Boolean] = Array (false, false))
      extends KMeansClusterer (x, k, flags):

    private val DEBUG = false                                            // debug flag

    //:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Randomly pick vectors/points to serve as the initial 'k' centroids 'cent'.
     *  Secondary technique for initiating the clustering.
     */
    override def initCentroids (): Boolean =
        val rvi = RandomVecI (k, x.dim-1, 0, stream = stream).igen       // random vector of integers
        for (i <- 0 until k) cent(i) = x(rvi(i))                         // set the centroids
        true                                                             // yes, they are initialized
    end initCentroids

end KMeansClusterer2

import Clusterer.test

//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `kMeansClusterer2Test` object is used to test the `KMeansClusterer2` class.
 *  > runMain scalation.modeling.clusterer.kMeansClusterer2Test
 */
@main def kMeansClusterer2Test (): Unit =

    import Clusterer.x

    val k   = 3
    val opt = 3.0

    println (s"x = $x")
    println (s"k = $k")
    println ("-" * 60)

    val tf = Array (true, false)
    for fl0 <- tf; fl1 <- tf do
        val fls = Array (fl0, fl1)
        test (x, fls, new KMeansClusterer2 (x, k, fls), opt)
    end for

    new Plot (x(?, 0), x(?, 1), null, "x0 vs x1")

end kMeansClusterer2Test


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `kMeansClusterer2Test2` object is used to test the `KMeansClusterer2` class.
 *  > runMain scalation.modeling.clusterer.kMeansClusterer2Test2
 */
@main def kMeansClusterer2Test2 (): Unit =

    //                         x0    x1
    val x = MatrixD ((8, 2),  1.0,  1.0,
                              1.0,  3.0,
                              5.0, 18.0,
                              5.0, 20.0,
                              9.0, 10.0,
                              9.0, 12.0,
                             15.0, 30.0,
                             15.0, 32.0)

    val k   = 4
    val opt = 8.0

    println (s"x = $x")
    println (s"k = $k")
    println ("-" * 60)

    val tf = Array (true, false)
    for fl0 <- tf; fl1 <- tf do
        val fls = Array (fl0, fl1)
        test (x, fls, new KMeansClusterer2 (x, k, fls), opt)
    end for

    new Plot (x(?, 0), x(?, 1), null, "x0 vs x1")

end kMeansClusterer2Test2


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `kMeansClusterer2Test2` object is used to test the `KMeansClusterer2` class.
 *  > runMain scalation.modeling.clustering.kMeansClusterer2Test3
 */
@main def kMeansClusterer2Test3 (): Unit =

    import scalation.random.{Bernoulli, Normal}

    val coin  = Bernoulli ()
    val dist1 = Normal (2.0, 1.0)
    val dist2 = Normal (8.0, 1.0)
    val x    = new MatrixD (50, 2)
    val k    = 4
    val opt  = 76.0

    for i <- x.indices do x(i) = VectorD (if coin.gen == 0 then dist1.gen else dist2.gen,
                                          if coin.gen == 0 then dist1.gen else dist2.gen)

    println (s"x = $x")
    println (s"k = $k")
    println ("-" * 60)

    val tf = Array (true, false)
    for fl0 <- tf; fl1 <- tf do
        val fls = Array (fl0, fl1)
        test (x, fls, new KMeansClusterer2 (x, k, fls), opt)
    end for

    new Plot (x(?, 0), x(?, 1))

end kMeansClusterer2Test3


//:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `kMeansClusterer2Test4` object is used to test the `KMeansClusterer2` class.
 *  > runMain scalation.modeling.clustering.kMeansClusterer2Test4
 */
@main def kMeansClusterer2Test4 (): Unit =

    import scalation.random.{Normal, Bernoulli}

    val coin  = Bernoulli ()
    val dist1 = Normal (2.0, 1.0)
    val dist2 = Normal (8.0, 1.0)
    val x    = new MatrixD (100, 2)
    val k    = 4
    val opt  = 171.0

    for i <- x.indices do x(i) = VectorD (if coin.gen == 0 then dist1.gen else dist2.gen,
                                          if coin.gen == 0 then dist1.gen else dist2.gen)

//  import org.apache.commons.math3.ml.clustering.KMeansPlusPlusClusterer2
//  val cl = new KMeansPlusPlusClusterer2 (k)
    
    println (s"x = $x")
    println (s"k = $k")
    println ("-" * 60)

    val tf = Array (true, false)
    for fl0 <- tf; fl1 <- tf do
        val fls = Array (fl0, fl1)
        test (x, fls, new KMeansClusterer2 (x, k, fls), opt)
    end for

    new Plot (x(?, 0), x(?, 1))

end kMeansClusterer2Test4

