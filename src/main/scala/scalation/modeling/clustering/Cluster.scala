
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 1.0
 *  @date    Sat Apr 27 12:55:24 EDT 2019
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Cluster Record
 */

package scalation
package modeling
package clustering

import scalation.mathstat.{MatrixD, VectorD}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Cluster` object is used for creating auto-increment identifiers for
 *  cluster ids.
 */
object Cluster:

    private var c = -1                                             // value for cluster id

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the next cluster id.
     */
    def next (): Int = { c += 1; c }

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Reset the cluster id counter.
     */
    def reset (): Unit = c = -1

end Cluster


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Cluster` case class maintains information about clusters, the
 *  cluster id, center/centroid, cluster size, and measure of error.
 *  Note: the cluster assignment function as an array 'to_c' indicates how
 *  points are assigned to clusters.
 *  @see `package.scala` for the definition of the 'distance' method
 *  @param c     the cluster id
 *  @param np    the number of points in the cluster (size)
 */
case class Cluster (c: Int = Cluster.next (), var np: Int = 0):

    private val flaw = flawf ("Cluster")                           // flaw function

    private var cen_ = VectorD.nullv                               // coordinates of the centroid
    private var sse_ = 0.0                                         // sum of squared errors for the cluster

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the centroid for this cluster.
     *  @param x     the data matrix where each row is a data point
     *  @param to_c  the data point assignment function
     */
    def cenf (x: MatrixD, to_c: Array [Int]): VectorD =
        if np < 1 then { flaw ("cenf", s"no centroid since cluster $c is empty"); return null }
        val cn = new VectorD (x.dim2)
        for i <- to_c.indices if to_c(i) == c do cn += x(i)
        cn / np
    end cenf

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the centroid for this cluster.
     *  @param cn  the calculated centroid
     */
    def set_cen (cn: VectorD): Unit = cen_ = cn

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the stored centroid for this cluster.
     */
    def cen: VectorD = cen_

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the sum of squared errors for this cluster.
     *  @param x     the data matrix where each row is a data point
     *  @param to_c  the data point assignment function
     *  @param cn    the hypothetical centroid (defaults to current centroid) 
     */
    def ssef (x: MatrixD, to_c: Array [Int], cn: VectorD = cen): Double =
        var ss = 0.0
        for i <- to_c.indices if to_c(i) == c do ss += dist (x(i), cn)
        ss
    end ssef

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Set the sum of squared error for this cluster.
     *  @param ss  the calculated sum of squared errors
     */
    def set_sse (ss: Double): Unit = sse_ = ss

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Return the stored sum of squared errors for this cluster.
     */
    def sse: Double = sse_

end Cluster

