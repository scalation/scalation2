
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller, Casey Bowman
 *  @version 2.0
 *  @date    Fri 31 May 2024 12:54:22 PM EDT
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Coordinates maps Lat-Long pairs to xy pairs in an animation environment
 */

package scalation

import scala.collection.mutable.ListBuffer
import scala.math.sqrt

import LatLong2UTM.latLong2UTMxy
import LatLong2CTM.latLong2CTMxy

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `Coordinates` class calculates the animation coordinates for a set of
 *  provided lat-long coordinates.  Uses CTM (Custom Transverse Mercator) 
 *  coordinates as an intermediate step.
 *  @param aniWidth   the width of the animation window
 *  @param aniHeight  the height of the animation window
 *  @param coords     an array of lat-long coordinates
 */
class Coordinates (aniWidth: Double, aniHeight: Double, coords: Array [(Double, Double)]):

// For general case, calculate new central meridian based on coords and create 
//   a custom transverse mercator projection so that all the points are in the same 
//   zone.  The northing values should be unaffected.  New easting values should 
//   all make sense together.  
//
//   In LatLong, in setVariables, "var2" is the value of the central meridian.  
//   Setting this value to a custom central meridian will yield the custom UTM-type
//   zone that is needed.  Everything else would stay the same.

    private val cent = getCentral (coords)
    private val xt   = 20.0                                   // x and y margins
    private val yt   = 20.0
    private val aw   = aniWidth  - 2.0 * xt                   // animation width  (adjusted for margins)
    private val ah   = aniHeight - 2.0 * yt                   // animation height (adjusted for margins)
            val cTMCoords = getCtmCoords ()                   // ListBuffer of UTM coordinates based on the lat-long coordinates
    private val nsew = findNSEW ()                            // nsew is the bounding box for the UTM coordinates
            val (aniCoords, scale) = calcAniCoords ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the central meridian for the CTM by taking the average longitude
     *  of all lat-longs in the set. 
     *  @param c  the array (set) of lat-longs
     */
    def getCentral (c: Array [(Double, Double)]): Double = 
        var sum = 0.0
        for l <- c do sum += l._2
        sum / c.length
    end getCentral

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Calculate the animation coordinates that will represent the lat-long
     *  coordinates provided to the class.
     */
    def calcAniCoords (): (ListBuffer [(Double, Double)], Double) =
        
        val aniCoords = ListBuffer [(Double, Double)] ()
        val (s, w) = (nsew(1), nsew(3))
        val ctmT = for c <- cTMCoords yield (c._1 - w, c._2 - s)
        val ch = nsew(5)                              // CTM bounding box height
        val cw = nsew(4)                              // CTM bounding box width
        val aniAspect = ah / aw
        val ctmAspect = ch / cw
//      val ctmAspRec = cw / ch
        val scale     = if aniAspect <= ctmAspect then ah / ch else aw / cw

        for c <- ctmT do 
            val nx = c._1 * scale + xt
            val ny = c._2 * -scale + ah + yt
            val np = (nx, ny)
            aniCoords += np
        end for        

        (aniCoords, scale)
    end calcAniCoords

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Convert Latitude-Longitude coordinates to CTM coordinates using the
     *  coords parameter of the class and output as a ListBuffer.
     */
    def getCtmCoords (): ListBuffer [(Double, Double)] =
        val c = ListBuffer [(Double, Double)] ()
        for cc <- coords do c += latLong2CTMxy (new LatLong (cc), cent)
        c
    end getCtmCoords

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Find the northern-most, southern-most, eastern-most, and western-most
     *  CTM x-values and y-values in the set of provided coordinates.  
     *  NOTE:  This does not necessarily correspond to actual complete coordinates
     *         in the set.  These values provide a bounding box around the set.
     */
    def findNSEW (): Array [Double] =
        val xs = for c <- cTMCoords yield c._1
        val ys = for c <- cTMCoords yield c._2
        val ysmax = ys.max; val ysmin = ys.min; 
        val xsmax = xs.max; val xsmin = xs.min; 
        Array (ysmax, ysmin, xsmax, xsmin, xsmax - xsmin, ysmax - ysmin)
    end findNSEW
    
    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Utility function for calculating the distance between two points as (Double, Double)
     *  values.  
     *  @param p  the first point (Double, Double)
     *  @param q  the second point (Double, Double)
     */
    def dist (p: (Double, Double), q: (Double, Double)): Double =
        sqrt ((p._1 - q._1) * (p._1 - q._1) + (p._2 - q._2) * (p._2 - q._2))
    end dist

end Coordinates


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Test main for the `Coordinates` class.  The lat-longs are collected coordinates
 *  around the Loop 10 highway in Athens, GA, USA.
 *  > runMain scalation.coordinatesTest
 */
@main def coordinatesTest (): Unit =

    val coords = Array ((33.91634138069811,  -83.4051542337287), 
                        (33.933357972261106, -83.36724178356303), 
                        (33.94675707615584,  -83.35608200084363), 
                        (33.97805332627655,  -83.35240177319633),
                        (33.977939520362476, -83.37819359017722),
                        (33.971885410013265, -83.4120232202679),
                        (33.96427038393156,  -83.42809200638573),
                        (33.93966752188796,  -83.46202941910865),
                        (33.91661409923615,  -83.45959899962827),
                        (33.91279189753823,  -83.45093951967232),
                        (33.92110134424739,  -83.37806326666644))
                 
    val c = new Coordinates (840.0, 540.0, coords)
                                                  
    val scale = c.scale

    val ac = c.aniCoords
                       
    val lls = for cc <- coords yield new LatLong (cc)
    
    println (":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
    println ("Distance computed using Lat-Long coordinates")
    
    for i <- 0 until lls.length - 1 do println (lls(i).distance (lls(i + 1)))
    
    println (":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
    println ("Distance computed using Universal Transverse Mercator Projection")
    
    val utms = for ll <- lls yield latLong2UTMxy (ll)
    
    for i <- 0 until utms.length - 1 do println (c.dist (utms(i), utms(i + 1)))
    
    println (":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
    println ("Distance computed using Custom Transverse Mercator Projection")

    for i <- 0 until ac.length - 1 do println (c.dist (ac(i), ac(i + 1)) / scale) 
    
    println (":::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::")
    println ("Distance computed using UTM Projection at https://www.zonums.com/online/coords/cotrans.php?module=13")
    
    val utms2 = Array ((277646.872,3755485.132),
                       (281196.135,3757290.972),
                       (282261.993,3758753.348),
                       (282681.836,3762216.691),
                       (280298.344,3762259.084),
                       (277156.604,3761660.666),
                       (275651.685,3760851.112),
                       (272449.715,3758197.084),
                       (272613.092,3755634.710),
                       (273403.711,3755191.612),
                       (280164.179,3755954.693))  
    
    for i <- 0 until utms2.length - 1 do println (c.dist (utms2(i), utms2(i + 1)))
    
end coordinatesTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Test main for `Coordinates` class.  The lat-longs are collected coordinates
 *  around the Loop 10 highway in Athens, GA, USA.
 *  > runMain scalation.coordinatesTest2
 */
@main def coordinatesTest2 (): Unit =

    val c = new Coordinates (840.0, 540.0, Array ((0.0,  -90.0), 
                                                  (10.0, -90.0),                                                   
                                                  (20.0, -90.0),
                                                  (30.0, -90.0),
                                                  (40.0, -90.0),
                                                  (0.0,  -84.0001), 
                                                  (10.0, -84.0001),                                                   
                                                  (20.0, -84.0001),
                                                  (30.0, -84.0001),
                                                  (40.0, -84.0001)))

    for co <- c.cTMCoords do println (co)

    println (c.aniCoords)    
    println (s"scale = ${c.scale}")

end coordinatesTest2


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Test main for `Coordinates` class.  The lat-longs are collected coordinates
 *  around the Loop 10 highway in Athens, GA, USA.
 *  > runMain scalation.coordinatesTest3
 */
@main def coordinatesTest3 (): Unit =

    val c = new Coordinates (840.0, 540.0, Array ((33.91634138069811,  -83.4051542337287), 
                                                  (33.933357972261106, -83.36724178356303)))

    for co <- c.cTMCoords do println (co)

//  println (c.aniCoords)    
//  println (s"scale = ${c.scale}")

end coordinatesTest3


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Test main for `Coordinates` class.  The lat-longs are collected coordinates
 *  around the 101N highway in San Francisco, CA, USA
 *  401474, 404533, 425697,402380, 402376, 400388, 402379, 400981, 401653
 *  419452No data, 405673, 400859, 401927, 401872, 404534, 402398, 404529
 *  422116, 401874, 402377
 *  > runMain scalation.coordinatesTest4
 */
@main def coordinatesTest4 (): Unit =

    val c = new Coordinates (840.0, 540.0, Array ((37.47483,  -122.165147),
                                                  (37.4652,   -122.148841),
                                                  (37.450295, -122.124287),
                                                  (37.451442, -122.125642),
                                                  (37.435945, -122.109246),
                                                  (37.480426, -122.174688),
                                                  (37.445961, -122.11976),
                                                  (37.468732, -122.154696),
                                                  (37.483811, -122.181671),
                                                  //(No data,     no data), // this sensor has no data
                                                  (37.486916, -122.200187),
                                                  (37.463019, -122.145022),
                                                  (37.483144, -122.17985),
                                                  (37.484232, -122.183031),
                                                  (37.472754, -122.161627),
                                                  (37.46959,  -122.156168),
                                                  (37.45328,  -122.12842),
                                                  (37.456467, -122.133857),
                                                  (37.487321, -122.203433),
                                                  (37.440393, -122.113925)))

    for co <- c.cTMCoords do banner (co.toString())
//  println (c.aniCoords)
//  println (s"scale = ${c.scale}")

end coordinatesTest4

