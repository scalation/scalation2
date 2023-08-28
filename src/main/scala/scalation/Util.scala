
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Thu Jul  9 17:40:18 EDT 2015
 *  @see     LICENSE (MIT style license file).
 *
 *  @title   Util - basic utilities: top-level constants and functions.
 */

package scalation

import java.io.File
import java.lang.System.nanoTime
import java.net.{MalformedURLException, URI} // URL

import scala.collection.mutable.ArrayBuffer
import scala.math.{max, min}
import scala.io.Source.{fromFile, fromURL}
import scala.util.Properties.envOrElse

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/*  Top-level definitions of values/constants and functions of utilities for files, timing
 *  and language extensions such as cfor.
 *  @see ValueType.scala for top-level definitions related to basic datatypes.
 *  @see CommonFunctions.scala for top-level definitions related to common scalar functions.
 */

/** Type definition for an interval [a, b]
 */
type Interval = (Double, Double)

/** Universal symbol for the ? character (e.g., meaning all rows of column 2 in x(?, 2) for maxtrix x)
 */
val ? = '?'

/** The file path separation character: '/' for Linux/Mac, '\' for Windows
 *  Use either 'SEP' or '⁄' for portability, i.e., do not use '/' or '\'.
 */
val SEP = File.separator
val ⁄   = File.separator                                    // Unicode symbol

/** File-name extension for serialized tables
 *  FIX: investigate using more efficient serialization, e.g.,
 *  @see github.com/EsotericSoftware/kryo
 */
val SER = ".ser"

/** File-name extension for CSV data files
 */
val CSV = ".csv"

/** File-name extension for JSON data files
 */
val JSON = ".json"

/** The token/element separation character (',' for CSV)
 */
val SP = ','

/** Base directory for ScalaTion (pick one, comment out the rest)
 */
val BASE = System.getProperty ("user.dir")                  // absolute path
// val BASE = "."                                           // relative path

/** File system path for input/output data directory
 *  Use 'SCALATION_HOME' environment variable or else BASE directory "."
 */
val DATA_DIR = envOrElse ("SCALATION_HOME", BASE) + ⁄ + "data" + ⁄

/** File system path for log (log/file output) directory
 *  Use 'SCALATION_HOME' environment variable or else BASE directory "."
 */
val LOG_DIR = envOrElse ("SCALATION_HOME", BASE) + ⁄ + "log" + ⁄

/** File system path for src (source code) directory
 *  Use 'SCALATION_HOME' environment variable or else BASE directory "."
 */
val SRC_DIR = envOrElse ("SCALATION_HOME", BASE) + ⁄ + "src" + ⁄
val SRC_SCALA_DIR = SRC_DIR + ⁄ + "main" + ⁄ + "scala" + ⁄

/** File system path for database storage directory
 *  Use 'SCALATION_HOME' environment variable or else BASE directory "."
 */
val STORE_DIR = envOrElse ("SCALATION_HOME", BASE) + ⁄ + "store" + ⁄

/** The number of units in one second
 *  Use 1.0 to make SECOND the base unit, 1.0 / 60.0 to make MINUTE the base unit, etc.
 *  @see `TimeNum` for alternative definitions
 */
val SECOND = 1.0 / 60.0

/** The number of seconds in one minute
 */
val MINUTE = 60.0 * SECOND

/** The number of minutes in one hour
 */
val HOUR = 60.0 * MINUTE

/** The number of hours in one day
 */
val DAY = 24.0 * HOUR

/** The number of days in one week
 */
val WEEK = 7.0 * DAY

/** The number of nanoseconds per millisecond
 */
val NS_PER_MS = 1E-6

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Remove the last path element from the file/directory pathname.
 *  @param s  the string to be so truncated
 */
def removeLast (s: String): String = s.substring (0, s.lastIndexOf (SEP))

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Print the elapsed time in milliseconds (ms) for the execution of an
 *  arbitrary block of code:  'time { block }'.  Return any result produced
 *  by the block of code.
 *  @see http://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala
 *  @param block  the block of code to be executed
 */
def time [R] (block: => R): R =
    val t0 = nanoTime ()
    val result = block                                     // call-by-name
    val t1 = nanoTime ()
    println ("Elapsed time: " + (t1 - t0) * NS_PER_MS + " ms")
    result
end time
    
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Calculate the elapsed time in milliseconds (ms) for the execution of an
 *  arbitrary block of code:  'timed { block }'.  Return any result produced
 *  by the block of code and its elapsed time.
 *  @see http://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala
 *  @param block  the block of code to be executed
 */
def timed [R] (block: => R): (R, Double) = 
    val t0 = nanoTime ()
    val result = block                                     // call-by-name
    val t1 = nanoTime ()
    (result, (t1 - t0) * NS_PER_MS)
end timed

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Calculate the elapsed time in milliseconds (ms) for the execution of an
 *  arbitrary block of code:  'gauge { block }'.  Return the block of code's
 *  elapsed time.
 *  @see http://stackoverflow.com/questions/9160001/how-to-profile-methods-in-scala
 *  @param block  the block of code to be executed
 */
def gauge [R] (block: => R): Double = 
    val t0 = nanoTime ()
    val result = block                                     // call-by-name
    val t1 = nanoTime ()
    (t1 - t0) * NS_PER_MS
end gauge

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Return the free, total, and max memory in MiB.
 */
def memoryUsed: Array [Long] =
    val MB = 1024 * 1024
    val runtime = Runtime.getRuntime
    Array (runtime.freeMemory / MB, runtime.totalMemory / MB, runtime.maxMemory / MB)
end memoryUsed

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Return a line iterator for a line-oriented data source (e.g., CSV file).
 *  The data source is accessed via (1) URL, (2) file's absolute path, or
 *  (3) file's relative path (relative to 'DATA-DIR').
 *  @see stackoverflow.com/questions/5713558/detect-and-extract-url-from-a-string
 *  @param path  the path name of the data source (via URL or file's path name)
 */
def getFromURL_File (path: String): Iterator [String] =
    val urlPat = "(?i)((https?|ftp|file)://|file:/).*"     // (?i) => case insensitive
    if path matches urlPat then
        try
//          return fromURL (new URL (path)).getLines ()
            return fromURL (new URI (path).toURL).getLines ()
        catch
            case mue: MalformedURLException => 
        end try    
    end if

    val file = new File (path)
//  if file.isAbsolute () then fromFile (file).getLines ()
    if file.exists ()     then fromFile (file).getLines ()
    else
        println (s"getFromURL_File: file '$path' does not exist, try prefixing DATA-DIR")
        fromFile (DATA_DIR + path).getLines ()
    end if
end getFromURL_File

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Make a string for use in printing a line of '-'s.
 *  @param n  the number of '-'s to use
 */
def sline (n: Int = 60): String = "-" * n + "\n"

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Print a banner, i.e., a string in a box.
 *  @param str  the string to put in the banner
 */
def banner (str: String): Unit =
    val len = str.size + 4
    println ("-" * len)
    println ("| " + str + " |")
    println ("-" * len)
end banner

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Show the flaw by printing the error message and returning false.
 *  Usage: def flaw: Unit = flawf ("MyClass")
 *         flaw ("myMethod", "Houston we have a problem")
 *  @param clsName  the class/trait/object containing the method
 *  @param method   the method where the error occurred
 *  @param message  the error message
 */
def flawf (clsName: String)(method: String, message: String): Boolean =
    println (s"ERROR @ $clsName.$method: $message")
    false
end flawf

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Show the debugging information that can be enabled or disabled.
 *  Usage: def debug: Unit = debugf ("MyClass", true)
 *         debug ("myMethod", s"x = $x")
 *  @param clsName  the class/trait/object containing the method
 *  @param enabled  indicates whether the debug message is enabled
 *  @param method   the method where the error occurred
 *  @param message  the error message
 */
def debugf (clsName: String, enabled: Boolean)(method: String, message: String): Unit =
    if enabled then
        println (s"DEBUG @ $clsName.$method: $message")
end debugf

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Return the type of an object as string, e.g., "Double".
 *  @param o  the object whose type is sought
 */
def typeOf (o: Any): String = o.getClass.getSimpleName

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Remove 'n' characters starting at position 'i' in string 'str' and
 *  return the new string.
 *  @param str  the source string 
 *  @param i    the starting position for character removal
 *  @param n    the number of characters to remove
 */
def removeAt (str: String, i: Int, n: Int = 1): String =
    str.substring (0, i) + str.substring (i + n)
end removeAt

def removeAt (str: Array [Char], i: Int, n: Int): Array [Char] =
    str.slice (0, i) ++ str.slice (i + n, str.length)
end removeAt

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** C/Java style for loop provides improved performance.
 *  usage: cfor (i < 10, i += 1) { a(i) = 2 * i }
 *  @see www.reddit.com/r/scala/comments/4m2qgb/will_the_new_optimizer_in_scala_212_help_optimize/
 *  @param pred  loop continuation predicate
 *  @param step  increment step
 *  @param body  main body of the loop
 */
inline def cfor (pred: => Boolean, step: => Unit) (body: => Unit): Unit =
    while pred do { body; step }
end cfor

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** C/Java style for loop provides improved performance.
 *  usage: cfor ({i = 0}, i < 10, i += 1) { a(i) = 2 * i }
 *  @see www.reddit.com/r/scala/comments/4m2qgb/will_the_new_optimizer_in_scala_212_help_optimize/
 *  @param init  initialization part
 *  @param pred  loop continuation predicate
 *  @param step  increment step
 *  @param body  main body of the loop
 */
inline def cfor (init: => Unit, pred: => Boolean, step: => Unit) (body: => Unit): Unit =
    init; while pred do { body; step }
end cfor

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Summation of the formula applied n times.
 *  @param n        the number of iteraations
 *  @param formula  the formula to apply, repeatedly
 */
inline def summation (n: Int)(formula: => Double): Double =
    var sum = 0.0
    for i <- 0 until n do sum += formula
    sum
end summation

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Convert `Boolean` to `Int` (true -> 1, false -> 0).
 *  @param p  the predicate to convert
 */
inline def is (p: Boolean): Int = if p then 1 else 0
inline def is_ (p: Boolean): Double = if p then 1.0 else 0.0

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Swap two elements in a regular array.
 *  @param i  first element to swap
 *  @param j  second element to swap
 */
inline def swap [T] (a: Array [T], i: Int, j: Int): Unit =
    val t = a(i); a(i) = a(j); a(j) = t
end swap

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Swap two elements in a resizable array.
 *  @param i  first element to swap
 *  @param j  second element to swap
 */
inline def swap [T] (a: ArrayBuffer [T], i: Int, j: Int): Unit =
    val t = a(i); a(i) = a(j); a(j) = t
end swap

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** Find the median of three unordered numbers.
 *  @param a1  the first number 
 *  @param a2  the second number 
 *  @param a3  the third number 
 */
@inline def median3 (a1: Double, a2: Double, a3: Double): Double =
    max (min (a1, a2), min (max (a1, a2), a3))
end median3

