
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  Casey Bowman, John Miller
 *  @version 2.0
 *  @date    Wed Jun  8 13:16:15 EDT 2016
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    EasyWriter Allows Switching between Standard Output and Files
 */

package scalation

import java.io.{FileOutputStream, PrintStream, PrintWriter, Writer}

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `redirectOutTest` main function is used to redirect standard output (stdout).
 *  FIX - should work without writing `System.out`.
 *  > runMain scalation.redirectOutTest
 */
@main def redirectOutTest (): Unit =
 
    val path = LOG_DIR + "outfiles" + ⁄ + "stdout"
    println (s"set stdout to $path")
    System.setOut (new PrintStream (new FileOutputStream (path)))

    System.out.println ("redirectOutTest should redirect standard output to a file")
    println ("redirectOutTest should redirect standard output to a file ???")

end redirectOutTest


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `EasyWriter` class makes it easy to switch between writing to standard output
 *  and a (log) file.
 *  @param project   the project or directory involved
 *  @param filename  the name of the file to be written
 *  @param toFile    flag indicating whether to write to a file
 */
class EasyWriter (project: String, filename: String, private var toFile: Boolean = true)
      extends Writer ():

    private val debug = debugf ("EasyWriter", true)                    // debug function

    /** The file path for the (log) file
     */
    private val LOG_PATH = LOG_DIR + project + ⁄ + filename
    debug ("init", s"log file path = $LOG_PATH")

    /** The internal `PrintWriter` to write to a file
     */
    private val pw = new PrintWriter (LOG_PATH)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Toggle between writing to a file and standard output.
     */
    def toggle (): Unit = toFile = ! toFile

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print/write the string s.
     *  @param s  the string to printed/written
     */
    def print (s: String): Unit = if toFile then pw.print (s) else Console.print (s)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print/write the string s and add a newline ('\n') at the end.
     *  @param s  the string to printed/written
     */
    def println (s: String): Unit = if toFile then pw.println (s) else Console.println (s)

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Print/write a newline ('\n').
     */
    def println (): Unit = if toFile then pw.println ("") else Console.println ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Write the character buffer.
     *  @param cbuf  the character buffer
     *  @param off   the offset in 'cbuf' to starting writing
     *  @param len   the length/number of characters to write
     */
    def write (cbuf: Array [Char], off: Int, len: Int): Unit =
        if toFile then pw.write (cbuf, off, len)
        else Console.print (cbuf.slice (off, off + len).mkString (""))
    end write

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Flush (force write) the output to the file.
     */
    def flush (): Unit = if toFile then pw.flush () else Console.flush ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Close the file (not applicable to standard output).
     */
    def close (): Unit = if toFile then pw.close ()

    //::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
    /** Finish by calling flush and close.
     */
    def finish (): Unit =
        flush (); close ()
    end finish

end EasyWriter


//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `easyWriterTest` main function is used to test the `EasyWriter` class.
 *  It will write into a file, unless there is a command-line argument.
 *  > runMain scalation.easyWriterTest
 */
@main def easyWriterTest (args: String*): Unit =

    val ew = new EasyWriter ("scalation", "test.txt")

    if args != null && args.length > 0 then ew.toggle ()             // switch to standard output

    val s = "Hello World!"

    ew.print (s)
    ew.println (s)
    ew.write (s, 2, 4)
    ew.finish ()

end easyWriterTest

