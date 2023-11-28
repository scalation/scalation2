
//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** @author  John Miller
 *  @version 2.0
 *  @date    Fri Jun 17 11:19:14 EDT 2022
 *  @see     LICENSE (MIT style license file).
 *
 *  @note    Example Database: Movie Database
 */

package scalation
package database
package table

//::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
/** The `movieDB` main function uses the `Table` class for simple database application.
 *  > runMain scalation.database.table.movieDB
 */
@main def movieDB (): Unit =

    val movie = Table ("movie", "title, year, length, genre, studioName, producerNo", "S, I, I, S, S, I", "title, year")

    val cinema = Table ("cinema", "title, year, length, genre, studioName, producerNo", "S, I, I, S, S, I", "title, year")

    val movieStar = Table ("movieStar", "name, address, gender, birthdate", "S, S, S, S", "name")

    val starsIn = Table ("starsIn", "movieTitle, movieYear, starName", "S, I, S", "movieTitle, movieYear, starName")

    val movieExec = Table ("movieExec", "certNo, name, address, fee", "I, S, S, D", "certNo")

    val studio = Table ("studio", "name, address, presNo", "S, S, I", "name")

    val film2 = Array [ValueType] ("Rocky", 1985, 200, "action", "Universal", 12125)
    val film3 = Array [ValueType] ("Rambo", 1978, 100, "action", "Universal", 32355)

    movie.add ("Star_Wars", 1977, 124, "sciFi", "Fox", 12345)
         .add ("Star_Wars_2", 1980, 124, "sciFi", "Fox", 12345)
         .add (film2)
         .add (film3)
         .show ()

    cinema.add (film2)
          .add (film3)
          .add ("Galaxy_Quest", 1999, 104, "comedy", "DreamWorks", 67890)
          .show ()

    movieStar.add ("Carrie_Fisher", "Hollywood", "F", "9/9/99")
             .add ("Mark_Hamill", "Brentwood", "M", "8/8/88")
             .add ("Harrison_Ford", "Beverly_Hills", "M", "7/7/77")
             .show ()

    starsIn.add ("Star_Wars", 1977, "Carrie_Fisher")
           .show ()

    movieExec.add (9999, "S_Spielberg", "Hollywood", 10000.00)
             .show ()

    studio.add ("Fox", "Los_Angeles", 7777)
          .add ("Universal", "Universal_City", 8888)
          .add ("DreamWorks", "Universal_City", 9999)
          .show ()

    movie.save ()
    cinema.save ()
    movieStar.save ()
    starsIn.save ()
    movieExec.save ()
    studio.save ()

    movieStar.create_index ()
    movieStar.show_index ()

    banner ("""movie.π ("title, year")""")
    movie.project (Array ("title", "year")).show ()
    movie.project ("title, year").show ()
    movie.π ("title, year").show ()

    banner ("""movie.σ (t => t(movie.on("title")) == "Star_Wars" && t(movie.on("year")) == 1977)""")
    movie.select (t => t(movie.on("title")) == "Star_Wars" && t(movie.on("year")) == 1977).show ()
    movie.σ (t => t(movie.on("title")) == "Star_Wars" && t(movie.on("year")) == 1977).show ()

    banner ("""movie.σ (t => t(movie.on("year")) < 1980)""")
    movie.select (t => t(movie.on("year")) < 1980).show ()
    movie.σ (t => t(movie.on("year")) < 1980).show ()

    banner ("""movie.σ ("year < 1980")""")
    movie.select ("year < 1980").show ()
    movie.σ ("year < 1980").show ()

    println ("""movieStar.σ (new KeyType ("Harrison_Ford"))""")
    movieStar.select (new KeyType ("Harrison_Ford")).show ()
    movieStar.σ (new KeyType ("Harrison_Ford")).show ()

    banner ("""movie ⋃ cinema""")
    (movie union cinema).show ()
    (movie ⋃ cinema).show ()

    banner ("""movie - cinema""")
    (movie minus cinema).show ()
    (movie - cinema).show ()

    banner ("""movie ⋂ cinem""")
    (movie intersect cinema).show ()
    (movie ⋂ cinema).show ()

    banner ("""movie × studio""")
    (movie product studio).show ()
    (movie × studio).show ()

    banner ("""movie.⋈ ((t, u) => t(movie.on("studioName")) == u(studio.on("name")), studio)""")
    movie.join ((t, u) => t(movie.on("studioName")) == u(studio.on("name")), studio).show ()
    movie.⋈ ((t, u) => t(movie.on("studioName")) == u(studio.on("name")), studio).show ()

    banner ("""movie.⋈ ("studioName", "name", studio)""")
    movie.join (Array ("studioName"), Array ("name"), studio).show ()
    movie.join ("studioName", "name", studio).show ()
    movie.⋈ ("studioName", "name", studio).show ()

    studio.create_index ()
    studio.show_index ()
    banner ("""movie.⋈ ("name", studio)""")
    studio.join ("studioName", movie).show ()                    // FIX - fails
//  movie.⋈ ("name", studio).show ()

    banner ("""movie.⋈ ("year", <, "year", cinema)""")
    movie.join ("year < year", cinema).show ()
    movie.⋈ ("year < year", cinema).show ()

    banner ("""movie ⋈ cinema""")
    (movie join cinema).show ()
    (movie ⋈ cinema).show ()

    banner ("""movie.↑ ("year")""")
    movie.orderBy ("year").show ()
    movie.↑ ("year").show ()

    banner ("""movie.↑ ("year")""")
    movie.orderByDesc ("year").show ()
    movie.↓ ("year").show ()

    banner ("""movie.delete (t => t(movie.on("year")) < 1980)""")
    movie.delete (t => t(movie.on("year")) < 1980)
    movie.show ()

    banner ("""movie.toMatrix ()""")
    println (s"matrix = ${movie.toMatrix ()}")

end movieDB

