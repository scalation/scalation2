
// build.sbt - sbt used to compile and run Scala projects

lazy val scalation = project.in(file("."))
  .settings(
    scalaVersion  := "3.6.4",
    scalacOptions ++= Seq(
       "-deprecation",         // emit warning and location for usages of deprecated APIs
       "-explain",             // explain errors in more detail
       "-new-syntax",          // require `then` and `do` in control expressions.
       "-Wunused:all",         // warn of unused imports, ...
       "-Xfatal-warnings")     // fail the compilation if there are any warnings

  )

fork := true

// ScalaFx (2D and 3D Graphics)
// https://mvnrepository.com/artifact/org.scalafx/scalafx_3/22.0.0-R33
libraryDependencies += "org.scalafx" %% "scalafx" % "22.0.0-R33"

