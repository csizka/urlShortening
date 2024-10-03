val scala3Version = "3.3.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "url-shortening",
    version := "0.1.0-SNAPSHOT",
    run / fork := true,

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(

      "org.postgresql" % "postgresql" % "42.7.4",
      "commons-codec" % "commons-codec" % "1.17.1",
      "io.seruco.encoding" % "base62" % "0.1.3",
      "com.lihaoyi" %% "utest" % "0.8.4" % "test",
      "com.lihaoyi" %% "scalatags" % "0.13.1",
      "com.lihaoyi" %% "cask" % "0.9.4",
      "io.lemonlabs" %% "scala-uri" % "4.0.3",
      ),

    testFrameworks += new TestFramework("utest.runner.Framework")
  
  )
