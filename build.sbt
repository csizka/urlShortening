val scala3Version = "3.3.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "url-shortening",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.postgresql" % "postgresql" % "42.7.4",
      "commons-codec" % "commons-codec" % "1.17.1",
      "io.seruco.encoding" % "base62" % "0.1.3"
    )
  )
