val scala3Version = "3.5.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "url-shortening",
    version := "0.1.0-SNAPSHOT",
    run / fork := true,

    scalaVersion := scala3Version,
    assembly / assemblyJarName := "url-shortening.jar",

    libraryDependencies ++= Seq(
      "org.apache.cassandra" % "java-driver-core" % "4.18.1",
      "org.postgresql" % "postgresql" % "42.7.4",
      "commons-codec" % "commons-codec" % "1.17.1",
      "io.seruco.encoding" % "base62" % "0.1.3",
      "com.lihaoyi" %% "scalatags" % "0.13.1",
      "com.lihaoyi" %% "cask" % "0.9.4",
      "io.lemonlabs" %% "scala-uri" % "4.0.3",
      "com.lihaoyi" %% "utest" % "0.8.4" % "test",
      ),

    testFrameworks += new TestFramework("utest.runner.Framework"),

    assembly / assemblyMergeStrategy := {
      case PathList("javax", "servlet", xs @ _*)         => MergeStrategy.first
      case PathList(ps @ _*) if ps.last endsWith ".html" => MergeStrategy.first
      case PathList(ps @ _*) if ps.last endsWith "StaticLoggerBinder.class" =>
        MergeStrategy.first
      case PathList(ps @ _*) if ps.last endsWith "StaticMDCBinder.class" =>
        MergeStrategy.first
      case PathList(ps @ _*) if ps.last endsWith "StaticMarkerBinder.class" =>
        MergeStrategy.first
      case PathList(ps @ _*) if ps.last endsWith "io.netty.versions.properties" =>
        MergeStrategy.first
      case PathList(ps @ _*) if ps.last endsWith "BUILD" => MergeStrategy.first
      case PathList(ps @ _*) if ps.last endsWith "module-info.class" => MergeStrategy.discard
      case "application.conf"                            => MergeStrategy.concat
      case x =>
        val oldStrategy = (assembly / assemblyMergeStrategy).value
        oldStrategy(x)
    },
  
  )
