val dottyVersion = "0.26.0-RC1"
val scala213Version = "2.13.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "dotty-cross",
    version := "0.1.0",

    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",

    // https://mvnrepository.com/artifact/dev.zio/zio
    libraryDependencies += "dev.zio" % "zio_2.13" % "1.0.0-RC21-2",

    libraryDependencies += "dev.zio" % "zio-streams_2.13" % "1.0.0-RC21-2",

    // To make the default compiler and REPL use Dotty
    scalaVersion := dottyVersion,

    // To cross compile with Dotty and Scala 2
    crossScalaVersions := Seq(dottyVersion, scala213Version)
  )
