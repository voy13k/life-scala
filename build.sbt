ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

val stVer = "3.2.18"

lazy val root = (project in file("."))
  .settings(
    name := "life-scala",
    libraryDependencies += "org.scalactic" %% "scalactic" % stVer,
    libraryDependencies += "org.scalatest" %% "scalatest" % stVer % Test,
    libraryDependencies += "org.scala-lang" %% "toolkit" % "0.4.0"
  )
