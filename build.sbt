ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code"
  )
libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
