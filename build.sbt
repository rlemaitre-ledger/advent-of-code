ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2-RC1"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code"
  )
libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Werror")

coverageHighlighting            := true
coverageFailOnMinimum           := false
coverageMinimumStmtTotal        := 50
coverageMinimumBranchTotal      := 50
coverageMinimumStmtPerPackage   := 50
coverageMinimumBranchPerPackage := 50
coverageMinimumStmtPerFile      := 50
coverageMinimumBranchPerFile    := 50
