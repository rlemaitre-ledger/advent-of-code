ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code"
  )
libraryDependencies += "org.scalameta" %% "munit"            % "0.7.29" % Test
libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test
scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature", "-Werror")

coverageHighlighting            := true
coverageFailOnMinimum           := true
coverageMinimumStmtTotal        := 80
coverageMinimumBranchTotal      := 80
coverageMinimumStmtPerPackage   := 80
coverageMinimumBranchPerPackage := 80
coverageMinimumStmtPerFile      := 80
coverageMinimumBranchPerFile    := 80
