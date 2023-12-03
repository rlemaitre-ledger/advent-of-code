ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code",
//    Compile / mainClass    := Some("AdventOfCode"),
//    run / mainClass        := Some("AdventOfCode"),
//    packageBin / mainClass := Some("AdventOfCode"),
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0",
      "com.monovore"           %% "decline"                  % "2.4.1",
      "org.scalameta"          %% "munit"                    % "0.7.29" % Test,
      "org.scalameta"          %% "munit-scalacheck"         % "0.7.29" % Test
    )
  )
  .enablePlugins(JavaAppPackaging, UniversalPlugin)
scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-explain",
  "-explain-types"
)

coverageHighlighting            := true
coverageFailOnMinimum           := true
coverageMinimumStmtTotal        := 80
coverageMinimumBranchTotal      := 80
coverageMinimumStmtPerPackage   := 80
coverageMinimumBranchPerPackage := 80
coverageMinimumStmtPerFile      := 80
coverageMinimumBranchPerFile    := 80

ThisBuild / coverageExcludedFiles := """.*AdventOfCode"""
