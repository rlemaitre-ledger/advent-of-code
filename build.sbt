ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.1"

lazy val root = (project in file("."))
  .settings(
    name := "advent-of-code",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.2.0",
      "com.monovore"           %% "decline"                  % "2.4.1",
      "io.github.iltotore"     %% "iron"                     % "2.3.0",
      "co.fs2"                 %% "fs2-core"                 % "3.9.3",
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
coverageFailOnMinimum           := false
coverageMinimumStmtTotal        := 50
coverageMinimumBranchTotal      := 50
coverageMinimumStmtPerPackage   := 50
coverageMinimumBranchPerPackage := 50
coverageMinimumStmtPerFile      := 50
coverageMinimumBranchPerFile    := 50
