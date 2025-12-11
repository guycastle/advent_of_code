import org.typelevel.scalacoptions.ScalacOptions

organization := "guycastle"
name := "aoc"
scalaVersion := "3.7.4"

val scalaTestVersion = "3.2.19"
val scalaNlpVersion = "2.1.0"

lazy val aoc = (project in file("."))
  .settings(settings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
    ),
  )

lazy val settings: Seq[Setting[?]] = Seq(
  // Dynamic versioning
  ThisBuild / dynverVTagPrefix := false,
  ThisBuild / dynverSeparator := "-",
  // Scalafix
  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
  // Disable warnings for scala tests
  Test / tpolecatExcludeOptions += ScalacOptions.warnNonUnitStatement,
)
