import org.typelevel.scalacoptions.ScalacOptions

organization := "guycastle"
name := "aoc"
scalaVersion := "3.5.2"

val scalaTestVersion = "3.2.19"

lazy val aoc = (project in file("."))
  .settings(settings)
  .settings(
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % scalaTestVersion,
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
