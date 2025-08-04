import sbt._

lazy val foundationsScala = (project in file(".")).settings(
  name := "foundations",
  organization := "net.degoes",
  version := "0.1-SNAPSHOT"
)

addCommandAlias("fmt", "all scalafmtSbt scalafmt test:scalafmt")
addCommandAlias(
  "check",
  "all scalafmtSbtCheck scalafmtCheck test:scalafmtCheck"
)

/* scala versions and options */
scalaVersion := "2.13.14"

// These options will be used for *all* versions.
scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-encoding",
  "UTF-8",
  "-Xlint",
  "-Xverify",
  "-feature",
  "-language:_"
)

javacOptions ++= Seq("-Xlint:unchecked", "-Xlint:deprecation", "-source", "1.8", "-target", "1.8")

val ZIOVersion = "2.1.10"

libraryDependencies ++= Seq(
  "dev.zio" %% "zio"              % ZIOVersion,
  "dev.zio" %% "zio-streams"      % ZIOVersion,
  "dev.zio" %% "zio-prelude"      % "1.0.0-RC23",
  "dev.zio" %% "zio-test"         % ZIOVersion % "test",
  "dev.zio" %% "zio-test-sbt"     % ZIOVersion % "test"
)

testFrameworks := Seq(new TestFramework("zio.test.sbt.ZTestFramework"))
