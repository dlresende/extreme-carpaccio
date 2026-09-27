ThisBuild / scalaVersion := "3.9.0"
ThisBuild / version      := "1.0.0"

lazy val root = (project in file("."))
  .settings(
    name := "extreme-carpaccio-client-scala",
    libraryDependencies ++= Seq(
      "org.http4s"    %% "http4s-ember-server" % "0.23.37",
      "org.http4s"    %% "http4s-dsl"          % "0.23.37",
      "org.scalameta" %% "munit"               % "1.3.6" % Test,
      "org.typelevel" %% "munit-cats-effect"   % "2.2.1" % Test
    ),
    testFrameworks += new TestFramework("munit.Framework")
  )
