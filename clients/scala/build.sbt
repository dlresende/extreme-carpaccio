import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      scalaVersion := "2.12.2",
      version      := "1.0"
    )),
    name := "scala",
    libraryDependencies += `akka-http`,
    libraryDependencies += `akka-http-circe`,
    libraryDependencies += `circe-generic`
  )
