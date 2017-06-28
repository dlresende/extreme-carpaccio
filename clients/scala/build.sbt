import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      scalaVersion := "2.11.6",
      version      := "1.0"
    )),
    name := "scala",
    libraryDependencies += `spray-routing`,
    libraryDependencies += `spray-can`,
    libraryDependencies += `akka-actor`,
    libraryDependencies += `play-json`
  )
