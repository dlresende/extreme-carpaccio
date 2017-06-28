import sbt._

object Dependencies {
  lazy val `spray-routing`: ModuleID = "io.spray" %% "spray-routing" % "1.3.2"
  lazy val `spray-can`: ModuleID = "io.spray" %% "spray-can" % "1.3.2"
  lazy val `akka-actor`: ModuleID = "com.typesafe.akka" %% "akka-actor" % "2.3.9"
  lazy val `play-json`: ModuleID = "com.typesafe.play" %% "play-json" % "2.4.0-M2"
}
