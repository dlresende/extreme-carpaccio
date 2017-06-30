import sbt._

object Dependencies {
  lazy val `akka-http`: ModuleID = "com.typesafe.akka" %% "akka-http" % "10.0.9"
  lazy val `akka-http-circe`: ModuleID = "de.heikoseeberger" %% "akka-http-circe" % "1.16.1"
  lazy val `circe-generic`: ModuleID = "io.circe" %% "circe-generic" % "0.8.0"
}
