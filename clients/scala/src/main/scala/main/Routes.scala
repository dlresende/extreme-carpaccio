package main

import cats.effect.IO
import org.http4s.HttpRoutes
import org.http4s.dsl.io.*

object Routes {

  val http: HttpRoutes[IO] = HttpRoutes.of[IO] {
    case POST -> Root / "ping" => Ok("pong")
  }
}
