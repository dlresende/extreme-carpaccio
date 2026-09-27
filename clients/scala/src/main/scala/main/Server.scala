package main

import cats.effect.{ExitCode, IO, IOApp}
import com.comcast.ip4s.{Port, host, port}
import org.http4s.ember.server.EmberServerBuilder

object Server extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {
    val serverPort = sys.env.get("PORT").flatMap(Port.fromString(_)).getOrElse(port"3000")

    EmberServerBuilder
      .default[IO]
      .withHost(host"0.0.0.0")
      .withPort(serverPort)
      .withHttpApp(Routes.http.orNotFound)
      .build
      .use(server => IO.println(s"Scala client listening on ${server.address}") *> IO.never)
      .as(ExitCode.Success)
  }
}
