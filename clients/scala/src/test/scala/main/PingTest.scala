package main

import cats.effect.IO
import munit.CatsEffectSuite
import org.http4s.*
import org.http4s.implicits.*

class PingTest extends CatsEffectSuite {

  private val app: HttpApp[IO] = Routes.http.orNotFound

  test("POST /ping responds with pong") {
    val request = Request[IO](Method.POST, uri"/ping")

    app.run(request).flatMap { response =>
      assertEquals(response.status, Status.Ok)
      response.as[String].map(body => assertEquals(body, "pong"))
    }
  }
}
