package main

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.ContentTypes.`application/json`
import akka.http.scaladsl.model.HttpEntity
import akka.http.scaladsl.server.{HttpApp, Route}
import de.heikoseeberger.akkahttpcirce.FailFastCirceSupport._
import io.circe.generic.auto._

case class Order(prices: Seq[Float], quantities: Seq[Int], country: String)

case class Result(total: Float)

case class Feedback(`type`: String, content: String)

object Server extends HttpApp with App {

  type ProcessOrder = Order => Option[Float]

  lazy val process: ProcessOrder = {
    // TODO To implement
    order => Some(0.0f)
  }

  override def routes: Route =
    path("order") {
      post {
        entity(as[Order]) { order =>
          complete {
            println("Request received : " + order)

            val total = process(order)

            Result(total.getOrElse(0f))
          }
        }
      }
    } ~
      path("feedback") {
        post {
          entity(as[Feedback]) { feedback =>
            complete {
              println("Feedback received : " + feedback)
              ""
            }
          }
        }
      }

  startServer(host = "0.0.0.0", port = 9000)

}