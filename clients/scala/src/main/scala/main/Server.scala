package main

import akka.http.scaladsl.server.{HttpApp, Route}

case class Order(prices: Seq[Float], quantities: Seq[Int], country: String)

object Server extends HttpApp with App {

  type ProcessOrder = Order => Option[Float]

  // TODO Modify the server configuration if necessary
  val address = "localhost"
  val port: Int = 9000

  val orderRoute = "order"
  val feedbackRoute = "feedback"

  lazy val process: ProcessOrder = {
    // TODO To implement
    order => Some(0.0f)
  }

  override def routes: Route =
    path(orderRoute) {
      post {
        entity(as[String]) {
          order =>
            complete {

              println("Request received : " + order)

              // TODO To implement
              val total = 0

              s"""{
                 |"total": $total
                  }""".stripMargin
            }
        }
      }
    } ~
      path("feedback") {
        post {
          entity(as[String]) {
            feedback =>
              complete {
                println("Feedback received : " + feedback)
                ""
              }
          }
        }
      }

  startServer(host = address, port = port)
}