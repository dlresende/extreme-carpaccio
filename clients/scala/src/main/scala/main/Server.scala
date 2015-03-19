package main

import akka.actor.ActorSystem
import spray.routing.SimpleRoutingApp

case class Order(prices: Seq[Float], quantities: Seq[Int], country: String)

object Server extends App with SimpleRoutingApp {

  implicit val system = ActorSystem("my-system")

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

  startServer(interface = address, port = port) {

    println("Server started @ " + address + ":" + port)

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

  }
}