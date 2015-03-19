package main

import akka.actor.ActorSystem
import play.api.libs.json._
import spray.routing.SimpleRoutingApp

case class Order(prices: Seq[Float], quantities: Seq[Int], country: String)

object Server extends App with SimpleRoutingApp {

  implicit val system = ActorSystem("my-system")

  type ProcessOrder = Order => Option[Float]

  val address = "localhost"
  val port: Int = 9000

  val orderRoute = "order"
  val feedbackRoute = "feedback"

  def applyTax(country: String, price: Float): Option[Float] = Country.fromName(country) match {
    case Some(c) => Some(price + price * c.tax)
    case _ => None
  }

  lazy val process: ProcessOrder = {
    order =>
      val total: Float = order.prices.zip(order.quantities).map(tuple => tuple._1 * tuple._2).sum

      for (withTax <- applyTax(order.country, total);
           withDiscount <- DiscountManager discount withTax
      ) yield withDiscount

  }

  startServer(interface = address, port = port) {

    println("Server started @ " + address + ":" + port)

    path(orderRoute) {
      post {
        entity(as[String]) {
          order =>
            complete {

              println("Request received : " + order)

              val json = Json.parse(order)

              val total = process(Order((json \ "prices").as[Seq[Float]], (json \ "quantities").as[Seq[Int]], (json \ "country").as[String])).getOrElse(0f)
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