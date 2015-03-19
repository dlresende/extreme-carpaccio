package main

import scala.language.implicitConversions

object Country extends Enumeration {

  val BG = CountryVal("BG", 0.10f)
  val CZ = CountryVal("CZ", 0.11f)
  val DK = CountryVal("DK", 0.12f)
  val DE = CountryVal("DE", 0.13f)
  val EE = CountryVal("EE", 0.14f)
  val IE = CountryVal("IE", 0.15f)
  val EL = CountryVal("EL", 0.16f)
  val ES = CountryVal("ES", 0.17f)
  val FR = CountryVal("FR", 0.18f)
  val HR = CountryVal("HR", 0.19f)
  val IT = CountryVal("IT", 0.20f)
  val CY = CountryVal("CY", 0.10f)
  val LV = CountryVal("LV", 0.11f)
  val LT = CountryVal("LT", 0.12f)
  val LU = CountryVal("LU", 0.13f)
  val HU = CountryVal("HU", 0.14f)
  val MT = CountryVal("MT", 0.15f)
  val NL = CountryVal("NL", 0.16f)
  val AT = CountryVal("AT", 0.17f)
  val PL = CountryVal("PL", 0.18f)
  val PT = CountryVal("PT", 0.19f)
  val RO = CountryVal("RO", 0.20f)
  val SI = CountryVal("SI", 0.10f)
  val SK = CountryVal("SK", 0.11f)
  val FI = CountryVal("FI", 0.12f)
  val SE = CountryVal("SE", 0.13f)
  val UK = CountryVal("UK", 0.14f)

  def fromName(name: String): Option[CountryVal] = {
    try {
      Some(super.withName(name))
    } catch {
      case ex: NoSuchElementException => None
    }
  }

  protected case class CountryVal(name: String, tax: Float) extends super.Val()

  implicit def convert(value: Value): CountryVal = value.asInstanceOf[CountryVal]
}