package main

object DiscountManager {

  def discount(amount: Float): Option[Float] =
    amount match {
      case p if p < 0 => None
      case p if p >= 50000 => Some(amount - amount * 0.15f)
      case p if p >= 10000 => Some(amount - amount * 0.10f)
      case p if p >= 7000 => Some(amount - amount * 0.07f)
      case p if p >= 5000 => Some(amount - amount * 0.05f)
      case p if p >= 1000 => Some(amount - amount * 0.03f)
      case _ => Some(amount)
    }

}
