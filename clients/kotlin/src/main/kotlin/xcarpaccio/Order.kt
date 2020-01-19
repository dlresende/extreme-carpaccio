package xcarpaccio

import kotlinx.serialization.Serializable

@Serializable
data class Order(val prices: List<Double>, val quantities: List<Double>, val country: String, val reduction: String) {

}