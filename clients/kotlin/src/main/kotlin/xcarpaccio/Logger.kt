package xcarpaccio

class Logger {
    fun log(message: String) {
        println(message)
    }

    fun log(ex: Exception) {
        ex.printStackTrace()
    }

    fun error(message: String) {
        System.err.println(message)
    }
}
