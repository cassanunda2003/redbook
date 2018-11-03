import errors.Try

val a = "112".toInt
val b = "hello".toInt

def parseInsuranceRateQuote(age: String, numberOfSpeedingTickers: String): Option[Double] = {
  val optAge: Option[Int] = Try(age.toInt)
  val optTickets: Option[Int] = Try(age.toInt)
}

def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
  age * numberOfSpeedingTickets + 1000.0
}

