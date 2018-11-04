import errors.Try
import errors.Option

val a = "112".toInt


def parseInsuranceRateQuote(age: String, numberOfSpeedingTickers: String): Option[Double] = {
  val optAge: Option[Int] = Try.Try(age.toInt)
  val optTickets: Option[Int] = Try.Try(numberOfSpeedingTickers.toInt)
  Try.map2bookFor(optAge,optTickets)(insuranceRateQuote)
}

def insuranceRateQuote(age: Int, numberOfSpeedingTickets: Int): Double = {
  println("Age"+age+"number"+numberOfSpeedingTickets)
  (age * numberOfSpeedingTickets) + 1000.0
}

//def insuranceRateQuoteEither(age: Int, numberOfSpeedingTickets: Int): Either[Exception,Double] = {
//  println("Age"+age+"number"+numberOfSpeedingTickets)
//  Right((age * numberOfSpeedingTickets) + 1000.0)
//}
//
val quote = parseInsuranceRateQuote("30","3")
//
//def parseInsuranceRateQuoteEither(
//                             age: String,
//                             numberOfSpeedingTickets: String): Either[Exception,Double] =
//  for {
//    a <- Either{ age.toInt }
//    tickets <- Either.Try { numberOfSpeedingTickets.toInt }
//  } yield insuranceRateQuoteEither(a, tickets)
//
//val quote2 = parseInsuranceRateQuoteEither("45","7")