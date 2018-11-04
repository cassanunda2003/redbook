import scala.util.Try

def insuranceRateQuoteEither(age: Int, numberOfSpeedingTickets: Int): Either[Exception,Double] = {
  println("Age"+age+"number"+numberOfSpeedingTickets)
  Right((age * numberOfSpeedingTickets) + 1000.0)
}


def parseInsuranceRateQuoteEither(
                             age: String,
                             numberOfSpeedingTickets: String): Try[Either[Exception,Double]] =
  for {
    a <- Try{ age.toInt }
    tickets <- Try  { numberOfSpeedingTickets.toInt }
  } yield insuranceRateQuoteEither(a, tickets)

val quote2 = parseInsuranceRateQuoteEither("45","7")