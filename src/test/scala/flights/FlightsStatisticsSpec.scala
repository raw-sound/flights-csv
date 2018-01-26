package flights
import java.time.LocalDate

import org.specs2._
class FlightsStatisticsSpec extends mutable.Specification{

val noFlights: Flights = List.empty
val twoFlightsToJFKAndOneToKBPAndOneToIEV: Flights = List( //dates are arbitrary
  Flight(LocalDate.now(), "LAX", "JFK"),
  Flight(LocalDate.now().withDayOfMonth(1), "KBP", "JFK"),
  Flight(LocalDate.now().withDayOfYear(1), "JFK", "KBP"),
  Flight(LocalDate.now().withDayOfYear(42), "LAX", "IEV"))

  /**
    * [[flights.FlightsStatistics.allAirports()]] tests
    */
  "allAirports" should {
    "return empty set when no flights" in {
      FlightsStatistics.allAirports(noFlights) === Set.empty
    }

    "list all air airports" in {
      FlightsStatistics.allAirports(twoFlightsToJFKAndOneToKBPAndOneToIEV) === Set("LAX", "KBP", "JFK", "IEV")
    }
  }

}


