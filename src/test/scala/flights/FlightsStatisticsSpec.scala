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

  /**
    * [[flights.FlightsStatistics.arrivalsPerAirport()]] tests
    */
  "arrivalsPerAirport" should {
    "return empty map when no flights" in {
      FlightsStatistics.arrivalsPerAirport(noFlights) === Map.empty
    }
    "Given 2 arrivals to JFK, 1 arrival to KBP and one arrival to IEV" should {
      val arrivals = FlightsStatistics.arrivalsPerAirport(twoFlightsToJFKAndOneToKBPAndOneToIEV)
      "have 3 entries" in {
        arrivals.keySet.size === 3
      }
      "have 2 arrivals to JFK" in {
        arrivals("JFK") === 2
      }
      "have 1 arrival to KBP" in {
        arrivals("KBP") === 1
      }
      "have 1 arrival to IEV" in {
        arrivals("IEV") === 1
      }
    }
  }

  /**
    * [[flights.FlightsStatistics.inOutDifferencePerAirport()]] tests
    */
  "inOutDifferencePerAirport" should {
    "return empty map when no flights" in {
      FlightsStatistics.inOutDifferencePerAirport(noFlights) === Map.empty
    }

    val diffPerAirport = FlightsStatistics.inOutDifferencePerAirport(twoFlightsToJFKAndOneToKBPAndOneToIEV)
    "return number of outgoing flights negated when airport has no incoming" in {
      diffPerAirport("LAX") === -2
    }

    "not contain airport with zero difference" in {
      diffPerAirport.isDefinedAt("KBP") === false
    }

    "return number of incoming flights negated when airport has no outgoing" in {
      diffPerAirport("IEV") === 1
    }

    "return difference between incoming and outgoing flights" in {
      diffPerAirport("JFK") === 1
    }

  }

}


