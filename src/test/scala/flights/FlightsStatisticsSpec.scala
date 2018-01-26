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
    * [[flights.FlightsStatistics.arrivalsByAirport()]] tests
    */
  "arrivalsPerAirport" should {
    "return empty map when no flights" in {
      FlightsStatistics.arrivalsByAirport(noFlights) === Map.empty
    }
    "Given 2 arrivals to JFK, 1 arrival to KBP and 1 arrival to IEV" should {
      val arrivals = FlightsStatistics.arrivalsByAirport(twoFlightsToJFKAndOneToKBPAndOneToIEV)
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

  val weeklyArrivals: Flights = List(
    Flight(LocalDate.of(2016, 1, 1), "LAX", "JFK"), //First day of week. The week is 0
    Flight(LocalDate.of(2016, 1, 3), "LAX", "IEV"), //Sunday of 0 week
    Flight(LocalDate.of(2016, 1, 4), "LAX", "KBP"), //Monday of 1st week
    Flight(LocalDate.of(2016, 1, 10), "JFK", "KBP"), //Sunday of 1st week
    Flight(LocalDate.of(2016, 1, 11), "JFK", "KBP"), //Monday of 2nd week
    Flight(LocalDate.of(2016, 12, 31), "JFK", "KBP"), //Last day of year (week 52)
  )
  /**
    * [[flights.FlightsStatistics.arrivalsByAirportByWeek()]] tests
    */
  "arrivalsPerAirportByWeek" should {
    val byWeek = FlightsStatistics.arrivalsByAirportByWeek(weeklyArrivals)
    "have week 0 for first days of year before first ISO-8601 week" in {
      byWeek.isDefinedAt(0)
    }

    "have flights up to Sunday of week 0 grouped in week 0" in {
      byWeek(0) === Map("JFK" -> 1, "IEV" -> 1)
    }

    "include flights from Mon till Sunday" in {
      byWeek(1) === Map("KBP" -> 2)
    }

    "include last day of year" in {
      byWeek(52) === Map("KBP" -> 1)
    }

    "contain only weeks that have any flights" in {
      byWeek.keySet === Set(0,1,2,52)
    }
  }

}


