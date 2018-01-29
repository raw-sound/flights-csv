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
    Flight(LocalDate.of(2016, 1, 5), "LAX", "JFK"), //first flight. (Tuesday)
    Flight(LocalDate.of(2016, 1, 11), "LAX", "IEV"), //next monday
    Flight(LocalDate.of(2016, 1, 12), "LAX", "KBP"), //1st day of 2nd week
    Flight(LocalDate.of(2016, 1, 18), "JFK", "KBP"), //last day of 2nd week
    Flight(LocalDate.of(2016, 1, 19), "JFK", "KBP"), //Monday of 3rd week
    Flight(LocalDate.of(2016, 12, 31), "JFK", "KBP"), //Last day of year (week 52)
    Flight(LocalDate.of(2017, 1, 1), "IEV", "JFK"), //First day of next year (week 52)
    Flight(LocalDate.of(2017, 1, 3), "IEV", "JFK"), //First tuesday of next year (week 53)
  )
  /**
    * [[flights.FlightsStatistics.arrivalsByAirportByWeek()]] tests
    */
  "arrivalsPerAirportByWeek" should {

    val byWeek = FlightsStatistics.arrivalsByAirportByWeek(weeklyArrivals)

    "return empty Map when no flights" in {
      FlightsStatistics.arrivalsByAirportByWeek(noFlights) === Map.empty
    }

    "1st week include first flight and flight on {FIRST FLIGHT DATE} + {6 DAYS} " in {
      byWeek(1) === Map("JFK" -> 1, "IEV" -> 1)
    }

    "2nd week include flight on {FIRST FLIGHT DATE} + {7 DAYS} and flight on {FIRST FLIGHT DATE} + {14 DAYS}" in {
      byWeek(2) === Map("KBP" -> 2)
    }

    "include and first day of years" in {
      byWeek(52) === Map("KBP" -> 1, "JFK" -> 1)
    }

    "week counting continues when year changes" in {
      byWeek(53) === Map("JFK" -> 1)
    }

    "contain only weeks that have any flights" in {
      byWeek.keySet === Set(1,2,3,52,53)
    }
  }

}


