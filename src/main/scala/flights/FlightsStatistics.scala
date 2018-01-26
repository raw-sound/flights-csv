package flights

import java.time.temporal.WeekFields

object FlightsStatistics {

  /**
    * Computes total arrivals per airport.
    *
    * @param flights all flights
    * @return map of (airport -> total arrivals to that airport). Does NOT contain airports that have no incoming flights
    */
  def arrivalsByAirport(flights: Flights): Map[Airport, Int] = flights.groupBy(_.destination).mapValues(_.size)

  /**
    * Collects all airports
    *
    * @param flights all flights
    * @return set of all airports that are either origin or destination in flights data
    */
  def allAirports(flights: Flights): Set[Airport] = flights.foldLeft(Set.empty[Airport])((set, flight) =>
    set + flight.origin + flight.destination)

  /**
    * Computes non-zero difference in total number of planes that arrived to and left from the airport for all airports
    *
    * @param flights all flights
    * @return map of (airport -> incoming - outgoing flights if it is not zero)
    */
  def inOutDifferencePerAirport(flights: Flights): Map[Airport, Int] = {
    def adjust[T](key: T, adjustment: Int)(map: Map[T, Int]) = {
      val newVal = (if (map.isDefinedAt(key)) map(key) else 0) + adjustment
      if (newVal == 0) map - key else map + (key -> newVal)
    }

    flights.foldLeft(Map.empty[Airport, Int])((map, flight) =>
      adjust(flight.destination, 1)(adjust(flight.origin, -1)(map)))
  }

  type WeekOfYear = Int

  /**
    * Computes total arrivals per airport per week of the year. The week is an ISO-8601 week of the year
    *
    * @param flights flights MUST have the same year
    * @return map of (week of the year -> map of (airport -> number of arrivals that week)).
    *         Map contains only weeks for which there is flights data
    */
  def arrivalsByAirportByWeek(flights: Flights): Map[WeekOfYear, Map[Airport, Int]] = flights
    .groupBy(_.flightDate.get(WeekFields.ISO.weekOfYear))
    .mapValues(arrivalsByAirport)
}
