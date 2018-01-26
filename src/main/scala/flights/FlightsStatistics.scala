package flights

object FlightsStatistics {

  /**
    * Computes total arrival per airport.
    *
    * @param flights all flights
    * @return map of (airport -> total arrivals to that airport). Does NOT contain airports that have no incoming flights
    */
  def arrivalsPerAirport(flights: Flights): Map[Airport, Int] = flights.groupBy(_.destination).mapValues(_.size)

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
}
