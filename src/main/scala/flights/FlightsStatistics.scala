package flights

object FlightsStatistics {

  /**
    * Computes total arrival per airport.
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

}
