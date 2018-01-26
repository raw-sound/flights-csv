package flights

object FlightsStatistics {

  def arrivalsPerAirport(flights: Flights): Map[Airport, Int] = flights.groupBy(_.destination).mapValues(_.size)

  def allAirports(flights: Flights): Set[Airport] = flights.foldLeft(Set.empty[Airport])((set, flight) =>
    set + flight.origin + flight.destination)

}
