import java.time.LocalDate

package object flights {
  type Airport = String
  case class Flight(
                     flightDate: LocalDate,
                     origin: Airport,
                     destination: Airport
                   )
  type Flights =  List[Flight]
}
