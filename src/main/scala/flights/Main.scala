package flights

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.util.zip.GZIPInputStream

import flights.FlightsStatistics._
import flights.WriteUtil._
import flights.csv.FlightsCsvReader.readFlightData

import scala.io.Source

object Main {
  val SOURCE_FILE_NAME = "planes_log.csv.gz"
  val ARRIVALS_PER_AIRPORT_FILE_NAME = "arrivals.txt"
  val ARRIVALS_DEPARTURES_DIFF_PER_AIRPORT_FILE_NAME = "arrivals-departures-diff.txt"
  val ARRIVALS_PER_WEEK_PER_AIRPORT_FILE_NAME = "arrivals-per-week.txt"

  def main(args: Array[String]): Unit = {

    val flights = readCsvGz(Paths.get(SOURCE_FILE_NAME))
    val airports = allAirports(flights)

    def statsForAllAirports(stats: Airport => Int) =
      (for (airport <- airports) yield (airport, stats(airport))).toIndexedSeq


    writeArrivalsByAirportToFile()
    writeArrivalsDeparturesDiffByAirportToFile()
    writeArrivalsByAirportByWeekToFile()

    def statsForAllAirportsSorted(map: Map[Airport, Int]) = statsForAllAirports(map withDefaultValue 0)
      .sortBy(_._2)(Ordering.Int.reverse)

    implicit def toPath(string: String): Path = Paths.get(string)

    def writeArrivalsByAirportToFile(): Unit = {
      val arrivalsPerAirport = statsForAllAirportsSorted(arrivalsByAirport(flights))
      writeToFile(ARRIVALS_PER_AIRPORT_FILE_NAME) { implicit w => printWithIndex(arrivalsPerAirport) }
    }

    def writeArrivalsDeparturesDiffByAirportToFile(): Unit = {
      val arrivalDepartureDiff = inOutDifferencePerAirport(flights).toIndexedSeq.sortBy(_._2)
      writeToFile(ARRIVALS_DEPARTURES_DIFF_PER_AIRPORT_FILE_NAME) { implicit w => printWithIndex(arrivalDepartureDiff) }
    }

    def writeArrivalsByAirportByWeekToFile(): Unit = {
      val weeklyArrivals = arrivalsByAirportByWeek(flights).toIndexedSeq.sortBy(_._1)
      writeToFile(ARRIVALS_PER_WEEK_PER_AIRPORT_FILE_NAME) { implicit w =>
        weeklyArrivals.foreach { case (weekOfYear, stats) =>
          val indent = 4
          printWeek(weekOfYear)
          val arrivals = statsForAllAirportsSorted(stats)
          printWithIndex(arrivals, indent)
        }
      }
    }

  }

  def readCsvGz(path: Path): Flights = withResource(Source.fromInputStream(new GZIPInputStream(Files
    .newInputStream(path, StandardOpenOption.READ))))(readFlightData)
}
