package flights

import java.nio.file.{Files, Path, Paths, StandardOpenOption}
import java.util.zip.GZIPInputStream

import flights.FlightsStatistics._
import flights.WriteUtil._
import flights.csv.FlightsCsvReader.readFlightData

import scala.collection.immutable
import scala.io.Source

/**
  * Reads csv data from [[flights.Main.SOURCE_FILE_NAME]] file and writes the following data to files:
  *
  * 1. List of all airports with total number of planes for the whole period that arrived to each airport (to file [[flights.Main.ARRIVALS_PER_AIRPORT_FILE_NAME]])
  * 2. Non-Zero difference in total number of planes that arrived to and left from the airport (to file [[flights.Main.ARRIVALS_DEPARTURES_DIFF_PER_AIRPORT_FILE_NAME]])
  * 3. Does the point 1 but sum number of planes separately per each week (to file [[flights.Main.ARRIVALS_PER_WEEK_PER_AIRPORT_FILE_NAME]])
  *
  * If files already exist their contents are overridden
  *
  */
object Main {
  val SOURCE_FILE_NAME = "planes_log.csv.gz"
  val ARRIVALS_PER_AIRPORT_FILE_NAME = "arrivals.txt"
  val ARRIVALS_DEPARTURES_DIFF_PER_AIRPORT_FILE_NAME = "arrivals-departures-diff.txt"
  val ARRIVALS_PER_WEEK_PER_AIRPORT_FILE_NAME = "arrivals-per-week.txt"

  def main(args: Array[String]): Unit = {
    process()
  }

  def process(): Unit = {
    implicit def toPath(string: String): Path = Paths.get(string)
    val flights = readCsvGz(SOURCE_FILE_NAME)
    val airports = allAirports(flights)

    writeArrivalsByAirportToFile()
    writeArrivalsDeparturesDiffByAirportToFile()
    writeArrivalsByAirportByWeekToFile()

    def writeArrivalsByAirportToFile(): Unit = {
      val arrivalsPerAirport = statsForAllAirportsSortedByStatValueReversed(arrivalsByAirport(flights))
      writeToFile(ARRIVALS_PER_AIRPORT_FILE_NAME) { implicit w => printWithIndex(arrivalsPerAirport) }
    }

    def writeArrivalsDeparturesDiffByAirportToFile(): Unit = {
      val arrivalDepartureDiff = inOutDifferencePerAirport(flights).toIndexedSeq.sortBy(_.swap)
      writeToFile(ARRIVALS_DEPARTURES_DIFF_PER_AIRPORT_FILE_NAME) { implicit w => printWithIndex(arrivalDepartureDiff) }
    }

    def writeArrivalsByAirportByWeekToFile(): Unit = {
      val weeklyArrivals = arrivalsByAirportByWeek(flights).toIndexedSeq.sortBy(_._1)
      writeToFile(ARRIVALS_PER_WEEK_PER_AIRPORT_FILE_NAME) { implicit w =>
        weeklyArrivals.foreach { case (weekOfYear, stats) =>
          val indent = 4
          printWeek(weekOfYear)
          val arrivals = statsForAllAirportsSortedByStatValueReversed(stats)
          printWithIndex(arrivals, indent)
        }
      }
    }

    def statsForAllAirports(stats: Airport => Int): immutable.IndexedSeq[AirportStat] =
      (for (airport <- airports) yield (airport, stats(airport))).toIndexedSeq

    def statsForAllAirportsSortedByStatValueReversed(map: Map[Airport, Int]): immutable.IndexedSeq[AirportStat] =
      statsForAllAirports(map withDefaultValue 0)
        .sortBy{ case (airport, value) => (-value, airport)}

  }

  def readCsvGz(path: Path): Flights = withResource(Source.fromInputStream(new GZIPInputStream(Files
    .newInputStream(path, StandardOpenOption.READ))))(readFlightData)

}
