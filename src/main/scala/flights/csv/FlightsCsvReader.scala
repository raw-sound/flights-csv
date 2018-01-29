package flights.csv

import java.time.LocalDate

import flights.Flight

import scala.io.Source

object FlightsCsvReader {

  /**
    * Reads flight data from agreed csv format like the one below
    *
    *  "YEAR","QUARTER","MONTH","DAY_OF_MONTH","DAY_OF_WEEK","FL_DATE","ORIGIN","DEST",
    *   2014,1,1,1,3,2014-01-01,"JFK","LAX",
    *   2014,1,1,5,7,2014-01-05,"JFK","KBP",
    *   2014,1,1,6,1,2014-01-06,"KBP","LAX",
    *   2014,1,1,8,3,2014-01-08,"JFK","LAX",
    *   2014,1,1,12,7,2014-01-12,"JFK","KBP",
    *   2014,1,1,13,1,2014-01-13,"KBP","LAX",
    *
    * @param source source of csv data. The source MUST have the csv headers, implicit datatypes
    *               and quotation as in the example above
    * @return List of all the data rows represented as #Flight
    */
  def readFlightData(source: Source): List[Flight] = {
    val csvData = readCsvWithHeaders(source)
    (0 until csvData.dataSize()).map(implicit i => {
      def fromCsv(header: String)(implicit index: Int): String = csvData.getData(header, index)

      def unquote(str: String): String = str.slice(1, str.length - 1)

      Flight(
        LocalDate.of(fromCsv("\"YEAR\"").toInt, fromCsv("\"MONTH\"").toInt, fromCsv("\"DAY_OF_MONTH\"").toInt),
        unquote(fromCsv("\"ORIGIN\"")),
        unquote(fromCsv("\"DEST\"")))
    }).toList
  }
}
