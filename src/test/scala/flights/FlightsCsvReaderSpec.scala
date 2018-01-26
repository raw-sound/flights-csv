package flights

import java.time.Month

import flights.csv.FlightsCsvReader.readFlightData
import org.specs2.mutable._

import scala.io.Source

trait FlightDataContext extends Before {
  val singleDataRowCsv = "\"YEAR\",\"QUARTER\",\"MONTH\",\"DAY_OF_MONTH\",\"DAY_OF_WEEK\",\"FL_DATE\",\"ORIGIN\",\"DEST\",\n" +
    "2014,1,2,3,4,2014-06-07,\"JFK\",\"LAX\",\n"
  val multiFlightCsv = singleDataRowCsv +
    "2015,2,3,4,5,2015-08-09,\"KBP\",\"LAX\","
  val singletonFlightData = readFlightData(Source.fromString(singleDataRowCsv))
  val multiFlightData = readFlightData(Source.fromString(multiFlightCsv))

  override def before: Any = {}
}


class FlightsCsvReaderSpec extends Specification with FlightDataContext {

  val headerOnly = "\"YEAR\",\"QUARTER\",\"MONTH\",\"DAY_OF_MONTH\",\"DAY_OF_WEEK\",\"FL_DATE\",\"ORIGIN\",\"DEST\","


  "FlightsCsvReaderSpec" >> {
    "Given csv with headers and no data rows readFlightData returns empty list" >> {
      val flightData = readFlightData(Source.fromString(headerOnly))
      flightData.size === 0
    }
  }

  "Given csv with headers and 1 data row readFlightData" should {
    "return singleton list " in {
      singletonFlightData.size === 1
    }
    "FlightsData.flightDate has year equal to \"YEAR\" in csv" in {
      singletonFlightData.head.flightDate.getYear === 2014
    }
    "FlightsData.flightDate.month equal to \"MONTH\" in csv" in {
      singletonFlightData.head.flightDate.getMonth === Month.FEBRUARY
    }
    "FlightsData.flightDate.dayOfMonth equal to \"DAY_OF_MONTH\" in csv" in {
      singletonFlightData.head.flightDate.getDayOfMonth === 3
    }
    "FlightsData.origin equal to \"ORIGIN\" in csv" in {
      singletonFlightData.head.origin === "JFK"
    }
    "FlightsData.destination equal to \"DEST\" in csv" in {
      singletonFlightData.head.destination === "LAX"
    }
  }

  "Given csv with headers and 2 data rows readFlightData" should {
    "return list of length 2" in {
      multiFlightData.size === 2
    }
    "FlightsData.origin of the 2nd item equal to corresponding \"ORIGIN\" in csv" in {
      multiFlightData.tail.head.origin === "KBP"
    }

  }

}
