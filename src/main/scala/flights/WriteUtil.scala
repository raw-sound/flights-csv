package flights

import java.io._
import java.nio.file.StandardOpenOption._
import java.nio.file.{Files, Path}

import flights.FlightsStatistics.WeekOfYear

object WriteUtil {

  type Indent = Int

  /**
    * Opens file and writes starting from start of file and flushes the writer. Creates the file if it does not exist.
    *
    * @param path to file
    * @param write what should be written
    */
  def writeToFile(path: Path)(write: Writer => Unit): Unit =
    withResource(Files.newBufferedWriter(path)) { w => {
      write(w)
      w.flush()
    }
    }

  def withResource[R <: AutoCloseable, A](resource: R)(code: R => A): A = {
    try
      code(resource)
    finally {
      resource.close()
    }
  }

  def printWithIndex(stats: IndexedSeq[AirportStat], indent: Indent = 0)(implicit writer: Writer): Unit = {
    stats.zipWithIndex.foreach { case (s, i) => printIndexedStat(s, i, indent) }
  }

  private[this] implicit class CoolWriter(writer: Writer) {
    def append(int: Int): Writer = writer.append(String.valueOf(int))
    def appendIndentation(indent: Indent = 0): Writer = {
      (1 to indent) foreach (_ => writer.append(' '))
      writer
    }
  }

  def printIndexedStat(airportStat: AirportStat, index: Int, indent: Indent = 0)(implicit writer: Writer): Unit = {
    airportStat match {
      case (airport, number) =>
        writer.appendIndentation(indent)
          .append(index + 1).append(". ")
          .append(airport).append(' ')
          .append(number).append('\n')
    }
  }

  def printWeek(weekOfYear: WeekOfYear, indent: Indent = 0)(implicit writer: Writer): Unit = {
    writer.appendIndentation(indent).append('W').append(weekOfYear).append('\n')
  }

}
