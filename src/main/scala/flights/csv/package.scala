package flights

import scala.io.Source

package object csv {

  def readCsvWithHeaders(source: Source) = {
    val lines = source.getLines().map(s => s.split(',')).toArray
    new CsvData(lines.head, lines.tail)
  }

  class CsvData(val headers: Array[String], val dataRows: Array[Array[String]]) {
    private[this] val headerIndexes = headers.zipWithIndex.foldLeft(Map.empty[String, Int]) ((m, si) => si match {
        case (s, i) => m + (s -> i)
      })
    def dataSize(): Int = dataRows.length
    def getData(header: String, index: Int): String =  dataRows(index)(headerIndexes(header))
  }
}
