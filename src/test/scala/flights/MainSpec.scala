package flights

import java.nio.file.{Files, Path, Paths}

import org.specs2.matcher.Matcher
import org.specs2.mutable
import org.specs2.mutable.Before

import scala.io.Source

trait MainContext extends Before {
  val copy = Paths.get(Main.SOURCE_FILE_NAME)
  val filesNamesToBeCreated =
    Main.ARRIVALS_PER_AIRPORT_FILE_NAME ::
      Main.ARRIVALS_DEPARTURES_DIFF_PER_AIRPORT_FILE_NAME ::
      Main.ARRIVALS_PER_WEEK_PER_AIRPORT_FILE_NAME :: Nil

  override def before: Any = {
    copy :: filesToBeCreated() foreach Files.deleteIfExists
    Files.copy(Paths.get("src").resolve("test").resolve("resources").resolve(Main.SOURCE_FILE_NAME), copy)
    Main.process()
  }

  def filesToBeCreated(): List[Path] = filesNamesToBeCreated.map(Paths.get(_))

  def readAllFromSource(source: Source) = try source.getLines().toIndexedSeq finally source.close()

  def readAll(fileName: String) = readAllFromSource(Source.fromFile(fileName))

}

class MainSpec extends mutable.Specification with MainContext {

  def fileExsists: Matcher[Path] = { path: Path =>
    (Files.exists(path), path.toString + " exists", path.toString + " does not exist")
  }

  /**
    * e2e tests
    */
  "Main" should {

    "create all output files in current dir" in {
      filesToBeCreated must contain(fileExsists).foreach
    }

    s"number of lines in ${Main.ARRIVALS_PER_AIRPORT_FILE_NAME} must be equal to number of all airports in csv" in {
      Files.readAllLines(Paths.get(Main.ARRIVALS_PER_AIRPORT_FILE_NAME)) must have size 3
    }

    s"number of lines in ${Main.ARRIVALS_DEPARTURES_DIFF_PER_AIRPORT_FILE_NAME} must be equal to " +
      "the number of airports with non-zero diff in csv" in {
      Files.readAllLines(Paths.get(Main.ARRIVALS_DEPARTURES_DIFF_PER_AIRPORT_FILE_NAME)) must have size 2
    }

    s"${Main.ARRIVALS_PER_AIRPORT_FILE_NAME} contents are expected" in {
      readAll(Main.ARRIVALS_PER_AIRPORT_FILE_NAME) ===
        "1. LAX 4" +:
          "2. KBP 2" +:
          "3. JFK 0" +:
          IndexedSeq.empty
    }

    s"${Main.ARRIVALS_DEPARTURES_DIFF_PER_AIRPORT_FILE_NAME} contents are expected" in {
      readAll(Main.ARRIVALS_DEPARTURES_DIFF_PER_AIRPORT_FILE_NAME) ===
        "1. JFK -4" +:
          "2. LAX 4" +:
          IndexedSeq.empty
    }

    s"${Main.ARRIVALS_PER_WEEK_PER_AIRPORT_FILE_NAME} contents are expected" in {
      readAll(Main.ARRIVALS_PER_WEEK_PER_AIRPORT_FILE_NAME) ===
        "W1" +:
          "    1. LAX 2" +:
          "    2. KBP 1" +:
          "    3. JFK 0" +:
          "W2" +:
          "    1. LAX 2" +:
          "    2. KBP 1" +:
          "    3. JFK 0" +:
          IndexedSeq.empty
    }

  }


}
