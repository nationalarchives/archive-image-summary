import scala.io.Source
import scala.io.BufferedSource
import scala.util.matching.Regex
import scala.collection._
import com.github.tototoshi.csv._

object CountCPTifs extends App {

  def isTxtFile(file: String): Unit = {
    require(file.takeRight(4) == ".txt", s"Expected a .txt file but received '$file'. Please fix and try again!")
  }

  def getFileLines(txtFileName: String, txtFile:BufferedSource): Seq[String] = {
    val linesOfFile: Seq[String] = txtFile.getLines().toSeq
    require(linesOfFile != Seq(), s"The content of the file '$txtFileName' is empty. Please fix and try again!")
    linesOfFile
  }

  def findRelevantColumnsForCsv(fileLine: String):(String, String, String)  = {
    val relevantFileInfoPattern: Regex = "ENGLTNA1D\\_([a-zA-Z]+)(\\d+)\\-Box(\\d+).*$".r
    val relevantFileInfoMatches: Iterator[Regex.Match] = relevantFileInfoPattern.findAllIn(fileLine).matchData
    val seqOfGroupsOfRecordDetails: Seq[(String, String, String)] = relevantFileInfoMatches.map {
      recordDetails => (recordDetails.group(1),
                        recordDetails.group(2),
                        recordDetails.group(3))
    }.toSeq
    seqOfGroupsOfRecordDetails.head
  }

  def findRelevantTifs(fileLines: Seq[String]): Seq[(String, String, String)] = {
    fileLines.collect{
      case fileLine if(fileLine.takeRight(4) == ".tif") => findRelevantColumnsForCsv(fileLine)
    }
  }

  def generateTally(infoPerTifFile: Seq[(String, String, String)]): Map[(String, String, String), Int] = {
    val tifTally: mutable.Map[(String, String, String), Int] = mutable.Map()
//    val tifTally = infoPerTifFile.groupBy(info => info).view.mapValues {
//      recordList => recordList.size
//    }.toMap
    infoPerTifFile.foreach { tifInfo =>
      try {
        tifTally(tifInfo) += 1
      }
      catch {
        case _: NoSuchElementException => tifTally += tifInfo -> 1
      }
    }
    tifTally.toMap
  }

  def generateFinalCsv(csvFileName: String, tifTally: Map[(String, String, String), Int]) = {
    val writer = CSVWriter.open("test"+csvFileName)
    writer.writeRow(List("Dept", "Series", "RefPiece", "ImageFormat", "NumberOfImages"))

    for(((dept, series, pieceNumber), tally) <- tifTally){
      writer.writeRow(List(dept, series, pieceNumber, "TIFF", tally))
    }
    writer.close()

  }

  val txtFileName = "/home/nigel/Documents/Projects/Chelsea Pensioners/CP_062.txt"
  isTxtFile(txtFileName)

  val txtFile: BufferedSource = Source.fromFile(txtFileName)
  val fileLines: Seq[String] = getFileLines(txtFileName, txtFile)
  val recordDetailsPerTifFile: Seq[(String, String, String)] = findRelevantTifs(fileLines)

  val tifTally: Map[(String, String, String), Int] = generateTally(recordDetailsPerTifFile)

  val csvFileName: String = args(0).replace(".txt",".csv")
  generateFinalCsv(csvFileName, tifTally)

}
