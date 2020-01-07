import scala.io.Source
import scala.io.BufferedSource
import scala.util.matching.Regex
import scala.collection._
import com.github.tototoshi.csv._

object CountCPTifs extends App {

  def isATxtFile(file: String): Unit = {
    require(file.takeRight(4) == ".txt", s"Expected a .txt file but received '$file'. Please fix and try again!")
  }

  def getFileLines(txtFileName: String, txtFile:BufferedSource): Seq[String] = {
    val linesOfFile: Seq[String] = txtFile.getLines().toSeq //
    require(linesOfFile != Seq(), s"The content of the file '$txtFileName' is empty. Please fix and try again!")
    linesOfFile
  }

  def findRelevantColumnsForCsv(fileLine: String):(String, String, String)  = {
    val relevantFileInfoPattern: Regex = "ENGLTNA1D\\_([a-zA-Z]+)(\\d+)\\-Box(\\d+).*$".r // find record details
    val relevantFileInfoMatches: Iterator[Regex.Match] = relevantFileInfoPattern.findAllIn(fileLine).matchData
    val seqOfGroupsOfRecordDetails: Seq[(String, String, String)] = relevantFileInfoMatches.map {
      recordDetails => (recordDetails.group(1), // department
                        recordDetails.group(2), // series
                        recordDetails.group(3)) // piece
    }.toSeq
    seqOfGroupsOfRecordDetails.head // take tuple out of the Seq
  }

  def findRelevantTifs(fileLines: Seq[String]): Seq[(String, String, String)] = {
    fileLines.collect{
      case fileLine if(fileLine.takeRight(4) == ".tif") => findRelevantColumnsForCsv(fileLine) // get details of only tifs
    }
  }

  def generateTally(infoPerTifFile: Seq[(String, String, String)]): Map[(String, String, String), Int] = {
//    val tifTally: mutable.Map[(String, String, String), Int] = mutable.Map()
    val tifTally = infoPerTifFile.groupBy(recordDetail => recordDetail).view.mapValues { // group duplicate details
      recordList => recordList.size // count number of record details duplicates
    }.toMap

//    infoPerTifFile.foreach { tifInfo =>
//      try {
//        tifTally(tifInfo) += 1
//      }
//      catch {
//        case _: NoSuchElementException => tifTally += tifInfo -> 1
//      }
//    }
    tifTally
  }

  def generateFinalCsv(csvFileName: String, tifTally: Map[(String, String, String), Int]) = {
    val writer = CSVWriter.open(csvFileName)
    writer.writeRow(List("Dept", "Series", "RefPiece", "ImageFormat", "NumberOfImages"))

    for(((dept, series, pieceNumber), numOfTifs) <- tifTally){
      writer.writeRow(List(dept, series, pieceNumber, "TIFF", numOfTifs))
    }
    writer.close()

  }

  println(args(0))
  println("test")

  val txtFileName = args(0)
  isATxtFile(txtFileName)

  val txtFile: BufferedSource = Source.fromFile(txtFileName)
  val fileLines: Seq[String] = getFileLines(txtFileName, txtFile)
  val recordDetailsPerTifFile: Seq[(String, String, String)] = findRelevantTifs(fileLines)

  val tifTally: Map[(String, String, String), Int] = generateTally(recordDetailsPerTifFile)

  val csvFileName: String = txtFileName.replace(".txt",".csv")
  generateFinalCsv(csvFileName, tifTally)

}
