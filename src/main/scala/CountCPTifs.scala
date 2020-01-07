import scala.io.Source
import scala.io.BufferedSource
import scala.util.matching.Regex
import scala.collection._

object CountCPTifs extends App {

  def isTxtFile(file: String) = {
    require(file.takeRight(4) == ".txt", s"Expected a .txt file but received '$file'. Please fix and try again!")
  }

  def getFileLines(txtFileName: String, txtFile:BufferedSource): List[String] = {
    val linesOfFile = txtFile.getLines().toList
    require(linesOfFile != List(), s"The content of the file '$txtFileName' is empty. Please fix and try again!")
    linesOfFile
//        try {
//          print(linesOfFile.headOption.getOrElse(List))
//
//          } finally txtFile.close()

      }
  def findRelevantColumnsForCsv(fileLine: String)  = {
    val relevantFileInfoPattern: Regex = "ENGLTNA1D\\_([a-zA-Z]+)(\\d+)\\-Box(\\d+).*$".r
    val relevantFileInfoMatches = relevantFileInfoPattern.findAllIn(fileLine).matchData
    val listOfFileInfoGroups: Seq[(String, String, String)] = relevantFileInfoMatches.map {
      recordDetails => (recordDetails.group(1), recordDetails.group(2), recordDetails.group(3))
    }.toList
    println(listOfFileInfoGroups)
    listOfFileInfoGroups.head
  }

  def findRelevantTifs(fileLines: List[String]): List[(String, String, String)] = {
    fileLines.collect{
      case fileLine if(fileLine.takeRight(4) == ".tif") => findRelevantColumnsForCsv(fileLine)
    }
  }

  def generateTally(infoPerTifFile: List[(String, String, String)]): Map[(String, String, String), Int] = {
    val tifTally: mutable.Map[(String, String, String), Int] = mutable.Map()
    val i = infoPerTifFile.groupBy(info => info).view.mapValues(recordList => recordList.size ).toMap
    println(i)
//    infoPerTifFile.foreach { tifInfo =>
//      try {
//        tifTally(tifInfo) += 1
//      }
//      catch {
//        case _: NoSuchElementException => tifTally += tifInfo -> 1
//      }
//    }
//    tifTally
    i
  }

  def createCsv(fileName: String) = {
    val dept: String = ""
    val series: String = ""
  }

  val txtFileName = "/home/nigel/Documents/Projects/Chelsea Pensioners/CP_062.txt"
  isTxtFile(txtFileName)

  val txtFile = Source.fromFile(txtFileName)
  val fileLines = getFileLines(txtFileName, txtFile)
  val infoPerTifFile: List[(String, String, String)] = findRelevantTifs(fileLines)

  val tifTally: Map[(String, String, String), Int] = generateTally(infoPerTifFile)


//  val csvFileName: String = args(0).replace("txt","csv")
//  createCsv(csvFileName)

}
