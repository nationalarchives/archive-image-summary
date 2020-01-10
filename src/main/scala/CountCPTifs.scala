import scala.io.Source
import scala.io.BufferedSource
import scala.util.matching.Regex
import scala.collection._
import com.github.tototoshi.csv._
import java.io.File


object CountCPTifs extends App {

  def checkForAFileArgument(): String = {
    try {
      args(0)
    }
    catch {
      case _: ArrayIndexOutOfBoundsException => throw new ArrayIndexOutOfBoundsException("ERROR: You did not provide an argument!")
    }
  }

  def checkIfArgumentIsADirectory(commandlineArg: String): File = {
    val argument: File = new File(commandlineArg)
    require(argument.isDirectory, "ERROR: The argument provided was not a directory!")
    argument
  }

  def getTxtFilesInDirectory(directory: File): Seq[File] = {
    val seqOfTxtFiles: Seq[File] = directory.listFiles.filter {
      item => item.isFile && item.toString.takeRight(4) == ".txt"
    }.toSeq
    require(seqOfTxtFiles != Seq(), "ERROR: The folder provided does not contain any .txt files in it!")
    seqOfTxtFiles
  }

  def getFileLines(txtFile: File): Seq[String] = {
    val bufferedTxtFile: BufferedSource = Source.fromFile(txtFile)
    val linesOfFile: Seq[String] = bufferedTxtFile.getLines().toSeq //
    require(linesOfFile != Seq(), s"ERROR: The content of the file '$txtFile' is empty. Please fix and try again!")
    bufferedTxtFile.close()
    linesOfFile
  }

  def findRelevantTextForEachColumn(txtFileName: String, fileLine: String): (String, String, String, String)  = {
    val relevantFileInfoPattern: Regex = "ENGLTNA1D\\_([a-zA-Z]+)(\\d+)\\-Box(\\d+).*$".r // find record details
    val relevantFileInfoMatches: Iterator[Regex.Match] = relevantFileInfoPattern.findAllIn(fileLine).matchData
    val seqOfGroupsOfRecordDetails: Seq[(String, String, String, String)] = relevantFileInfoMatches.map {
      recordDetails => (txtFileName,
                        recordDetails.group(1), // department
                        recordDetails.group(2), // series
                        recordDetails.group(3)) // piece
    }.toSeq
    seqOfGroupsOfRecordDetails.head // take tuple out of the Seq
  }

  def findLinesEndingWithTif(txtFileName: String, fileLines: Seq[String]): Seq[(String, String, String, String)] = {
    val cleanedFileLines = fileLines.map{ // check in case '\par' has been added to the end of the lines
      fileLine => if(fileLine.takeRight(4) == "\\par") fileLine.dropRight(4) else fileLine
    }
    val tifLines = cleanedFileLines.collect{// get details for only tifs
      case fileLine if(fileLine.takeRight(4) == ".tif") => findRelevantTextForEachColumn(txtFileName, fileLine)
    }
    val percentageOfTifLines = (tifLines.size.toFloat / fileLines.size.toFloat) * 100

    if(percentageOfTifLines < 99.00) { // check in case no tif lines were found
      println(s"WARNING: The % of lines ending in .tif in '${txtFileName}' is ${percentageOfTifLines.toInt}% which is lower than the expected 99%")
    }
    tifLines
  }

  def generateTifTally(infoPerTifFile: Seq[(String, String, String, String)]): Map[(String, String, String, String), Int] = {
    val tifTally = infoPerTifFile.groupBy(recordDetail => recordDetail).view.mapValues { // group duplicate details
      recordList => recordList.size // count number of record details duplicates
    }.toMap

    tifTally
  }

  def generateFinalCsv(csvFileName: String, tifTally: Map[(String, String, String, String), Int]): String = {
    val writer = CSVWriter.open(s"$csvFileName.csv")
    writer.writeRow(List("File Name", "Dept", "Series", "RefPiece", "ImageFormat", "NumberOfImages"))

    for(((fileName, dept, series, pieceNumber), numOfTifs) <- tifTally) {
      writer.writeRow(List(fileName, dept, series, pieceNumber, "TIFF", numOfTifs))
    }

//    tifTally.foreach {
//      case ((fileName, dept, series, pieceNumber), numOfTifs) => writer.writeRow(
//          List(fileName, dept, series, pieceNumber, "TIFF", numOfTifs)
//      )
//    }
    
    writer.close()
    s"\nTASK COMPLETED!! A CSV named '$csvFileName.csv' has been created for you in ${System.getProperty("user.dir")}!"
  }
  println(s"\nStarting task, please wait...")
  val argument: String = checkForAFileArgument() // check to see if argument was provided
  val directory: File = checkIfArgumentIsADirectory(argument)
  val seqOfTxtFilePaths: Seq[File] = getTxtFilesInDirectory(directory)

  val tifTallyPerTxtFile: Seq[Map[(String, String, String, String), Int]] = seqOfTxtFilePaths.map{ txtFilePath =>
    val fileLines: Seq[String] = getFileLines(txtFilePath)
    val txtFile = txtFilePath.getName
    val recordDetailsPerTifFile: Seq[(String, String, String, String)] = findLinesEndingWithTif(txtFile, fileLines)
    val tifTally: Map[(String, String, String, String), Int] = generateTifTally(recordDetailsPerTifFile)
    tifTally
  }

  val tifTallyForAllFiles: Predef.Map[(String, String, String, String), Int] = tifTallyPerTxtFile.flatten.toMap
  val sortedTifTallyForAllFiles = immutable.ListMap(tifTallyForAllFiles.toSeq.sortBy(_._1):_*)
  val csvFileName: String = "CP_tif_File_Count"
  val completionMessage: String = generateFinalCsv(csvFileName, sortedTifTallyForAllFiles)
  println(completionMessage)
}

//CODE to show Suzanne

//val tifTallyPerTxtFile: Seq[Map[(String, String, String, String), Int]] = for {
//  txtFilePath <- seqOfTxtFilePaths
//  fileLines: Seq[String] = getFileLines(txtFilePath)
//  val txtFile = txtFilePath.getName
//  recordDetailsPerTifFile: Seq[(String, String, String, String)] = findLinesEndingWithTif(txtFile, fileLines)
//  tifTally: Map[(String, String, String, String), Int] = generateTifTally(recordDetailsPerTifFile)
//} yield tifTally