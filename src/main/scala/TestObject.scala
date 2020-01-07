import scala.util.matching.Regex

object TestObject {

  val fileLine = raw"ENGLTNA1D_WO97-Box0220/ENGLTNA1D_WO97-Box0220-000_001.tif"
  val relevantFileInfoPattern: Regex = "ENGLTNA1D\\_([a-zA-Z]+)(\\d+)\\-Box(\\d+)".r
  val relevantFileInfo = relevantFileInfoPattern.findAllIn(fileLine)
  println(relevantFileInfo.matchData)
  //println(r.toList)

}
