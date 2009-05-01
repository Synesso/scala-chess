package au.com.loftinspace.scalachess.game

import scala.util.matching.Regex

object Positioning {
  def position(s: Symbol): InPosition = position(arrayIndexForCoordinate(s))
  def position(rankAndFile: Tuple2[Int, Int]): InPosition = InPosition(rankAndFile._1, rankAndFile._2)
  def captured = Captured

  private val CoordinatePattern = new Regex("^[a-h][1-8]$")

  private def arrayIndexForCoordinate(s: Symbol) = {
    if (s == null || CoordinatePattern.findFirstIn(s.name).equals(None)) throw new IllegalArgumentException("Invalid coordinate: " + s)
    ((s.name.charAt(0).asDigit - 10), (s.name.charAt(1).asDigit - 1))
  }
}

abstract class Position {
  def asArrayIndex: Option[Int]
}
case object Captured extends Position {
  override def asArrayIndex: Option[Int] = None
}
case class InPosition(rank: Int, file: Int) extends Position {
  override def asArrayIndex: Option[Int] = Some(rank + (file * 8))
}

