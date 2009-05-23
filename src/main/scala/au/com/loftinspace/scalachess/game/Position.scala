package au.com.loftinspace.scalachess.game

import scala.util.matching.Regex
import Math.abs

object Positioning {
  def position(s: Symbol): Position = position(coordinateToTuple(s))
  def position(rankAndFile: Tuple2[Int, Int]): Position = Position(rankAndFile._1, rankAndFile._2)

  private val CoordinatePattern = new Regex("^[a-h][1-8]$")

  private def coordinateToTuple(s: Symbol) = {
    if (s == null || CoordinatePattern.findFirstIn(s.name).equals(None)) throw new IllegalArgumentException("Invalid coordinate: " + s)
    ((s.name.charAt(1).asDigit), (s.name.charAt(0).asDigit - 9))
  }
}

case class Position(rank: Int, file: Int) {
  def ^(n: Int): Option[Position] = if (rank + n <= 8) Some(Position(rank + n, file)) else None
  def >(n: Int): Option[Position] = if (file + n <= 8) Some(Position(rank, file + n)) else None
  def v(n: Int): Option[Position] = if (rank - n >= 1) Some(Position(rank - n, file)) else None
  def <(n: Int): Option[Position] = if (file - n >= 1) Some(Position(rank, file - n)) else None
  def -?(other: Position) = rank == other.rank
  def |?(other: Position) = file == other.file
  def /?(other: Position) = abs(file - other.file) == abs(rank - other.rank)
  def fileAsLetter = (96 + file).toChar.toString
  override def toString = fileAsLetter + rank
}
