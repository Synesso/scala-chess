package au.com.loftinspace.scalachess.game

import scala.util.matching.Regex

class Game {
  private val CoordinatePattern = new Regex("^[a-h][1-8]$")

  private val pieces: Array[Option[Piece]] = new Array[Option[Piece]](64).map((op: Option[Piece]) => None)

  def place(p: Piece) = new Placement(p)
  case class Placement(p: Piece) {
    def at(s: Symbol): Option[Piece] = {
      val index = arrayIndexForCoordinate(s)
      val oldPiece = pieces(index)
      val oldIndex = pieces.findIndexOf((anOption: Option[Piece]) => anOption equals Some(p))
      if (oldIndex >= 0) pieces(oldIndex) = None
      pieces(index) = Some(p)
      oldPiece
    }
  }

  def pieceAt(s: Symbol):Option[Piece] = pieces(arrayIndexForCoordinate(s))

  private def arrayIndexForCoordinate(s: Symbol): Int = {
    if (s == null || CoordinatePattern.findFirstIn(s.name).equals(None)) throw new IllegalArgumentException("Invalid coordinate: " + s)
    ((s.name.charAt(0).asDigit - 10) * 8) + (s.name.charAt(1).asDigit - 1)
  }

  // todo - get rid of this!
  def includeIn_:[P <: Piece](piece: P): Unit = {}
}
