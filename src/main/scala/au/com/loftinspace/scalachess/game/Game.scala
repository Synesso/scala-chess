package au.com.loftinspace.scalachess.game

import scala.util.matching.Regex

class Game {
  private val CoordinatePattern = new Regex("^[a-h][1-8]$")

  private val pieces: Array[Option[Piece]] = new Array[Option[Piece]](64).map((op: Option[Piece]) => None)

  def place(p: Piece) = new Placement(p)
  case class Placement(p: Piece) {
    def at(s: Symbol): Option[Piece] = {
      validateCoordinate(s)
      val index = arrayIndexForSymbol(s)
      val oldPiece = pieces(index)
      val oldIndex = pieces.findIndexOf((anOption: Option[Piece]) => anOption equals Some(p))
      if (oldIndex >= 0) pieces(oldIndex) = None
      pieces(index) = Some(p)
      oldPiece
    }
  }

  def pieceAt(s: Symbol):Option[Piece] = {
    validateCoordinate(s)
    pieces(arrayIndexForSymbol(s))
  }

  private def validateCoordinate(coord: Symbol) = if (coord == null || CoordinatePattern.findFirstIn(coord.name).equals(None)) throw new IllegalArgumentException("Invalid coordinate: " + coord)
  private def arrayIndexForSymbol(s: Symbol): Int = ((s.name.charAt(0).asDigit - 10) * 8) + (s.name.charAt(1).asDigit - 1)

  // todo - get rid of this!
  def includeIn_:[P <: Piece](piece: P): Unit = {}
}
