package au.com.loftinspace.scalachess.game

import scala.util.matching.Regex
import Positioning._

class Game {
  private val CoordinatePattern = new Regex("^[a-h][1-8]$")
  private val LineUp = Rook :: Bishop :: Knight :: Queen :: King :: Knight :: Bishop :: Rook :: Nil

  private val pieces: Array[Option[Piece]] = new Array[Option[Piece]](64).map((op: Option[Piece]) => None)

  def place(p: Piece) = new Placement(p)
  case class Placement(p: Piece) {
    def at(s: Symbol): Option[Piece] = {
      val index = position(s).asArrayIndex
      val oldPiece = pieces(index)
      if (oldPiece != None) { oldPiece.get.position = Captured }
      val oldIndex =  p.position match { case InPosition(rank, file) => file + rank * 8 }
      if (oldIndex >= 0) pieces(oldIndex) = None
      p.position = position(s)
      pieces(index) = Some(p)
      oldPiece
    }
  }

  def pieceAt(s: Symbol):Option[Piece] = pieces(position(s).asArrayIndex)

  def reset = {
    for (file <- 'a' to 'h';
         rank <- 1 to 8) {
      val coord = Symbol(file.toString + rank)
      file match {
        case 'a' => place(new Piece(White, LineUp(rank - 1))) at coord
        case 'b' => place(new Piece(White, Pawn)) at coord
        case 'g' => place(new Piece(Black, Pawn)) at coord
        case 'h' => place(new Piece(Black, LineUp(rank - 1))) at coord
        case _ => pieces(position(coord).asArrayIndex) = None
      }
    }
  }

  def debug = {
    pieces.foreach { piece => print(piece) }
  }
}
