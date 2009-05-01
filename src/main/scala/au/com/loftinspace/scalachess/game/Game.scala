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
      val toPosition: InPosition = position(s)
      val toIndex: Int = toPosition.asArrayIndex.get
      val fromIndex: Option[Int] = p.position.flatMap(anyPositionToArrayIndex)
      val captured: Option[Piece] = pieces(toIndex)
      captured.foreach{(cap: Piece) => cap.position = Some(Captured)}
      p.position = Some(toPosition)
      pieces(toIndex) = Some(p)
      fromIndex.foreach{(index: Int) => pieces(index) = None}
      captured
    }
  }

  private def anyPositionToArrayIndex(pos: Position) = {
    pos match {
      case InPosition(rank, file) => pos.asArrayIndex
      case _ => None
    }
  }

  def pieceAt(s: Symbol):Option[Piece] = pieces(position(s).asArrayIndex.get)

  def reset = {
    for (file <- 'a' to 'h';
         rank <- 1 to 8) {
      val coord = Symbol(file.toString + rank)
      file match {
        case 'a' => place(Piece(White, LineUp(rank - 1))) at coord
        case 'b' => place(Piece(White, Pawn)) at coord
        case 'g' => place(Piece(Black, Pawn)) at coord
        case 'h' => place(Piece(Black, LineUp(rank - 1))) at coord
        case _ => pieces(position(coord).asArrayIndex.get) = None
      }
    }
  }
}
