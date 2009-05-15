package au.com.loftinspace.scalachess.game

import scala.util.matching.Regex
import Positioning._

class Game {
  private val CoordinatePattern = new Regex("^[a-h][1-8]$")
  private val LineUp = Rook :: Bishop :: Knight :: Queen :: King :: Knight :: Bishop :: Rook :: Nil

  private val pieces: Array[Option[Piece]] = new Array[Option[Piece]](64).map((op: Option[Piece]) => None)
  private var piecesCaptured: List[Piece] = Nil
  def capturedPieces = piecesCaptured

  def place(p: Piece) = new Placement(p)
  case class Placement(p: Piece) {
    def at(s: Symbol): Option[Piece] = {
      val destination: Position = position(s)
      p canMoveTo destination given Game.this
      val toIndex: Int = destination.asArrayIndex.get
      val fromIndex: Option[Int] = p.position.flatMap(_.asArrayIndex)
      val captured: Option[Piece] = pieces(toIndex)
      captured.foreach{(cap: Piece) => 
        cap.captured = true
        piecesCaptured = cap :: piecesCaptured
      }
      p.position = Some(destination)
      pieces(toIndex) = Some(p)
      fromIndex.foreach{(index: Int) => pieces(index) = None}
      captured
    }
  }

  def findMovesFor(p: Piece): Set[Position] = {
    if (!p.isInPlay) return Set()
    val position = p.position.get
println("It's a " + p)
    p match {
      case Piece(colour, role) => {
        role match {
          case Pawn => {
            val moveForwardBy = position.^_ // if (colour.equals(White)) position.^_ else position.v_
            Set(moveForwardBy(1), moveForwardBy(2)).filter(optP => optP.isDefined).map(_.get)
          }
          case _ => Set()
        }
      }
    }
  }

  def pieceAt(s: Symbol):Option[Piece] = pieces(position(s).asArrayIndex.get)
  def pieceAt(rank: Int, file: Int):Option[Piece] = pieces((rank - 1) + (file - 1) * 8)

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
