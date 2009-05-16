package au.com.loftinspace.scalachess.game

import scala.collection.mutable.HashMap
import scala.util.matching.Regex
import Positioning._

class Game {
  private val CoordinatePattern = new Regex("^[a-h][1-8]$")
  private val LineUp = Rook :: Bishop :: Knight :: Queen :: King :: Knight :: Bishop :: Rook :: Nil

  private val pieces = new HashMap[Position, Option[Piece]]
  private var piecesCaptured: List[Piece] = Nil
  reset
  
  def capturedPieces = piecesCaptured

  def place(p: Piece) = new Placement(p)
  case class Placement(p: Piece) {
    def at(s: Symbol): Option[Piece] = {
      val destination: Position = position(s)
      val captured: Option[Piece] = pieces(destination)
      captured.foreach{(cap: Piece) => 
        cap.captured = true
        piecesCaptured = cap :: piecesCaptured
      }
      p.position.foreach{(origin: Position) => pieces(origin) = None}
      p.position = Some(destination)
      pieces(destination) = Some(p)
      captured
    }
  }

  def findMovesFor(p: Piece): Set[Position] = {
    println("working on ... " + p)
    if (!p.isInPlay) return Set()
    println("is in play")
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

  def pieceAt(p: Position) = pieces(p)
  def pieceAt(s: Symbol): Option[Piece] = pieceAt(position(s))
  def pieceAt(rank: Int, file: Int): Option[Piece] = pieceAt(Position(rank, file))

  def reset = {
    for (file <- 'a' to 'h';
         rank <- 1 to 8) {
      val coord = position(Symbol(file.toString + rank))
      rank match {
        case 1 => pieces(coord) = Some(Piece(White, LineUp(file - 'a')))
        case 2 => pieces(coord) = Some(Piece(White, Pawn))
        case 7 => pieces(coord) = Some(Piece(Black, Pawn))
        case 8 => pieces(coord) = Some(Piece(Black, LineUp(file - 'a')))
        case _ => pieces(coord) = None
      }
    }
  }

  def printout = {
    for (rank <- 8 to 1 by -1) {
      for (file <- 'a' to 'h') {
        print(pieceAt(Symbol(file.toString + rank)))
        print(' ')
      }
      println
    }
  }
}