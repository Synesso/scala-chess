package au.com.loftinspace.scalachess.game

import scala.collection.immutable._
import scala.util.matching.Regex
import Positioning._

class Game {
  private val CoordinatePattern = new Regex("^[a-h][1-8]$")
  private val LineUp = Rook :: Knight :: Bishop :: Queen :: King :: Bishop :: Knight :: Rook :: Nil

  private var pieces: Map[Position, Option[Piece]] = new HashMap[Position, Option[Piece]]
  private var piecesCaptured: List[Piece] = Nil
  private var movesMade: List[Move] = Nil

  reset
  
  def capturedPieces = piecesCaptured

  def move(s: Symbol) = new Movement(pieceAt(s))
  case class Movement(p: Option[Piece]) {
    val piece = p.getOrElse(throw new IllegalMoveException("Cannot move a piece from an empty position"))
    def to(s: Symbol): Option[Piece] = {
      Game.this check piece canMoveTo s
      val from = piece.position.get
      piece.hasMoved = true
      var taken = Game.this place piece at s
      if (taken.isEmpty && (movesMade.lastOption.map(move => move.isPawnLaunch && move.traversed(s))).getOrElse(false)) {
        taken = Some(movesMade.last.piece)
        taken.get.captured = true
        piecesCaptured = taken.get :: piecesCaptured
      }
      movesMade = movesMade ::: List(Move(piece, from, position(s), taken))
      taken
    }
  }

  def check(p: Piece) = new PieceAssertion(p)
  case class PieceAssertion(p: Piece) {
    def canMoveTo(s: Symbol) = {
      val target = position(s)
      val validMoves = Game.this findMovesFor p
      if (!validMoves.contains(target))
      throw new IllegalMoveException(p + " cannot move from " + p.position + " to " + target)
    }
  }

  def place(p: Piece) = new Placement(p)
  case class Placement(p: Piece) {
    def at(s: Symbol): Option[Piece] = at(position(s))
    def at(destination: Position): Option[Piece] = {
      val captured: Option[Piece] = pieces(destination)
      captured.foreach{(cap: Piece) => 
        cap.captured = true
        piecesCaptured = cap :: piecesCaptured
      }
      p.position.foreach{(origin: Position) => pieces = pieces.update(origin, None)}
      p.position = Some(destination)
      pieces = pieces.update(destination, Some(p))
      captured
    }
  }

  def findMovesFor(p: Piece): Set[Position] = {
    p.movesWithinContext(pieces, moves.lastOption)
  }

  def pieceAt(p: Position) = pieces(p)
  def pieceAt(s: Symbol): Option[Piece] = pieceAt(position(s))
  def pieceAt(rank: Int, file: Int): Option[Piece] = pieceAt(Position(rank, file))

  def reset = {
    for (file <- 'a' to 'h'; rank <- 1 to 8) {
      val coord = position(Symbol(file.toString + rank))
      pieces = pieces.update(coord, None)
      rank match {
        case 1 => place(Piece(White, LineUp(file - 'a'))) at coord
        case 2 => place(Piece(White, Pawn)) at coord
        case 7 => place(Piece(Black, Pawn)) at coord
        case 8 => place(Piece(Black, LineUp(file - 'a'))) at coord
        case _ =>
      }
    }
  }

  def moves = movesMade

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
