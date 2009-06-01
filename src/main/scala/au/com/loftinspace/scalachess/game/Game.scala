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

  def piecesMap = pieces
  def capturedPieces = piecesCaptured
  def moves = movesMade

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

  def findMovesFor(p: Piece): Set[Position] = p.movesWithinContext(this, moves.lastOption)

  def pieceAt(p: Position) = pieces(p)
  def pieceAt(s: Symbol): Option[Piece] = pieceAt(position(s))
  def pieceAt(rank: Int, file: Int): Option[Piece] = pieceAt(Position(rank, file))
  def piecesAt(positions: List[Position]): List[Option[Piece]] = positions.map(pieceAt(_))

  def presents(scenario: Map[Symbol, Option[Piece]]) =
    scenario.foldLeft(true) { (res, entry) => res && pieces(position(entry._1)).equals(entry._2) }

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

  def arrange(layout: Map[Symbol, Piece]) = {
    for (rank <- 1 to 8; file <- 'a' to 'h') {
      val symbol = Symbol(file.toString + rank)
      pieces = pieces.update(position(symbol), layout.get(symbol))
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

object Scenarios {
  val WhiteKingsRookCastle = Map('e1->Some(Piece(White, King)), 'f1->None, 'g1->None, 'h1->Some(Piece(White, Rook)))
  val WhiteQueensRookCastle = Map('e1->Some(Piece(White, King)), 'd1->None, 'c1->None, 'b1->None, 'a1->Some(Piece(White, Rook)))
  val BlackKingsRookCastle = Map('e8->Some(Piece(Black, King)), 'f8->None, 'g8->None, 'h8->Some(Piece(Black, Rook)))
  val BlackQueensRookCastle = Map('e8->Some(Piece(Black, King)), 'd8->None, 'c8->None, 'b8->None, 'a8->Some(Piece(Black, Rook)))
}