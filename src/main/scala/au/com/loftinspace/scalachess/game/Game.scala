package au.com.loftinspace.scalachess.game

import scala.collection.immutable._
import scala.util.matching.Regex
import Positioning._

class Game {
  private val CoordinatePattern = new Regex("^[a-h][1-8]$")
  private val LineUp = Rook :: Bishop :: Knight :: Queen :: King :: Knight :: Bishop :: Rook :: Nil

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
      val taken = Game.this place piece at s
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
    var moves: Set[Position] = HashSet()
    if (!p.isInPlay) return moves
    val position = p.position.get
    p match {
      case Piece(colour, role) => {
          role match {
            case Pawn => {
                val maxSteps = if (p.hasMoved) 1 else 2
                val moveForwardBy: (Int) => Option[Position] = if (colour.equals(White)) position.^ else position.v
                for (step <- 1 to maxSteps; if (moveForwardBy(step)).isDefined; target = moveForwardBy(step).get) {
                  if (pieceAt(target).isDefined) return moves;
                  moves += target
                }
                val diagonals = List(moveForwardBy(1).get < 1, moveForwardBy(1).get > 1)
                for (diagonal <- diagonals;
                     if (diagonal.isDefined);
                     if (pieceAt(diagonal.get).isDefined);
                     if ((pieceAt(diagonal.get).get).colour.equals(p.opposingColour))) {
                  moves += diagonal.get
                }
                val sides = List((position < 1, moveForwardBy(1).get < 1), (position > 1, moveForwardBy(1).get > 1))
                for (side <- sides;
                     if (side._1.isDefined);
                     if (pieceAt(side._1.get).isDefined);
                     if (pieceAt(side._1.get).get.lastMoveWasPawnLaunch)) {
                  moves += side._2.get
                }
              }
            case _ =>
          }
        }
    }
    moves
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
