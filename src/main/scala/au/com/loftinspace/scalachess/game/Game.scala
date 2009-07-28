package au.com.loftinspace.scalachess.game

import _root_.com.twitter.service.Json
import reflect.BeanProperty
import scala.collection.immutable._
import scala.util.matching.Regex
import Positioning._

case class Game(@BeanProperty board: Board, history: List[History], players: Map[Colour, String], nextMove: Colour) {
  def this() = this (new Board().reset, Nil, Map.empty, White)

  /**
   * Find all possible moves on the board during the next turn of play
   */
  def moves: Map[Position, Set[Position]] = {
    val default: Map[Position, Set[Position]] = Map.empty
    board.pieces.foldLeft(default) { (moves, boardElement) =>
      val (position, piece) = boardElement
      if (nextMove.equals(piece.colour)) {
        val viableMoves = movesFrom(position)
        if (viableMoves.isEmpty) {
          moves
        } else {
          moves(position) = viableMoves
        }
      } else {
        moves
      }
    }
  }

  /**
   * Find all possible moves on the board during the next turn of play - in JSON format
   */
  def movesAsJson = {
    Json.build(moves).toString
  }

  /**
   * Find all valid moves on the board from the given position for the next turn of play
   */
  def movesFrom(pos: Position): Set[Position] = {
    type BoardQuery = (Board, Position, List[History]) => IterationControl

    def follow(vector: List[Position], query: BoardQuery): Set[Position] = {
      def next(positions: Set[Position], tail: List[Position]): Set[Position] = {
        if (tail.isEmpty) return positions
        query(board, tail(0), history) match {
          case Continue => next(positions + tail(0), tail.tail)
          case IncludeAndStop => positions + tail(0)
          case Stop => positions
        }
      }
      next(Set(), vector)
    }
    board.pieces.get(pos) match {
      case None => Set()
      case Some(piece) => if (piece.colour.equals(nextMove)) {
        piece.movesFrom(pos).foldLeft(Set(): Set[Position]) {
          (positions, vectorAndQuery) =>
                  val (vector, (query, implication)) = vectorAndQuery
                  positions ++ follow(vector, query)
        }
      } else {
        Set()
      }
    }
  }

  /*
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
    def findMovesFor(s: Symbol): Set[Position] = pieceAt(s).map(findMovesFor(_)).getOrElse(Set())

    def pieceAt(p: Position) = pieces(p)
    def pieceAt(s: Symbol): Option[Piece] = pieceAt(position(s))
    def pieceAt(rank: Int, file: Int): Option[Piece] = pieceAt(Position(rank, file))
    def piecesAt(positions: List[Position]): List[Option[Piece]] = positions.map(pieceAt(_))

    def presents(scenario: Map[Symbol, Option[Piece]]) =
      scenario.foldLeft(true) { (res, entry) => res && pieces(position(entry._1)).equals(entry._2) }

    def positionsOf(p: Piece) = pieces.filter(_._2.equals(Some(p))).keySet.toSeq

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
        val pos = position(symbol)
        val pieceOpt = layout.get(symbol)
        pieces = pieces.update(pos, pieceOpt)
        pieceOpt.foreach(_.position = Some(pos))
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
  */
}

