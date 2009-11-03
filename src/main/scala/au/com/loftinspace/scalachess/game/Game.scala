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

  def toJson = Json.build(Map(
    'moves.name -> moves,
    'pieces.name -> board.pieces,
    'taken.name -> board.taken,
    'last.name -> history.firstOption.map(_.toJsonMap).getOrElse(null))).toString

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

  def move(from: String, to: String) = {
    val opponent = opposite of nextMove
    val piece = board.pieces(position(from))
    val (newBoard, newHistory) = if (board.pieces.get(position(to)).map(_.colour.equals(opponent)).getOrElse(false)) {
      ((board take position(to)) move position(from) to position(to), History(Move(piece, position(from), position(to)), Some(Take(position(to)))))
    } else {
      ((board move position(from) to position(to)), History(Move(piece, position(from), position(to)), None))
    }
    Game(newBoard, newHistory :: history, players, opponent)
  }
}

