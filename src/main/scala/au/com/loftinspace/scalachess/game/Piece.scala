package au.com.loftinspace.scalachess.game

import Positioning.{^, >, v, <, vectorBasedPositions, radialBasedPositions, position}

trait Role
case object King extends Role
case object Queen extends Role
case object Rook extends Role
case object Bishop extends Role
case object Knight extends Role
case object Pawn extends Role

trait Colour
case object White extends Colour
case object Black extends Colour

case class Piece(colour: Colour, role: Role) {
  def opposingColour = opposite of colour

  type BoardQuery = (Board, Position, List[History]) => IterationControl
  type Implication = (Board, Position, List[History]) => Board

  /**
   * Find the moves this piece can make from the given position.
   * @return A map of position sequences to tuple of BoardQuery and Implication
   */
  def movesFrom(pos: Position): Map[List[Position], (BoardQuery, Implication)] = {

    val resultSeed = Map.empty[List[Position], (BoardQuery, Implication)]

    def captureQuery(board: Board, target: Position, history: List[History]) =
      board.pieces.get(target).map(_.colour).map(col => if (col.equals(opposingColour)) IncludeAndStop else Stop).getOrElse(Continue)
    def pawnForwardQuery(board: Board, target: Position, history: List[History]) = if (board.pieces.contains(target)) Stop else Continue
    def pawnDiagonalQuery(board: Board, target: Position, history: List[History]) = {
      board.pieces.get(target).map(_.colour).map(col => if (col.equals(opposingColour)) IncludeAndStop else Stop).getOrElse {
        val onEnPassantRank = if (colour.equals(White)) pos.rank == 5 else pos.rank == 4
        val (to: Position, from: Position) = (Position(pos.rank, target.file), Position(if (colour.equals(White)) target.rank + 1 else target.rank - 1, target.file))
        if (onEnPassantRank && history.lastOption.map(h => h.move.isEnPassant && h.move.from.equals(from) && h.move.to.equals(to)).getOrElse(false)) IncludeAndStop else Stop
      }
    }
    def castleQuery(board: Board, target: Position, history: List[History]): IterationControl = {
      val rookPosition = if (target.file < pos.file) Position(pos.rank, 1) else Position(pos.rank, 8)
      for (f <- Math.min(pos.file, rookPosition.file) until Math.max(pos.file, rookPosition.file); if (f != Math.min(pos.file, rookPosition.file))) {
        if (board.pieces.contains(Position(pos.rank, f))) return Stop
      }
      for (f <- Math.min(pos.file, target.file) to Math.max(pos.file, target.file)) {
        val threats = board.threatsTo(colour).at(Position(pos.rank, f))
        if (threats.size > 0) return Stop
      }
      history.foreach(h => if (h.move.from.equals(pos) || h.move.from.equals(rookPosition)) return Stop)
      IncludeAndStop
    }

    def captureImplication(board: Board, target: Position, history: List[History]): Board = {
      val capturing = board.pieces.get(target).map(_.colour.equals(opposingColour)).getOrElse(false)
      (if (capturing) (board take target) else board) move pos to target
    }
    def pawnMoveOnlyIfEmptyImplication(board: Board, target: Position, history: List[History]): Board = board move pos to target
    def pawnMoveOnlyIfCapturingImplication(board: Board, target: Position, history: List[History]): Board = {
      val onEnPassantRank = if (colour.equals(White)) pos.rank == 5 else pos.rank == 4
      history.lastOption.foreach {
        history =>
                if (history.move.isEnPassant && history.move.to.equals(Position(pos.rank, target.file))) {
                  return (board take history.move.to) move pos to target
                }
      }
      val capturing = board.pieces.get(target).map(_.colour.equals(opposingColour)).getOrElse(false)
      if (capturing) (board take target) move pos to target else throw new IllegalMoveException("Cannot move " + this + " to " + target + " unless capturing")
    }
    def castleImplication(board: Board, target: Position, history: List[History]) = {
      val (rookFrom: Position, rookTo: Position) = {
        if (target.file < pos.file) (Position(pos.rank, 1), Position(pos.rank, 4))
        else (Position(pos.rank, 8), Position(pos.rank, 6))
      }
      (board move pos to target) move rookFrom to rookTo
    }


    val rookVectors = List(List(^ _), List(> _), List(v _), List(< _))
    val bishopVectors = List(List(^ _, < _), List(^ _, > _), List(v _, < _), List(v _, > _))
    val queenVectors = rookVectors ::: bishopVectors
    def vectorBasedMoves(directions: List[List[Option[Position] => Option[Position]]]) = {
      vectorBasedPositions(pos, directions).foldLeft(resultSeed) {(acc, listPos) => acc(listPos) = (captureQuery, captureImplication)}
    }
    def radialBasedMoves(offsets: Collection[Int], filter: (Int, Int) => Boolean) = {
      radialBasedPositions(pos, offsets, filter).foldLeft(resultSeed) {(acc, next) => acc(List(next)) = (captureQuery, captureImplication)}
    }
    def castlingMoves = {
      val positions = colour match {
        case Black if (position('e8).equals(pos)) => Set(List(position('c8)), List(position('g8)))
        case White if (position('e1).equals(pos)) => Set(List(position('c1)), List(position('g1)))
        case _ => Set.empty[List[Position]]
      }
      positions.foldLeft(resultSeed) {(acc, next) => acc(next) = (castleQuery, castleImplication)}
    }

    import Math.abs

    role match {
      case King => radialBasedMoves(-1 to 1, (rank: Int, file: Int) => {rank != 0 || file != 0}) ++ castlingMoves
      case Queen => vectorBasedMoves(queenVectors)
      case Bishop => vectorBasedMoves(bishopVectors)
      case Knight => radialBasedMoves(Set(-2, -1, 1, 2), (rank: Int, file: Int) => {abs(rank) != abs(file)})
      case Rook => vectorBasedMoves(rookVectors)
      case Pawn => {
        val unmoved = (colour.equals(White) && pos.rank == 2) || (colour.equals(Black) && pos.rank == 7)
        val fwd: Int => Option[Position] = if (colour.equals(White)) pos.^ else pos.v
        val onward = (if (unmoved) List((fwd(1)), (fwd(2))) else List((fwd(1)))).filter(_.isDefined).map(_.get)
        val takeLeft = List(<(fwd(1))).filter(_.isDefined).map(_.get)
        val takeRight = List(>(fwd(1))).filter(_.isDefined).map(_.get)
        (resultSeed + ((onward, (pawnForwardQuery _, pawnMoveOnlyIfEmptyImplication _)),
                (takeLeft, (pawnDiagonalQuery _, pawnMoveOnlyIfCapturingImplication _)),
                (takeRight, (pawnDiagonalQuery _, pawnMoveOnlyIfCapturingImplication _)))).filter(!_._1.isEmpty)
      }
      case _ => resultSeed
    }
  }

  override def toString = (colour + "_" + role).toLowerCase
}

trait IterationControl
case object Continue extends IterationControl
case object IncludeAndStop extends IterationControl
case object Stop extends IterationControl

object opposite {
  def of(c: Colour) = c match {
    case Black => White
    case White => Black
  }
}
