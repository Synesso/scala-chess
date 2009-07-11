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

  type BoardQuery = (Board, Position, Option[Delta]) => IterationControl
  type Implication = (Board, Position, Option[Delta]) => Board

  /**
   * Find the moves this piece can make from the given position.
   * @return A map of position sequences to tuple of BoardQuery and Implication
   */
  def movesFrom(pos: Position): Map[List[Position], (BoardQuery, Implication)] = {

    val resultSeed = Map.empty[List[Position], (BoardQuery, Implication)]

    def captureQuery(board: Board, target: Position, lastMove: Option[Delta]) =
      board.pieces.get(target).map(_.colour).map(col => if (col.equals(opposingColour)) IncludeAndStop else Stop).getOrElse(Continue)
    def pawnForwardQuery(board: Board, target: Position, lastMove: Option[Delta]) = if (board.pieces.contains(target)) Stop else Continue
    def pawnDiagonalQuery(board: Board, target: Position, lastMove: Option[Delta]) = {
      board.pieces.get(target).map(_.colour).map(col => if (col.equals(opposingColour)) IncludeAndStop else Stop).getOrElse {
        val onEnPassantRank = if (colour.equals(White)) pos.rank == 5 else pos.rank == 4
        val toAndFrom: Set[Position] = Set(Position(pos.rank, target.file), Position(if (colour.equals(White)) target.rank + 1 else target.rank - 1, target.file))
        if (onEnPassantRank &&
                board.pieces.get(Position(pos.rank, target.file)).map(_.equals(Piece(opposingColour, Pawn))).getOrElse(false) &&
                (toAndFrom ** lastMove.map(_.pieces.keySet).getOrElse(Set())).equals(toAndFrom))
          IncludeAndStop else Stop
      }
    }

    def captureImplication(board: Board, target: Position, lastMove: Option[Delta]): Board = {
      val capturing = board.pieces.get(target).map(_.colour.equals(opposingColour)).getOrElse(false)
      (if (capturing) (board take target) else board) move pos to target
    }
    def pawnMoveOnlyIfEmptyImplication(board: Board, target: Position, lastMove: Option[Delta]): Board = board move pos to target
    def pawnMoveOnlyIfCapturingImplication(board: Board, target: Position, lastMove: Option[Delta]): Board = {
      val onEnPassantRank = if (colour.equals(White)) pos.rank == 5 else pos.rank == 4
      lastMove.foreach {
        move =>
                val enPassantTo = move.enPassantTo
                if (enPassantTo.map(_.equals(Position(pos.rank, target.file))).getOrElse(false)) {
                  return (board take enPassantTo.get) move pos to target
                }
      }
      val capturing = board.pieces.get(target).map(_.colour.equals(opposingColour)).getOrElse(false)
      if (capturing) (board take target) move pos to target else throw new IllegalMoveException("Cannot move " + this + " to " + target + " unless capturing")
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
      positions.foldLeft(resultSeed) {(acc, next) => acc(next) = (captureQuery, captureImplication)}
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
