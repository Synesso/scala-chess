package au.com.loftinspace.scalachess.game

import Positioning.{^, >, v, <}

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

  type BoardQuery = (Board, Position) => IterationControl
  type Implication = (Board, Position) => Board

  /**
   * Find the moves this piece can make from the given position.
   * @return A map of position sequences to a function that takes a board and a position and returns an IterationControl
   */
  def movesFrom(pos: Position): Map[List[Position], (BoardQuery, Implication)] = {

    val resultSeed = Map.empty[List[Position], (BoardQuery, Implication)]

    def captureQuery(board: Board, target: Position) =
      board.pieces.get(target).map(_.colour).map(col => if (col.equals(opposingColour)) IncludeAndStop else Stop).getOrElse(Continue)
    def pawnForwardQuery(board: Board, target: Position) = if (board.pieces.contains(target)) Stop else Continue
    def pawnDiagonalQuery(board: Board, target: Position) = {
      board.pieces.get(target).map(_.colour).map(col => if (col.equals(opposingColour)) IncludeAndStop else Stop).getOrElse(Stop)
    }

    def captureImplication(board: Board, target: Position): Board = {
      val capturing = board.pieces.get(target).map(_.colour.equals(opposingColour)).getOrElse(false)
      (if (capturing) (board take target) else board) move pos to target
    }
    def moveOnlyIfEmptyImplication(board: Board, target: Position): Board = board move pos to target
    def moveOnlyIfCapturingImplication(board: Board, target: Position): Board = {
      val capturing = board.pieces.get(target).map(_.colour.equals(opposingColour)).getOrElse(false)
      if (capturing) (board take target) move pos to target else throw new IllegalMoveException("Cannot move " + this + " to " + target + " unless capturing") 
    }


    def expand(direction: Seq[Option[Position] => Option[Position]]): List[Position] = {
      def next(acc: List[Position]): List[Position] = {
        val seed: Option[Position] = Some(acc.firstOption.getOrElse(pos))
        val candidate = direction.foldLeft(seed) {(intermediate, step) => step(intermediate)}
        candidate match {
          case Some(p) => next(p :: acc)
          case None => acc
        }
      }
      next(Nil).reverse
    }

    val rookVectors = List(List(^ _), List(> _), List(v _), List(< _))
    val bishopVectors = List(List(^ _, < _), List(^ _, > _), List(v _, < _), List(v _, > _))
    val queenVectors = rookVectors ::: bishopVectors
    def vectorBasedMoves(directions: List[List[Option[Position] => Option[Position]]]) = {
      val positions = directions.foldLeft(Nil: List[List[Position]]) {(acc, next) => expand(next) :: acc}.filter(l => !l.isEmpty)
      positions.foldLeft(resultSeed) {(acc, listPos) => acc(listPos) = (captureQuery, captureImplication)}
    }
    def radialBasedMoves(offsets: Collection[Int], filter: (Int, Int) => Boolean) = {
      val positions = (for (rank <- offsets; file <- offsets; if (filter(rank, file))) yield (pos ^ rank).flatMap(_ < file)).filter(_.isDefined).map(_.get)
      positions.foldLeft(resultSeed) {(acc, next) => acc(List(next)) = (captureQuery, captureImplication)}
    }

    import Math.abs

    role match {
      case King => radialBasedMoves(-1 to 1, (rank: Int, file: Int) => {rank != 0 || file != 0})
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
        (resultSeed + ((onward, (pawnForwardQuery _, moveOnlyIfEmptyImplication _)),
                (takeLeft, (pawnDiagonalQuery _, moveOnlyIfCapturingImplication _)),
                (takeRight, (pawnDiagonalQuery _, moveOnlyIfCapturingImplication _)))).filter(!_._1.isEmpty)
      }
      case _ => Map.empty[List[Position], (BoardQuery, Implication)]
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
