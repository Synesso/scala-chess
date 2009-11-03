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

  type BoardQuery = (Board, Position, List[History]) => IterationControl
  type Implication = (Board, Position, List[History]) => Board

  /**
   * Find the moves this piece can make from the given position.
   * @return A map of position sequences to tuple of BoardQuery and Implication
   */
  def movesFrom(pos: Position): Map[List[Position], (BoardQuery, Implication)] = {

    val resultSeed = Map.empty[List[Position], (BoardQuery, Implication)]

    val queries = new Queries(colour, role, pos)
    val implications = new Implications(colour, role, pos)

    val rookVectors = List(List(^ _), List(> _), List(v _), List(< _))
    val bishopVectors = List(List(^ _, < _), List(^ _, > _), List(v _, < _), List(v _, > _))
    val queenVectors = rookVectors ::: bishopVectors
    def vectorBasedMoves(directions: List[List[Option[Position] => Option[Position]]]) = {
      vectorBasedPositions(pos, directions).foldLeft(resultSeed) {(acc, listPos) => acc(listPos) = (queries.captureQuery, implications.captureImplication)}
    }
    def radialBasedMoves(offsets: Collection[Int], filter: (Int, Int) => Boolean) = {
      radialBasedPositions(pos, offsets, filter).foldLeft(resultSeed) {(acc, next) => acc(List(next)) = (queries.captureQuery, implications.captureImplication)}
    }
    def castlingMoves = {
      val positions = colour match {
        case Black if (position('e8).equals(pos)) => Set(List(position('c8)), List(position('g8)))
        case White if (position('e1).equals(pos)) => Set(List(position('c1)), List(position('g1)))
        case _ => Set.empty[List[Position]]
      }
      positions.foldLeft(resultSeed) {(acc, next) => acc(next) = (queries.castleQuery, implications.castleImplication)}
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
        (resultSeed + ((onward, (queries.pawnForwardQuery _, implications.pawnMoveOnlyIfEmptyImplication _)),
                (takeLeft, (queries.pawnDiagonalQuery _, implications.pawnMoveOnlyIfCapturingImplication _)),
                (takeRight, (queries.pawnDiagonalQuery _, implications.pawnMoveOnlyIfCapturingImplication _)))).filter(!_._1.isEmpty)
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
