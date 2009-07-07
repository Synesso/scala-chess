package au.com.loftinspace.scalachess.game


//import java.util.{Collection, Set, Map, List} // todo - remove me
//import javax.swing.text.html.Option // todo - remove me
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

/*
case class Piece(colour: Colour, role: Role) {
  import Positioning.{^,>,v,<,noop}
  import scala.collection.immutable.HashSet
  import Math.abs
  import Scenarios._

  var captured = false
  var hasMoved = false
  var position: Option[Position] = None

  def opposingColour = opposite of colour
  def isInPlay = position.isDefined && !captured

  def movesWithinContext(game: Game, lastMove: Option[Move]): Set[Position] = {
    val pieces = game.piecesMap
    val moveForwardBy: (Int) => Option[Position] = if (colour.equals(White)) position.get.^ else position.get.v

    def forward = {
      val potentials = if (hasMoved) Set(moveForwardBy(1)) else Set(moveForwardBy(1), moveForwardBy(2))
      potentials.foldLeft((Set(): Set[Position], true)) { (result: (Set[Position], Boolean), pos: Option[Position]) =>
        if (!result._2)
          result
        else if (pos.map(!pieces(_).isDefined).getOrElse(false))
          (result._1 + pos.get, true)
        else
          (result._1, false)
      }._1
    }

    def diagonals: Set[Position] = {
      val candidates = Set(moveForwardBy(1).get < 1, moveForwardBy(1).get > 1).filter(_.isDefined).map(_.get)
      candidates.filter{candidate => pieces(candidate).map(_.colour.equals(opposingColour)).getOrElse(false)}
    }

    def enpassant = {
      val sides = Set(position.get < 1, position.get > 1).filter(_.isDefined).map(_.get)
      lastMove.filter(move => move.isPawnLaunch && sides.contains(move.to))
              .map(move => Position((move.to.rank + move.from.rank) /2, move.to.file)).toList
    }

    def castling = {
      if (hasMoved) Set() else
      colour match {
        case Black => Set(
          (if ((game presents BlackKingsRookCastle) && !(game pieceAt 'h8 get).hasMoved) Some(Position(8, 7)) else None),
          (if ((game presents BlackQueensRookCastle) && !(game pieceAt 'a8 get).hasMoved) Some(Position(8, 3)) else None)).filter(_.isDefined).map(_.get)
        case White => Set(
          (if ((game presents WhiteKingsRookCastle) && !(game pieceAt 'h1 get).hasMoved) Some(Position(1, 7)) else None),
          (if ((game presents WhiteQueensRookCastle) && !(game pieceAt 'a1 get).hasMoved) Some(Position(1, 3)) else None)).filter(_.isDefined).map(_.get)
      }
    }

    def expand(direction: (Option[Position]=>Option[Position])*): Set[Position] = {
      def next(p: Option[Position], accumulator: Set[Position], direction: Array[(Option[Position]=>Option[Position])]): Set[Position] = {
        val nextPositionOpt = direction.foldLeft(p: Option[Position]){
          (currentPos, nextMove) => nextMove(currentPos)
        }
        val nextColourOpt = nextPositionOpt.flatMap(pieces(_)).map(piece => (piece.colour))
        nextColourOpt.map(nextColour => return if (nextColour.equals(colour)) accumulator else accumulator + nextPositionOpt.get)
        if (nextPositionOpt.isDefined) next(nextPositionOpt, accumulator + nextPositionOpt.get, direction) else accumulator
      }
      next(position, Set(), direction.toArray)
    }

    def follow(moves: (Position=>Option[Position])*) =
      moves.foldLeft(position) {(current, transform) => current.flatMap(pos => transform(pos))}

    def ring(offsets: Set[Int], guard: (Int, Int) => Boolean): Set[Position] = {
      val potentials = for (x <- offsets; y <- offsets; if (guard(x,y))) yield position.get.^(x).flatMap(_.>(y))
      potentials.filter(_.isDefined).filter(pos => pieces(pos.get)
              .map(piece => piece.colour.equals(opposingColour)).getOrElse(true)).map(_.get)
    }

    def knightRing = ring(Set(-2, -1, 1, 2), (x: Int, y: Int) => abs(x)!=abs(y))
    def kingRing = ring(Set(-1, 0, 1), (x: Int, y: Int) => x!=0 || y!=0)
    def bishopMoves = expand(v,>) ++ expand(v,<) ++ expand(^,>) ++ expand(^,<)
    def rookMoves = expand(<) ++ expand(^) ++ expand(>) ++ expand(v)

    def exposesKing(target: Position) = {
      def expandAndCollect(current: Option[Position], rankMove: (Option[Position] => Option[Position]),
                           fileMove: (Option[Position] => Option[Position]), collected: Set[Position]): (Set[Position], Option[Piece]) = {
        val next = rankMove(fileMove(current))
        if (next.isEmpty) return (collected, None)
        val piece = game pieceAt (next.get)
        if (piece.isEmpty) expandAndCollect(next, rankMove, fileMove, collected + next.get)
        else (collected, piece)
      }

      val positionOfKing = (game positionsOf Piece(colour, King))(0)
      if (position.get *? positionOfKing) {
        val rankMoves: Seq[Option[Position] => Option[Position]] = position.get.rank - positionOfKing.rank match {
          case r if r < 0 => Seq(^, v)
          case r if r > 0 => Seq(v, ^)
          case _ => Seq(noop, noop)
        }
        val fileMoves: Seq[Option[Position] => Option[Position]] = position.get.file - positionOfKing.file match {
          case f if f < 0 => Seq(<, >)
          case f if f > 0 => Seq(>, <)
          case _ => Seq(noop, noop)
        }
        val expansionToKing: (Set[Position], Option[Piece]) = expandAndCollect(position, rankMoves(0), fileMoves(0), Set())
        val expansionFromKing: (Set[Position], Option[Piece]) = expandAndCollect(position, rankMoves(1), fileMoves(1), Set())
        if (expansionToKing._1.contains(target) || expansionFromKing._1.contains(target)) false else
        expansionFromKing._2 match {
          case Some(Piece(opposingColour, Queen)) => true
          case Some(Piece(opposingColour, Bishop)) => position.get /? positionOfKing
          case Some(Piece(opposingColour, Rook)) => (position.get |? positionOfKing) || (position.get -? positionOfKing)
          case _ => false
        }
      } else false
    }

    val candidatePositions: Set[Position] = if (isInPlay) {
      role match {
        case Pawn => forward ++ diagonals ++ enpassant
        case Rook => rookMoves
        case Bishop => bishopMoves
        case Knight => knightRing
        case Queen => rookMoves ++ bishopMoves
        case King => kingRing ++ castling
        case _ => Set()
      }
    } else Set()

    candidatePositions.filter(candidate => !exposesKing(candidate))
  }

  override def toString = colour + " " + role
}
*/

object opposite {
  def of(c: Colour) = c match {
    case Black => White
    case White => Black
  }
}
