package au.com.loftinspace.scalachess.game

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

object opposite {
  def of (c: Colour) = c match {
    case Black => White
    case White => Black
  }
}

case class Piece(colour: Colour, role: Role) {
  import scala.collection.immutable.HashSet
  import Math.abs

  var captured = false
  var hasMoved = false
  var position: Option[Position] = None

  def opposingColour = opposite of colour
  def isInPlay = position.isDefined && !captured

  def movesWithinContext(pieces: Map[Position, Option[Piece]], lastMove: Option[Move]): Set[Position] = {

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

    def expand(direction: (Position=>Option[Position])*): Set[Position] = {
      def next(p: Position, accumulator: Set[Position], direction: Array[(Position=>Option[Position])]): Set[Position] = {
        val nextPositionOpt = direction.foldLeft(Some(p): Option[Position]){
          (currentPos, nextMove) => currentPos.flatMap(nextMove(_))
        }
        val nextColourOpt = nextPositionOpt.flatMap(pieces(_)).map(piece => (piece.colour))
        nextColourOpt.map(nextColour => return if (nextColour.equals(colour)) accumulator else accumulator + nextPositionOpt.get)
        nextPositionOpt.map(nextPos => return next(nextPos, accumulator + nextPos, direction))
        accumulator
      }
      next(position.get, Set(), direction.toArray)
    }

    def follow(moves: (Position=>Option[Position])*) =
      moves.foldLeft(position) {(current, transform) => current.flatMap(pos => transform(pos))}

    def ^(p: Position) = p ^ 1
    def >(p: Position) = p > 1
    def v(p: Position) = p v 1
    def <(p: Position) = p < 1

    def ring(offsets: Set[Int], guard: (Int, Int) => Boolean): Set[Position] = {
      val potentials = for (x <- offsets; y <- offsets; if (guard(x,y))) yield position.get.^(x).flatMap(_.>(y))
      potentials.filter(_.isDefined).filter(pos => pieces(pos.get)
              .map(piece => piece.colour.equals(opposingColour)).getOrElse(true)).map(_.get)
    }

    def knightRing = ring(Set(-2, -1, 1, 2), (x: Int, y: Int) => abs(x)!=abs(y))
    def kingRing = ring(Set(-1, 0, 1), (x: Int, y: Int) => x!=0 || y!=0)
    def bishopMoves = expand(v,>) ++ expand(v,<) ++ expand(^,>) ++ expand(^,<)
    def rookMoves = expand(<) ++ expand(^) ++ expand(>) ++ expand(v)

    if (isInPlay) {
      role match {
        case Pawn => forward ++ diagonals ++ enpassant
        case Rook => rookMoves
        case Bishop => bishopMoves
        case Knight => knightRing
        case Queen => rookMoves ++ bishopMoves
        case King => kingRing
        case _ => Set()
      }
    } else Set()
  }

  override def toString = colour + " " + role
}
