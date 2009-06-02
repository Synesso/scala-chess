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

case class Piece(colour: Colour, role: Role) {
  import Positioning.{^,>,v,<}
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
        case King => kingRing ++ castling
        case _ => Set()
      }
    } else Set()
  }

  def pendingThreats(game: Game): Set[Seq[Position]] = {
    // if one of our pieces, then an opposing queen, bishop (diags only) or rook (rankfiles only)
    // then this is a threat.

    // expand ...
    // if edge of board, fail
    // if first piece is opposite colour, fail
    // if second piece is same colour, fail
    // if rank or file and second piece is queen or rook, pass - add it
    // if diagonal and second piece is queen or bishop, pass -add it

    // todo - check function does not ascertain role of opposing piece.
/*
    def check(direction: (Position=>Option[Position])*): List[Position] = {
      def next(p: Position, accumulator: (List[Position], Option[Piece]), direction: Array[(Position=>Option[Position])]): List[Position] = {
        val nextPositionOpt = direction.foldLeft(Some(p): Option[Position]) { (currrentPos, nextMove) => currentPos.flatMap(nextMove(_)) }
        // if next position is not present, exit with Nil
        if (nextPositionOpt.isEmpty) return Nil
        val nextPieceOpt = nextPositionOpt.flatMap(pieces(_))
        nextPieceOpt.get match {
          case Some(Piece(col, rol)) => {
            accumulator._2 match {
              case Some(Piece(accCol, accRol)) => {
                if (col.equals(colour))
                  // if next piece is same colour && accum piece is set, exit with Nil
                  return Nil
                else
                  // if next piece is opposite colour && accum piece is set, exit with position :: accum._1
                  return nextPositionOpt.get :: accumulator._1
              }
              case None => {
                if (col.equals(colour))
                  // if next piece is same colour && accum piece is not set, set it, add position & continue
                  return next(nextPositionOpt.get, (nextPositionOpt.get :: accumulator._1, nextPieceOpt, direction))
                else
                  // if next piece is opposite colour && accum piece is not set, exit with Nil
                  return Nil
              }
            }
          }
          // if next piece is not present, add position & continue
          case None => return next(nextPositionOpt.get, (nextPositionOpt.get :: accumulator._1, accumulator._2), direction)
        }
      }
      next(position.get, (Nil, None), direction.toArray)
    }
*/

    // todo - for each rank/file check for queen or rooks
    // todo - for each diagonal check for queen or bishops
    Set()
  }

  override def toString = colour + " " + role
}

object opposite {
  def of (c: Colour) = c match {
    case Black => White
    case White => Black
  }
}
