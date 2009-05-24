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

  var captured = false
  var hasMoved = false
  var lastMoveWasPawnLaunch = false;
  var position: Option[Position] = None

  def opposingColour = opposite of colour
  def isInPlay = position.isDefined && !captured

  def movesWithinContext(pieces: Map[Position, Option[Piece]], lastMove: Option[Move]): Set[Position] = {

    val moveForwardBy: (Int) => Option[Position] = if (colour.equals(White)) position.get.^ else position.get.v

    def forward = {
      // todo - this is a pawn specific move - can be generic?
      var result: Set[Position] = HashSet()
      val maxSteps = if (hasMoved) 1 else 2
      var clearPath = true
      for (step <- 1 to maxSteps; if (moveForwardBy(step)).isDefined; if (clearPath); target = moveForwardBy(step).get) {
        clearPath = !pieces(target).isDefined
        if (clearPath) { result += target }
      }
      result
    }

    def diagonals: Set[Position] = {
      // todo - this is a pawn specific move - can be generic?
      val candidates = Set(moveForwardBy(1).get < 1, moveForwardBy(1).get > 1).filter(_.isDefined).map(_.get)
      candidates.filter{candidate => pieces(candidate).map(_.colour.equals(opposingColour)).getOrElse(false)}
    }

    def enpassant = {
      val sides = Set(position.get < 1, position.get > 1).filter(_.isDefined).map(_.get)
      lastMove.filter(move => move.isPawnLaunch && sides.contains(move.to))
              .map(move => Position((move.to.rank + move.from.rank) /2, move.to.file)).toList
    }

    if (isInPlay) {
      role match {
        case Pawn => forward ++ diagonals ++ enpassant
        case _ => Set()
      }
    } else Set()
  }

  override def toString = colour + " " + role
}
