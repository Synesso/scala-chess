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

  def movesWithinContext(pieces: Map[Position, Option[Piece]], lastMove: Option[Move]): Set[Position] = HashSet()

  override def toString = colour + " " + role
}
