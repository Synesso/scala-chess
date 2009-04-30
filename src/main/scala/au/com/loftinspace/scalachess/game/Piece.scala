package au.com.loftinspace.scalachess.game

trait Role
trait Orientation
case object King extends Role with Orientation
case object Queen extends Role with Orientation
case object Rook extends Role
case object Bishop extends Role
case object Knight extends Role
case object Pawn extends Role

trait Colour
case object White extends Colour with Orientation
case object Black extends Colour with Orientation

object opposite {
  def of(r: Role with Orientation) = r match {
    case King => Queen
    case Queen => King
  }
  def of (c: Colour) = c match {
    case Black => White
    case White => Black
  }
}

class Piece(val colour: Colour, val role: Role) {

  var position: Position = null

  def opposingColour = opposite of colour

  override def toString = colour + " " + role
}
