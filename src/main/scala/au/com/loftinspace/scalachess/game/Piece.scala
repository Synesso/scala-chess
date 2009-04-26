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

class Piece(val colour: Colour, val role: Role, val position: Position) {
  def this(c: Colour, r: Role, col: Int, row: Int) = this(c, r, new InPosition(col, row))

  val ColumnLabels = "abcdefgh".toCharArray
  var game: Game = null

  def location: Option[String] =
  position match {
    case InPosition(c, r) => Some(ColumnLabels.charAt(c-1).toString + r)
    case Captured => None
  }

  def can[M <: SpecialMove](m: M) = canMove(m)
  def canMove[M <: Movement](m: M): CanMove[M] = CanMove(m, this)
}
