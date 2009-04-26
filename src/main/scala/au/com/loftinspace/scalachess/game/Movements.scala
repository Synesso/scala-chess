package au.com.loftinspace.scalachess.game

trait Movement
trait SpecialMove extends Movement
case object Straight extends Movement
case object Diagonally extends Movement
case object DogLeggedly extends Movement
case object Castle extends SpecialMove
case object Launch extends SpecialMove

case class CanMove[M <: Movement](m: M, p: Piece) {
  var dist = 1
  def towards(o: Orientation): Boolean = {
    p.role match {
      case Pawn => dist == 1 && o.equals(opposite of p.colour)
      case _ => false
    }
  }

  def towards(o: Tuple2[Colour, Role with Orientation]): Boolean = false
  def by(distance: Int) = {
    dist = distance
    this
  }
}

class IntegerEnabler(i: Int) {
  def square = i
  def squares = i
}

object IntegerEnabler {
  implicit def enableInteger(i: Int) = new IntegerEnabler(i)
}