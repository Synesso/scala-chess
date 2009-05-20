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
    var result: Set[Position] = HashSet()
    if (isInPlay) {
      role match {
        case Pawn => {
          val maxSteps = if (hasMoved) 1 else 2
          val moveForwardBy: (Int) => Option[Position] = if (colour.equals(White)) position.get.^ else position.get.v
          var clearPath = true
          for (step <- 1 to maxSteps; if (moveForwardBy(step)).isDefined; if (clearPath); target = moveForwardBy(step).get) {
            clearPath = !pieces(target).isDefined
            if (clearPath) { result += target }
          }
          val diagonals = List(moveForwardBy(1).get < 1, moveForwardBy(1).get > 1)
          for (diagonal <- diagonals;
               if (diagonal.isDefined);
               if (pieces(diagonal.get).isDefined);
               if ((pieces(diagonal.get).get).colour.equals(opposingColour))) {
            result += diagonal.get
          }
          val sides = List((position.get < 1, moveForwardBy(1).get < 1), (position.get > 1, moveForwardBy(1).get > 1))
          for (side <- sides;
               if (side._1.isDefined);
               if (pieces(side._1.get).isDefined);
               if (pieces(side._1.get).get.lastMoveWasPawnLaunch)) {
            result += side._2.get
          }
        }
      }
    }
    result
  }

  override def toString = colour + " " + role
}
