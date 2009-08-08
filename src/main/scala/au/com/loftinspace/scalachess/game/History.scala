package au.com.loftinspace.scalachess.game;

trait Action

case class History(move: Move, implication: Option[Action]) extends Action {
  def toJsonMap = {
    def moveJson(from: Position, to: Position) = Map('from.name -> from, 'to.name -> to)
    val implicationMap: Map[String, AnyRef] = implication match {
      case Some(Take(pos)) => Map('take.name -> pos)
      case Some(Promote(pos)) => Map('promote.name -> pos)
      case Some(Move(piece, from, to)) => Map('alsoMove.name -> moveJson(from, to))
      case _ => Map.empty
    }
    moveJson(move.from, move.to) ++ implicationMap
  }
}
case class Take(pieceAt: Position) extends Action
case class Promote(pawnAt: Position) extends Action
case class Move(piece: Piece, from: Position, to: Position) extends Action {
  def isEnPassant = Pawn.equals(piece.role) && from.file == to.file && (
          (White.equals(piece.colour) && from.rank == 2 && to.rank == 4) ||
          (Black.equals(piece.colour) && from.rank == 7 && to.rank == 5))

}

