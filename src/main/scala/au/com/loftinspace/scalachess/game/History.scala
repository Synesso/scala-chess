package au.com.loftinspace.scalachess.game;

trait Action

case class History(move: Move, implication: Option[Action]) extends Action
case class Take(pieceAt: Position) extends Action
case class Promote(pawnAt: Position) extends Action
case class Move(piece: Piece, from: Position, to: Position) extends Action {
  import Positioning._
  def this(piece: Piece, from: Symbol, to: Symbol) = this(piece, position(from), position(to))
  def isEnPassant = Pawn.equals(piece.role) && from.file == to.file && (
          (White.equals(piece.colour) && from.rank == 2 && to.rank == 4) ||
          (Black.equals(piece.colour) && from.rank == 7 && to.rank == 5))

}

