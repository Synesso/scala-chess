package au.com.loftinspace.scalachess.game

class Implications(colour: Colour, role: Role, position: Position) {
  def opposingColour = opposite of colour
  def captureImplication(board: Board, target: Position, history: List[History]): Board = {
    val capturing = board.pieces.get(target).map(_.colour.equals(opposingColour)).getOrElse(false)
    (if (capturing) (board take target) else board) move position to target
  }
  def pawnMoveOnlyIfEmptyImplication(board: Board, target: Position, history: List[History]): Board = board move position to target
  def pawnMoveOnlyIfCapturingImplication(board: Board, target: Position, history: List[History]): Board = {
    val onEnPassantRank = if (colour.equals(White)) position.rank == 5 else position.rank == 4
    history.lastOption.foreach {
      history =>
              if (history.move.isEnPassant && history.move.to.equals(Position(position.rank, target.file))) {
                return (board take history.move.to) move position to target
              }
    }
    val capturing = board.pieces.get(target).map(_.colour.equals(opposingColour)).getOrElse(false)
    if (capturing) (board take target) move position to target else throw new IllegalMoveException("Cannot move " + this + " to " + target + " unless capturing")
  }
  def castleImplication(board: Board, target: Position, history: List[History]) = {
    val (rookFrom: Position, rookTo: Position) = {
      if (target.file < position.file) (Position(position.rank, 1), Position(position.rank, 4))
      else (Position(position.rank, 8), Position(position.rank, 6))
    }
    (board move position to target) move rookFrom to rookTo
  }
}