package au.com.loftinspace.scalachess.game

class Queries(colour: Colour, role: Role, position: Position) {
  def opposingColour = opposite of colour
  def captureQuery(board: Board, target: Position, history: List[History]) =
    board.pieces.get(target).map(_.colour).map(col => if (col.equals(opposingColour)) IncludeAndStop else Stop).getOrElse(Continue)
  def pawnForwardQuery(board: Board, target: Position, history: List[History]) = if (board.pieces.contains(target)) Stop else Continue
  def pawnDiagonalQuery(board: Board, target: Position, history: List[History]) = {
    board.pieces.get(target).map(_.colour).map(col => if (col.equals(opposingColour)) IncludeAndStop else Stop).getOrElse {
      val onEnPassantRank = if (colour.equals(White)) position.rank == 5 else position.rank == 4
      val (to: Position, from: Position) = (Position(position.rank, target.file), Position(if (colour.equals(White)) target.rank + 1 else target.rank - 1, target.file))
      if (onEnPassantRank && history.lastOption.map(h => h.move.isEnPassant && h.move.from.equals(from) && h.move.to.equals(to)).getOrElse(false)) IncludeAndStop else Stop
    }
  }
  def castleQuery(board: Board, target: Position, history: List[History]): IterationControl = {
    val rookPosition = if (target.file < position.file) Position(position.rank, 1) else Position(position.rank, 8)
    for (f <- Math.min(position.file, rookPosition.file) until Math.max(position.file, rookPosition.file); if (f != Math.min(position.file, rookPosition.file))) {
      if (board.pieces.contains(Position(position.rank, f))) return Stop
    }
    for (f <- Math.min(position.file, target.file) to Math.max(position.file, target.file)) {
      val threats = board.threatsTo(colour).at(Position(position.rank, f))
      if (threats.size > 0) return Stop
    }
    history.foreach(h => if (h.move.from.equals(position) || h.move.from.equals(rookPosition)) return Stop)
    IncludeAndStop
  }
}