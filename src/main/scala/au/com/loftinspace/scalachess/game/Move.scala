package au.com.loftinspace.scalachess.game

class Move(val piece: Piece, val from: Position, val to: Position, val taking: Piece) {
  def isEnPassant = from.rank.equals(2) && to.rank.equals(4) && from.file.equals(to.file)

}
