package au.com.loftinspace.scalachess.game

case class Move(val piece: Piece, val from: Position, val to: Position, val taking: Option[Piece]) {
  def isPawnLaunch =
    ((piece.colour.equals(White) && from.rank.equals(2) && to.rank.equals(4)) ||
     (piece.colour.equals(Black) && from.rank.equals(7) && to.rank.equals(5))) &&
    from.file.equals(to.file) && piece.role.equals(Pawn)

  //  todo - PGN
  override def toString = piece.colour.toString + piece.role + ":" + from + (if (taking.isDefined) "x" else "-") + to
}