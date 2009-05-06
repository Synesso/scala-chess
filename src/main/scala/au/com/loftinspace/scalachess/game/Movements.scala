package au.com.loftinspace.scalachess.game


case class MovementQuery(p: Piece, fromOption: Option[Position], to: Position) {
  def given(g: Game): Unit = {
    if (fromOption.isEmpty) return
    val from = fromOption.get
//    if (p.captured) throw new IllegalMoveException("Captured pieces cannot be moved")
//    if (from equals to) throw new IllegalMoveException("Piece would not be moved")
//    val movement = from >> to
//    if (!movement.candidates.contains(p.role))
//      throw new IllegalMoveException(p.role.toString + "s cannot move " + movement.description)

    // todo - what direction are they moving towards?
    // todo - how far? (pawn launch)
    // todo - anything in between?
    // todo - capturing only move?
    // todo - O-O and O-O-O
    // todo - en passant

    true
  }
}

case class Movement(traversal: Seq[Position], candidates: Set[Role], description: String)
