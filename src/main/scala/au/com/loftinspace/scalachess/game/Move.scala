package au.com.loftinspace.scalachess.game

import scala.collection.immutable.Set
import Positioning._
import Math.{abs, min, max}

case class Move(val piece: Piece, val from: Position, val to: Position, val taking: Option[Piece]) {
  def isPawnLaunch =
    ((piece.colour.equals(White) && from.rank.equals(2) && to.rank.equals(4)) ||
     (piece.colour.equals(Black) && from.rank.equals(7) && to.rank.equals(5))) &&
    from.file.equals(to.file) && piece.role.equals(Pawn)

  def traversing: Set[Position] = {
    if (from equals to) return Set()
    var traversed: Set[Position] = Set()
    val minFile = min(from.file, to.file)
    val maxFile = max(from.file, to.file)
    val minRank = min(from.rank, to.rank)
    val maxRank = max(from.rank, to.rank)
    if (from |? to) {
      for (rank <- minRank to maxRank) traversed += position((rank, to.file))
    } else if (from -? to) {
      for (file <- minFile to maxFile) traversed += position((to.rank, file))
    } else if (from /? to) {
      for (file <- minFile to maxFile; rank <- minRank to maxRank; if (rank-from.rank == file-from.file)) traversed += position((rank, file))
    }
    traversed = traversed.-(from, to)
    traversed
  }

  def traversed(s: Symbol) = traversing.contains(position(s))

  def implies: Option[Move] = {
    val rank = if (piece.colour.equals(White)) 1 else 8
    if (piece.role.equals(King))
      if (from.equals(Position(rank, 5)) && to.equals(Position(rank, 7)))
        Some(Move(Piece(piece.colour, Rook), Position(rank, 8), Position(rank, 6), None))
      else if (from.equals(Position(rank, 5)) && to.equals(Position(rank, 3)))
        Some(Move(Piece(piece.colour, Rook), Position(rank, 1), Position(rank, 4), None))
      else None
    else None
  }

  //  todo - PGN

  override def toString = piece.colour.toString + piece.role + ":" + from + (if (taking.isDefined) "x" else "-") + to
}