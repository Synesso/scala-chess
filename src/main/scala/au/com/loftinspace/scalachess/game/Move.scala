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
      for (file <- minFile to maxFile; rank <- minRank to maxRank) traversed += position((rank, file))
    }
    traversed = traversed.-(from, to)
    traversed
  }

  def traversed(s: Symbol) = traversing.contains(position(s))
  
  //  todo - PGN

  override def toString = piece.colour.toString + piece.role + ":" + from + (if (taking.isDefined) "x" else "-") + to
}