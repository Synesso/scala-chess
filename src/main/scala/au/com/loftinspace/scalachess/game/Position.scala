package au.com.loftinspace.scalachess.game

import scala.util.matching.Regex

object Positioning {
  def position(s: Symbol): InPosition = position(arrayIndexForCoordinate(s))
  def position(rankAndFile: Tuple2[Int, Int]): InPosition = InPosition(rankAndFile._1, rankAndFile._2)

  private val CoordinatePattern = new Regex("^[a-h][1-8]$")

  private def arrayIndexForCoordinate(s: Symbol) = {
    if (s == null || CoordinatePattern.findFirstIn(s.name).equals(None)) throw new IllegalArgumentException("Invalid coordinate: " + s)
    ((s.name.charAt(0).asDigit - 10), (s.name.charAt(1).asDigit - 1))
  }
}

trait Position {
  def asArrayIndex: Option[Int]
  def >>(p: Position): Movement
}

case object Captured extends Position {
  override def asArrayIndex: Option[Int] = None
  override def >>(p: Position) = Movement(Nil, Set.empty, "when captured")
}

case class InPosition(rank: Int, file: Int) extends Position {
  override def asArrayIndex: Option[Int] = Some(rank + (file * 8))
  override def >>(p: Position) = {
    p match {
      case InPosition(toRank, toFile) => {
        def fileStep: Int = if (file > toFile) -1 else 1
        def rankStep: Int = if (rank > toRank) -1 else 1
        def noStep = rank == toRank && file == toFile
        def alongRank = rank == toRank
        def alongFile = file == toFile
        def diagonal = Math.abs(file - toFile) == Math.abs(rank - toRank)
        def strafe = (Math.abs(file - toFile) + Math.abs(rank - toRank) == 3) && !alongRank && !alongFile

        if (noStep) Movement(Nil, Set.empty, "in place")
        if (alongRank) Movement((toFile until file by fileStep).reverse.map{(f) => InPosition(rank, f)}, Set(Rook, King, Queen), "along file")
        if (alongFile) Movement((toRank until rank by fileStep).reverse.map{(r) => InPosition(r, file)}, Set(Pawn, Queen, King, Rook), "along rank")
        if (diagonal) Movement((1 to Math.abs(file - toFile)).map{(d) => InPosition(rank + (rankStep * d), file + (fileStep * d))}, Set(Pawn, King, Queen, Bishop), "diagonally")
        if (strafe) Movement((toFile until file by fileStep).reverse.map{(f) => InPosition(rank, f)} ++ (toRank until rank by fileStep).reverse.map{(r) => InPosition(r, file)}, Set(Knight), "like a knight")
        Movement(Nil, Set.empty, "randomly")
      }
      case _ => Movement(Nil, Set.empty, "when not yet on the board")
    }
  }
}
