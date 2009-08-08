package au.com.loftinspace.scalachess.game

import scala.util.matching.Regex
import Math.{abs,min,max}

object Positioning {
  def position(s: String): Position = position(Symbol(s))
  def position(s: Symbol): Position = position(coordinateToTuple(s))
  def position(rankAndFile: Tuple2[Int, Int]): Position = Position(rankAndFile._1, rankAndFile._2)
  def ^(p: Option[Position]): Option[Position] = p.flatMap(_ ^ 1)
  def >(p: Option[Position]): Option[Position] = p.flatMap(_ > 1)
  def v(p: Option[Position]): Option[Position] = p.flatMap(_ v 1)
  def <(p: Option[Position]): Option[Position] = p.flatMap(_ < 1)
  def noop(p: Option[Position]) = p
  def vectorBasedPositions(from: Position, directions: List[List[Option[Position] => Option[Position]]]) = {
    def expand(direction: Seq[Option[Position] => Option[Position]]): List[Position] = {
      def next(acc: List[Position]): List[Position] = {
        val seed: Option[Position] = Some(acc.firstOption.getOrElse(from))
        val candidate = direction.foldLeft(seed) {(intermediate, step) => step(intermediate)}
        candidate match {
          case Some(p) => next(p :: acc)
          case None => acc
        }
      }
      next(Nil).reverse
    }
    directions.foldLeft(Nil: List[List[Position]]) {(acc, next) => expand(next) :: acc}.filter(l => !l.isEmpty)
  }
  def radialBasedPositions(from: Position, offsets: Collection[Int], filter: (Int, Int) => Boolean) = {
    (for (rank <- offsets; file <- offsets; if (filter(rank, file))) yield (from ^ rank).flatMap(_ < file)).filter(_.isDefined).map(_.get)
  }

  private val CoordinatePattern = new Regex("^[a-h][1-8]$")

  private def coordinateToTuple(s: Symbol) = {
    if (s == null || CoordinatePattern.findFirstIn(s.name).equals(None)) throw new IllegalArgumentException("Invalid coordinate: " + s)
    ((s.name.charAt(1).asDigit), (s.name.charAt(0).asDigit - 9))
  }
}

case class Position(rank: Int, file: Int) {
  val validValues = 1 to 8
  def ^(n: Int): Option[Position] = if (validValues.contains(rank + n)) Some(Position(rank + n, file)) else None
  def >(n: Int): Option[Position] = if (validValues.contains(file + n)) Some(Position(rank, file + n)) else None
  def v(n: Int): Option[Position] = this ^ (n * -1)
  def <(n: Int): Option[Position] = this > (n * -1)
  def -?(other: Position) = rank == other.rank
  def |?(other: Position) = file == other.file
  def /?(other: Position) = abs(file - other.file) == abs(rank - other.rank)
  def *?(other: Position) = this.-?(other) || this.|?(other) || this./?(other) 
  def ^^(n: Int): List[Position] = expand(n, List(Some(this)), Positioning.^).filter(_.isDefined).map(_.get).reverse
  def >>(n: Int): List[Position] = expand(n, List(Some(this)), Positioning.>).filter(_.isDefined).map(_.get).reverse
  def vv(n: Int): List[Position] = expand(n, List(Some(this)), Positioning.v).filter(_.isDefined).map(_.get).reverse
  def <<(n: Int): List[Position] = expand(n, List(Some(this)), Positioning.<).filter(_.isDefined).map(_.get).reverse
  def fileAsLetter = (96 + file).toChar.toString
  override def toString = fileAsLetter + rank
  private def expand(i: Int, accumulator: List[Option[Position]], direct: Option[Position] => Option[Position]): List[Option[Position]] = {
    if (i > 0 && accumulator.first.isDefined) expand(i-1, direct(accumulator.first) :: accumulator, direct) else accumulator
  }
}
