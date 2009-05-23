package au.com.loftinspace.scalachess.game

import org.specs._
import Positioning._
import Math.abs

object PositionSpecs extends Specification {

  val validPositions = for (rank <- 1 to 8; file <- 1 to 8) yield position(rank, file)

  "A position" should {

    "be used to derive a relative position on the board" in {
      val pos = position('d5)
      (pos ^ 1) must beSome[Position].which(_.equals(position('d6)))
      (pos v 1) must beSome[Position].which(_.equals(position('d4)))
      (pos > 1) must beSome[Position].which(_.equals(position('e5)))
      (pos < 1) must beSome[Position].which(_.equals(position('c5)))
    }

    "be used to calculate a non-position off the edge of the board" in {
      val pos = position('d5)
      (pos ^ 4) must beNone
      (pos v 5) must beNone
      (pos > 5) must beNone
      (pos < 4) must beNone
    }

    "advise if it is along the same rank as another position" in {
      for (one <- validPositions; another <- validPositions) {
        if (one.rank == another.rank) {
          one -? another must beTrue
          another -? one must beTrue
        } else {
          one -? another must beFalse
          another -? one must beFalse
        }
      }
    }

    "advise if it is along the same file as another position" in {
      for (one <- validPositions; another <- validPositions) {
        if (one.file == another.file) {
          one |? another must beTrue
          another |? one must beTrue
        } else {
          one |? another must beFalse
          another |? one must beFalse
        }
      }
    }

    "advise if it is along the same diagonal as another position" in {
      for (one <- validPositions; another <- validPositions) {
        if (abs(one.file - another.file) == abs(one.rank - another.rank)) {
          one /? another must beTrue
          another /? one must beTrue
        } else {
          one /? another must beFalse
          another /? one must beFalse
        }
      }
    }
  }

}
