package au.com.loftinspace.scalachess.game

import org.specs._
import Positioning._
import Math.abs

object PositionSpecs extends GameSpecification {

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

    "be able to calculate a relative position with negative numbers" in {
      val pos = position('d5)
      (pos ^ -2) must beSome[Position].which(_.equals(position('d3)))
      (pos > -3) must beSome[Position].which(_.equals(position('a5)))
      (pos v -1) must beSome[Position].which(_.equals(position('d6)))
      (pos < -3) must beSome[Position].which(_.equals(position('g5)))
    }

    "be used to calculate a non-position off the edge of the board using negative numbers" in {
      val pos = position('d5)
      (pos ^ -6) must beNone
      (pos v -6) must beNone
      (pos > -6) must beNone
      (pos < -6) must beNone
    }

    "be used to derive a relative list of positions" in {
      val pos = position('d4)
      (pos ^^ 3) must containPositions('d4, 'd5, 'd6, 'd7)
      (pos >> 3) must containPositions('d4, 'e4, 'f4, 'g4)
      (pos vv 3) must containPositions('d4, 'd3, 'd2, 'd1)
      (pos << 3) must containPositions('d4, 'c4, 'b4, 'a4)
    }

    "be used to derive a relative list of positions not including those off the board" in {
      val pos = position('d4)
      (pos ^^ 8) must containPositions('d4, 'd5, 'd6, 'd7, 'd8)
      (pos >> 8) must containPositions('d4, 'e4, 'f4, 'g4, 'h4)
      (pos vv 8) must containPositions('d4, 'd3, 'd2, 'd1)
      (pos << 8) must containPositions('d4, 'c4, 'b4, 'a4)
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
