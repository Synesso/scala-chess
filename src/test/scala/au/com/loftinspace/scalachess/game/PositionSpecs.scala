package au.com.loftinspace.scalachess.game

import org.specs._
import Positioning._

object PositionSpecs extends Specification {

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
  }

}
