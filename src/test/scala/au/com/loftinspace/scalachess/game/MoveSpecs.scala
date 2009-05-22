package au.com.loftinspace.scalachess.game

import org.specs._
import Positioning._

object MoveSpec extends Specification with SystemContexts {

  "a move" should {
    "describe the piece being moved" in {
      val piece = Piece(White, Bishop)
      val move = Move(piece, null, null, None)
      move.piece must_== piece
    }

    "describe the position moved from" in {
      val position = Position(4, 7)
      val move = Move(null, position, null, None)
      move.from must_== position
    }

    "describe the position moved to" in {
      val position = Position(3, 1)
      val move = Move(null, null, position, None)
      move.to must_== position
    }

    "describe the piece taken, if any" in {
      val piece = Piece(Black, Pawn)
      val move = Move(null, null, null, Some(piece))
      move.taking must beSome[Piece].which(_.equals(piece))
    }

    "know about the positions travelled throughi, along a file" in {
      val move = Move(null, position('a3), position('a7), None)
      move.traversing must containAll('a4, 'a5, 'a6)
    }
  }

  "a white pawn move two spaces forward from its starting position" should {
    "be described as pawnlaunch" in {
      val move = Move(Piece(White, Pawn), position('d2), position('d4), None)
      move.isPawnLaunch must beTrue
    }
  }

  "a black pawn move two spaces forward from its starting position" should {
    "be described as pawnlaunch" in {
      val move = Move(Piece(Black, Pawn), position('h7), position('h5), None)
      move.isPawnLaunch must beTrue
    }
  }

  "a pawn move other than two spaces forward from its starting position" should {
    "not be described as pawnlaunch" in {
      Move(Piece(White, Pawn), position('a2), position('a3), None).isPawnLaunch must beFalse
    }
  }

  "a non-pawn move" should {
    "not be described as pawn launch" in {
      Move(Piece(White, Bishop), position('a2), position('a4), None).isPawnLaunch must beFalse
    }
  }
  
  "a pawn capturing move" should {
    "not be described as pawn launch" in {
      Move(Piece(Black, Pawn), position('a2), position('b3), None).isPawnLaunch must beFalse
    }
  }
}
