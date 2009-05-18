package au.com.loftinspace.scalachess.game

import org.specs._
import Positioning._

object MoveSpec extends Specification with SystemContexts {

  "a move" should {
    "describe the piece being moved" in {
      val piece = Piece(White, Bishop)
      val move = new Move(piece, null, null, null)
      move.piece must_== piece
    }

    "describe the position moved from" in {
      val position = Position(4, 7)
      val move = new Move(null, position, null, null)
      move.from must_== position
    }

    "describe the position moved to" in {
      val position = Position(3, 1)
      val move = new Move(null, null, position, null)
      move.to must_== position
    }

    "describe the piece taken, if any" in {
      val piece = Piece(Black, Pawn)
      val move = new Move(null, null, null, piece)
      move.taking must_== piece
    }
  }

  // TODO - actually, en passant is the reply. This doesn't have a name. Call it pawn-launch?
  "a pawn move two spaces forward from its starting position" should {
    "be described as en passant" in {
      val move = new Move(Piece(White, Pawn), Position(2, 1), Position(4, 1), null)
      move.isEnPassant must beTrue
    }
  }

  // TODO -- check for every pawn
  "a pawn move other than two spaces forward from its starting position" should {
    "not be described as en passant" in {
      val move = new Move(Piece(White, Pawn), Position(2, 1), Position(3, 1), null)
      move.isEnPassant must beFalse
    }
  }

}
