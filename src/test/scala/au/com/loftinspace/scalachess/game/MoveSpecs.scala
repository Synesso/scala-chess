package au.com.loftinspace.scalachess.game

import org.specs._
import Positioning._

object MoveSpec extends GameSpecification {
  "a move" should {

    "report no en passant when it is not a pawn move" in {
      val move = Move(Piece(White, Rook), position('a2), position('a4))
      move.isEnPassant must beFalse
    }

    "report no en passant when it is not an enpassant pawn move" in {
      val move = Move(Piece(Black, Pawn), position('b7), position('b6))
      move.isEnPassant must beFalse
    }

    "report no en passant when it is on the wrong rank" in {
      val move = Move(Piece(White, Pawn), position('c3), position('c5))
      move.isEnPassant must beFalse
    }

    "provide the correct en passant target for a white pawn" in {
      val move = Move(Piece(White, Pawn), position('f2), position('f4))
      move.isEnPassant must beTrue
    }

    "report en passant for a black pawn" in {
      val move = Move(Piece(Black, Pawn), position('h7), position('h5))
      move.isEnPassant must beTrue
    }

    "report no en passant for a white pawn doing a black en passant" in {
      val move = Move(Piece(Black, Pawn), position('f2), position('f4))
      move.isEnPassant must beFalse
    }

    "report no en passant for a black pawn doing a white en passant" in {
      val move = Move(Piece(White, Pawn), position('h7), position('h5))
      move.isEnPassant must beFalse
    }
  }
}
