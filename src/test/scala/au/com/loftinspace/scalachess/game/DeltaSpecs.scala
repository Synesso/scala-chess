package au.com.loftinspace.scalachess.game

import Positioning._

object DeltaSpecs extends GameSpecification {
  "a delta" should {

    "provide no en passant target when it is not a pawn move" in {
      val delta = Delta(Map(
        position('a2) -> (Some(Piece(White, King)), None),
        position('a4) -> (None, Some(Piece(White, King)))), None)
      delta.enPassantTo must beNone
    }

    "provide no en passant target when it is not an enpassant pawn move" in {
      val delta = Delta(Map(
        position('a3) -> (Some(Piece(Black, Pawn)), None),
        position('a4) -> (None, Some(Piece(Black, Pawn)))), None)
      delta.enPassantTo must beNone
    }

    "provide no en passant target when it is on the wrong rank" in {
      val delta = Delta(Map(
        position('a3) -> (Some(Piece(White, Pawn)), None),
        position('a5) -> (None, Some(Piece(White, Pawn)))), None)
      delta.enPassantTo must beNone
    }

    "provide the correct en passant target for a white pawn" in {
      val delta = Delta(Map(
        position('f2) -> (Some(Piece(White, Pawn)), None),
        position('f4) -> (None, Some(Piece(White, Pawn)))), None)
      delta.enPassantTo must_== Some(position('f4))
    }

    "provide the correct en passant target for a black pawn" in {
      val delta = Delta(Map(
        position('h7) -> (Some(Piece(Black, Pawn)), None),
        position('h5) -> (None, Some(Piece(Black, Pawn)))), None)
      delta.enPassantTo must_== Some(position('h5))
    }

    "provide no en passant target for a white pawn doing a black en passant" in {
      val delta = Delta(Map(
        position('f2) -> (Some(Piece(Black, Pawn)), None),
        position('f4) -> (None, Some(Piece(Black, Pawn)))), None)
      delta.enPassantTo must beNone
    }

    "provide no en passant target for a black pawn doing a white en passant" in {
      val delta = Delta(Map(
        position('h7) -> (Some(Piece(White, Pawn)), None),
        position('h5) -> (None, Some(Piece(White, Pawn)))), None)
      delta.enPassantTo must beNone
    }

  }

}