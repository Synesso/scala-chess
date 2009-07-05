package au.com.loftinspace.scalachess.game

import Positioning._
import org.specs.Specification

object BoardSpecs extends Specification {

  val board = new Board

/*
  "the board helper object" should {

    "advise whether a position is unoccupied on a board" in {
      Board.unoccupied(board, position('a1)) must beTrue
      Board.unoccupied((board place Piece(White, Knight) at 'e7), position('e7)) must beFalse
    }

    "advise whether a position is occupied by a black piece" in {
      Board.occupiedBy(board, position('f3), Black) must beFalse
      Board.occupiedBy((board place Piece(White, King) at 'f3), position('f3), Black) must beFalse
      Board.occupiedBy((board place Piece(Black, King) at 'f3), position('f3), Black) must beTrue
    }

    "advise whether a position is occupied by a white piece" in {
      Board.occupiedBy(board, position('f3), White) must beFalse
      Board.occupiedBy((board place Piece(White, King) at 'f3), position('f3), White) must beTrue
      Board.occupiedBy((board place Piece(Black, King) at 'f3), position('f3), White) must beFalse
    }
  }
*/

  "a board" should {

    "have no pieces by default" in { board.pieces must beEmpty }
    "have nothing taken by default" in { board.taken must beEmpty }

    "allow a piece to be placed" in {
      val updatedBoard = board place Piece(White, Rook) at 'e3
      updatedBoard.pieces must_== Map(position('e3) -> Piece(White, Rook))
    }

    "allow a piece to be taken" in {
      var updatedBoard = board place Piece(White, Rook) at 'e3
      updatedBoard = updatedBoard take position('e3)
      updatedBoard.pieces must beEmpty
      updatedBoard.taken must_== Piece(White, Rook) :: Nil
    }

    "allow a pawn to be promoted to a queen" in {
      var updatedBoard = board place Piece(Black, Pawn) at 'a8
      updatedBoard = updatedBoard promote position('a8) to Queen
      updatedBoard.pieces must_== Map(position('a8) -> Piece(Black, Queen))
    }

    "allow a pawn to be promoted to a bishop" in {
      var updatedBoard = board place Piece(Black, Pawn) at 'f8
      updatedBoard = updatedBoard promote position('f8) to Bishop
      updatedBoard.pieces must_== Map(position('f8) -> Piece(Black, Bishop))
    }

    "allow a pawn to be promoted to a knight" in {
      var updatedBoard = board place Piece(White, Pawn) at 'g1
      updatedBoard = updatedBoard promote position('g1) to Knight
      updatedBoard.pieces must_== Map(position('g1) -> Piece(White, Knight))
    }

    "allow a pawn to be promoted to a rook" in {
      var updatedBoard = board place Piece(White, Pawn) at 'd1
      updatedBoard = updatedBoard promote position('d1) to Rook
      updatedBoard.pieces must_== Map(position('d1) -> Piece(White, Rook))
    }

    "no-op when promoting a pawn to a pawn" in {
      var updatedBoard = board place Piece(White, Pawn) at 'd2
      updatedBoard = updatedBoard promote position('d2) to Pawn
      updatedBoard.pieces must_== Map(position('d2) -> Piece(White, Pawn))
    }

    "not allow a pawn to be promoted to a king" in {
      val updatedBoard = board place Piece(White, Pawn) at 'd2
      (updatedBoard promote position('d2) to King) must throwAn[IllegalPromotionException]
    }

    "not allow an empty position to be promoted" in {
      (board promote position('a6) to Queen) must throwAn[IllegalPromotionException]
    }

    "not allow a non-pawn to be promoted" in {
      val updatedBoard = board place Piece(Black, Knight) at 'f7
      (updatedBoard promote position('f7) to Queen) must throwAn[IllegalPromotionException]
    }

    "allow a piece to move" in {
      var updatedBoard = board place Piece(Black, Rook) at 'h5
      updatedBoard = updatedBoard move position('h5) to position('h8)
      updatedBoard.pieces must_== Map(position('h8) -> Piece(Black, Rook))
    }

    "not allow an empty position to move" in {
      (board move position('a5) to position('a6)) must throwAn[IllegalMoveException]
    }

    "not allow a piece to move to an occupied position" in {
      var updatedBoard = board place Piece(White, Pawn) at position('e1)
      updatedBoard = updatedBoard place Piece(Black, Bishop) at position('g3)
      (updatedBoard move position('g3) to position('e1)) must throwAn[IllegalMoveException]
    }

    "be able to be reset to the starting game layout" in {
      val newBoard = board.reset
      newBoard.pieces must_== Map(
        position('a1) -> Piece(White, Rook),
        position('b1) -> Piece(White, Knight),
        position('c1) -> Piece(White, Bishop),
        position('d1) -> Piece(White, Queen),
        position('e1) -> Piece(White, King),
        position('f1) -> Piece(White, Bishop),
        position('g1) -> Piece(White, Knight),
        position('h1) -> Piece(White, Rook),
        position('a2) -> Piece(White, Pawn),
        position('b2) -> Piece(White, Pawn),
        position('c2) -> Piece(White, Pawn),
        position('d2) -> Piece(White, Pawn),
        position('e2) -> Piece(White, Pawn),
        position('f2) -> Piece(White, Pawn),
        position('g2) -> Piece(White, Pawn),
        position('h2) -> Piece(White, Pawn),
        position('a7) -> Piece(Black, Pawn),
        position('b7) -> Piece(Black, Pawn),
        position('c7) -> Piece(Black, Pawn),
        position('d7) -> Piece(Black, Pawn),
        position('e7) -> Piece(Black, Pawn),
        position('f7) -> Piece(Black, Pawn),
        position('g7) -> Piece(Black, Pawn),
        position('h7) -> Piece(Black, Pawn),
        position('a8) -> Piece(Black, Rook),
        position('b8) -> Piece(Black, Knight),
        position('c8) -> Piece(Black, Bishop),
        position('d8) -> Piece(Black, Queen),
        position('e8) -> Piece(Black, King),
        position('f8) -> Piece(Black, Bishop),
        position('g8) -> Piece(Black, Knight),
        position('h8) -> Piece(Black, Rook)
      )
    }

  }
}