package au.com.loftinspace.scalachess.game

import Positioning._

object KingSpec extends GameSpecification {
  "a king" should {
    val king = Piece(Black, King)
    val rook = Piece(Black, Rook)
    def boardWithKingAt(pos: Position) = new Board(Map(pos -> king), Nil)
    val board = boardWithKingAt(position('d4))

    "be able to move 1 position in any direction" in {
      king.movesFrom(position('d4)).keySet must containPositionLists(List('d3), List('c3), List('c4), List('c5), List('d5), List('e5), List('e4), List('e3))
    }

    "be able to move 1 position in any direction, even from the edges" in {
      king.movesFrom(position('h8)).keySet must containPositionLists(List('h7), List('g7), List('g8))
    }

    "be able to castle if it hasn't moved" in {
      king.movesFrom(position('e8)).keySet must containPositionLists(List('c8), List('g8), List('d8), List('d7), List('e7), List('f7), List('f8))
    }

    "require that positions can only be moved to if they aren't occupied by the same colour" in {
      val board = new Board
      king.movesFrom(position('d4)).elements.foreach {
        element =>
                val query = element._2._1
                element._1.foreach {
                  position =>
                          query(board, position, None) must_== Continue
                          query(new Board(board.pieces(position) = Piece(White, Pawn), Nil), position, None) must_== IncludeAndStop
                          query(new Board(board.pieces(position) = Piece(Black, Pawn), Nil), position, None) must_== Stop
                }
      }
    }

    // todo - many castling moves to spec out after history refactoring.
    "require that castling moves can be done if neither king nor rook have moved and the king does not pass through check" in {
//      val board = new Board().place(king).at('e8).place(rook).at('a8)
//      val query = king.movesFrom(position('e8))(List(position('c8)))._1
//      query(board, position('c8), None) must_== IncludeAndStop
    }

    "require that castling moves can not be done if the king has moved" in {
//      val board = new Board().place(king).at('e8).place(rook).at('a8).move(position('e8)).to(position('e7)).move(position('e7)).to(position('e8))
//      val query = king.movesFrom(position('e8))(List(position('c8)))._1
//      query(board, position('c8), None) must_== Stop
    }

    "require that castling moves can not be done if the rook has moved" in {
/*
      val board = new Board().place(king).at('e8).place(rook).at('a8).move('a8).to('a5).move('a5).to('a8)
      val query = king.movesFrom(position('e8))(List('c8))._1
      query(board, position('c8), None) must_== Stop
*/
    }

    "invoke the correct board movements if the option is taken" in {
      king.movesFrom(position('d4)).elements.foreach {
        element =>
                val implication = element._2._2
                element._1.foreach {
                  toPosition =>
                          val boardAfterMove = boardWithKingAt(toPosition)
                          val boardWithBlackPieceAtTarget = new Board(board.pieces(toPosition) = Piece(Black, Pawn), Nil)
                          val boardWithWhitePieceAtTarget = new Board(board.pieces(toPosition) = Piece(White, Pawn), Nil)
                          val boardAfterPieceIsTaken = new Board(boardWithKingAt(toPosition).pieces, Piece(White, Pawn) :: Nil)
                          implication(board, toPosition, None) must_== boardAfterMove
                          implication(boardWithBlackPieceAtTarget, toPosition, None) must throwAn[IllegalMoveException]
                          implication(boardWithWhitePieceAtTarget, toPosition, None) must_== boardAfterPieceIsTaken
                }
      }
    }


  }
}
