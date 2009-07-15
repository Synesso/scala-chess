package au.com.loftinspace.scalachess.game

import Positioning._

object BishopSpec extends GameSpecification {
  "a bishop" should {

    val bishop = Piece(White, Bishop)
    def boardWithBishopAt(pos: Position) = new Board(Map(pos -> bishop), Nil)
    val board = boardWithBishopAt(position('e4))

    "be able to move in any of 8 positions, 2 and 1 squares away" in {
      bishop.movesFrom(position('e4)).keySet must containPositionLists(
        List('f3, 'g2, 'h1), List('d5, 'c6, 'b7, 'a8), List('d3, 'c2, 'b1), List('f5, 'g6, 'h7))
    }

    "be able to move in any of 8 positions, 2 and 1 squares away, even when at the edges" in {
      bishop.movesFrom(position('h7)).keySet must containPositionLists(List('g8), List('g6, 'f5, 'e4, 'd3, 'c2, 'b1))
    }

    "require that positions can only be moved to if they aren't occupied by the same colour" in {
      bishop.movesFrom(position('d4)).elements.foreach {
        element =>
                val query = element._2._1
                element._1.foreach {
                  position =>
                          query(board, position, Nil) must_== Continue
                          query(new Board(board.pieces(position) = Piece(Black, Pawn), Nil), position, Nil) must_== IncludeAndStop
                          query(new Board(board.pieces(position) = Piece(White, Pawn), Nil), position, Nil) must_== Stop
                }
      }
    }

    "invoke the correct board movements if the option is taken" in {
      bishop.movesFrom(position('e4)).elements.foreach {
        element =>
                val implication = element._2._2
                element._1.foreach {
                  toPosition =>
                          val boardAfterMove = boardWithBishopAt(toPosition)
                          val boardWithBlackPieceAtTarget = new Board(board.pieces(toPosition) = Piece(Black, Pawn), Nil)
                          val boardWithWhitePieceAtTarget = new Board(board.pieces(toPosition) = Piece(White, Pawn), Nil)
                          val boardAfterPieceIsTaken = new Board(boardWithBishopAt(toPosition).pieces, Piece(Black, Pawn) :: Nil)
                          implication(board, toPosition, Nil) must_== boardAfterMove
                          implication(boardWithWhitePieceAtTarget, toPosition, Nil) must throwAn[IllegalMoveException]
                          implication(boardWithBlackPieceAtTarget, toPosition, Nil) must_== boardAfterPieceIsTaken
                }
      }
    }
  }
}