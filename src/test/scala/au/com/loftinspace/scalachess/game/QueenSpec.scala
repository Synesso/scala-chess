package au.com.loftinspace.scalachess.game

import Positioning._

object QueenSpec extends GameSpecification {
  "a queen" should {

    val queen = Piece(Black, Queen)
    def boardWithQueenAt(pos: Position) = new Board(Map(pos -> queen), Nil)
    val board = boardWithQueenAt(position('d4))


    "be able to move in any direction until the edge of the board" in {
      queen.movesFrom(position('d4)).keySet must containPositionLists(
        List('d5, 'd6, 'd7, 'd8), List('d3, 'd2, 'd1), List('e4, 'f4, 'g4, 'h4), List('c4, 'b4, 'a4),
        List('c3, 'b2, 'a1), List('e5, 'f6, 'g7, 'h8), List('c5, 'b6, 'a7), List('e3, 'f2, 'g1))
    }

    "be able to move 1 position in any direction, even from the edges" in {
      queen.movesFrom(position('h8)).keySet must containPositionLists(
        List('h7, 'h6, 'h5, 'h4, 'h3, 'h2, 'h1),
        List('g7, 'f6, 'e5, 'd4, 'c3, 'b2, 'a1),
        List('g8, 'f8, 'e8, 'd8, 'c8, 'b8, 'a8))
    }

    "require that positions can only be moved to if they aren't occupied by the same colour" in {
      queen.movesFrom(position('d4)).elements.foreach {
        element =>
                val query = element._2._1
                element._1.foreach {
                  position =>
                          query(board, position) must_== Continue
                          query(new Board(board.pieces(position) = Piece(White, Pawn), Nil), position) must_== IncludeAndStop
                          query(new Board(board.pieces(position) = Piece(Black, Pawn), Nil), position) must_== Stop
                }
      }
    }

    "invoke the correct board movements if the option is taken" in {
      queen.movesFrom(position('d4)).elements.foreach {
        element =>
                val implication = element._2._2
                element._1.foreach {
                  toPosition =>
                          val boardAfterMove = boardWithQueenAt(toPosition)
                          val boardWithBlackPieceAtTarget = new Board(board.pieces(toPosition) = Piece(Black, Pawn), Nil)
                          val boardWithWhitePieceAtTarget = new Board(board.pieces(toPosition) = Piece(White, Pawn), Nil)
                          val boardAfterPieceIsTaken = new Board(boardWithQueenAt(toPosition).pieces, Piece(White, Pawn) :: Nil)
                          implication(board, toPosition) must_== boardAfterMove
                          implication(boardWithBlackPieceAtTarget, toPosition) must throwAn[IllegalMoveException]
                          implication(boardWithWhitePieceAtTarget, toPosition) must_== boardAfterPieceIsTaken
                }
      }
    }
  }
}
