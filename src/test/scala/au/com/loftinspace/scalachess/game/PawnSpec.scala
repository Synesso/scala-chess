package au.com.loftinspace.scalachess.game

import Positioning._

object PawnSpec extends GameSpecification {
  "a black pawn on rank 7" should {

/*
    val blackPawn = Piece(Black, Pawn)
    val board = new Board
*/

/*
    "be able to move towards rank 1 by 1 or 2 squares, or can capture diagonally towards rank 1 by 1 square" in {
      blackPawn.movesFrom(position('e7)).keySet must containPositionLists(List('e6, 'e5), List('d6), List('f6))
    }
*/

/*
    "require that positions can only be moved to if they aren't occupied by the same colour" in {
      blackPawn.movesFrom(position('d4)).elements.foreach {
        element =>
                val position = element._1(0)
                val query = element._2._1
                query(board, position) must_== Continue
                query(new Board(board.pieces(position) = Piece(Black, Pawn), Nil), position) must_== IncludeAndStop
                query(new Board(board.pieces(position) = Piece(White, Pawn), Nil), position) must_== Stop
      }
    }

    "invoke the correct board movements if the option is taken" in {
      blackPawn.movesFrom(position('e4)).elements.foreach {
        element =>
                val toPosition = element._1(0)
                val implication = element._2._2
                val boardAfterMove = boardWithBlackPawnAt(toPosition)
                val boardWithBlackPieceAtTarget = new Board(board.pieces(toPosition) = Piece(Black, Pawn), Nil)
                val boardWithWhitePieceAtTarget = new Board(board.pieces(toPosition) = Piece(White, Pawn), Nil)
                val boardAfterPieceIsTaken = new Board(boardWithBlackPawnAt(toPosition).pieces, Piece(Black, Pawn) :: Nil)
                implication(board, toPosition) must_== boardAfterMove
                implication(boardWithWhitePieceAtTarget, toPosition) must throwAn[IllegalMoveException]
                implication(boardWithBlackPieceAtTarget, toPosition) must_== boardAfterPieceIsTaken
      }
    }
*/
  }
}