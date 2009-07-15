package au.com.loftinspace.scalachess.game

import Positioning._

object KnightSpec extends GameSpecification {
  "a knight" should {

    val knight = Piece(White, Knight)
    def boardWithKnightAt(pos: Position) = new Board(Map(pos -> knight), Nil)
    val board = boardWithKnightAt(position('e4))

    "be able to move in any of 8 positions, 2 and 1 squares away" in {
      knight.movesFrom(position('e4)).keySet must containPositionLists(
        List('f6), List('g5), List('g3), List('f2), List('d2), List('c3), List('c5), List('d6))
    }

    "be able to move 2 and 1 squares away, even when at the edges" in {
      knight.movesFrom(position('h8)).keySet must containPositionLists(List('g6), List('f7))
    }

    "require that positions can only be moved to if they aren't occupied by the same colour" in {
      knight.movesFrom(position('d4)).elements.foreach {
        element =>
                val position = element._1(0)
                val query = element._2._1
                query(board, position, Nil) must_== Continue
                query(new Board(board.pieces(position) = Piece(Black, Pawn), Nil), position, Nil) must_== IncludeAndStop
                query(new Board(board.pieces(position) = Piece(White, Pawn), Nil), position, Nil) must_== Stop
      }
    }

    "invoke the correct board movements if the option is taken" in {
      knight.movesFrom(position('e4)).elements.foreach {
        element =>
                val toPosition = element._1(0)
                val implication = element._2._2
                val boardAfterMove = boardWithKnightAt(toPosition)
                val boardWithBlackPieceAtTarget = new Board(board.pieces(toPosition) = Piece(Black, Pawn), Nil)
                val boardWithWhitePieceAtTarget = new Board(board.pieces(toPosition) = Piece(White, Pawn), Nil)
                val boardAfterPieceIsTaken = new Board(boardWithKnightAt(toPosition).pieces, Piece(Black, Pawn) :: Nil)
                implication(board, toPosition, Nil) must_== boardAfterMove
                implication(boardWithWhitePieceAtTarget, toPosition, Nil) must throwAn[IllegalMoveException]
                implication(boardWithBlackPieceAtTarget, toPosition, Nil) must_== boardAfterPieceIsTaken
      }
    }
  }
}