package au.com.loftinspace.scalachess.game

import Positioning._

object KingSpec extends GameSpecification {
  "a king" should {
    val king = Piece(Black, King)

    "be able to move 1 position in any direction" in {
      king.movesFrom(position('d4)).keySet must containPositionLists(List('d3), List('c3), List('c4), List('c5), List('d5), List('e5), List('e4), List('e3))
    }

    "be able to move 1 position in any direction, even from the edges" in {
      king.movesFrom(position('h8)).keySet must containPositionLists(List('h7), List('g7), List('g8))
    }

    "require that positions can only be moved to if they aren't occupied by the same colour" in {
      val board = new Board
      king.movesFrom(position('d4)).elements.foreach {
        element =>
                val position = element._1(0)
                val query = element._2._1
                query(board, position) must_== Continue
                query(new Board(board.pieces(position) = Piece(White, Pawn), Nil), position) must_== IncludeAndStop
                query(new Board(board.pieces(position) = Piece(Black, Pawn), Nil), position) must_== Stop
      }
    }

    // todo - castling
  }
}