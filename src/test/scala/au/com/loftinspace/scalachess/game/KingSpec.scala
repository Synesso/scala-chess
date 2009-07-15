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

    "be able to castle if it is in its starting position" in {
      king.movesFrom(position('e8)).keySet must containPositionLists(List('c8), List('g8), List('d8), List('d7), List('e7), List('f7), List('f8))
      king.movesFrom(position('e1)).keySet must containPositionLists(List('d1), List('d2), List('e2), List('f2), List('f1))
      Piece(White, King).movesFrom(position('e8)).keySet must containPositionLists(List('d8), List('d7), List('e7), List('f7), List('f8))
      Piece(White, King).movesFrom(position('e1)).keySet must containPositionLists(List('c1), List('g1), List('d1), List('d2), List('e2), List('f2), List('f1))
    }

    "require that positions can only be moved to if they aren't occupied by the same colour" in {
      val board = new Board
      king.movesFrom(position('d4)).elements.foreach {
        element =>
                val query = element._2._1
                element._1.foreach {
                  position =>
                          query(board, position, Nil) must_== Continue
                          query(new Board(board.pieces(position) = Piece(White, Pawn), Nil), position, Nil) must_== IncludeAndStop
                          query(new Board(board.pieces(position) = Piece(Black, Pawn), Nil), position, Nil) must_== Stop
                }
      }
    }

    "require that castling moves can be done if neither king nor rook have moved and the king does not pass through check" in {
      val board = new Board().place(king).at('e8).place(rook).at('a8)
      val query = king.movesFrom(position('e8))(List(position('c8)))._1
      query(board, position('c8), Nil) must_== IncludeAndStop
    }

    "require that castling moves can not be done if the king has moved (black, queen-side)" in {
      val board = new Board().place(king).at('e8).place(rook).at('a8)
      val query = king.movesFrom(position('e8))(List(position('c8)))._1
      var moves = List(History(Move(king, position('e8), position('f8)), None), History(Move(king, position('f8), position('e8)), None))
      query(board, position('c8), moves) must_== Stop
    }

    "require that castling moves can not be done if the king has moved (black, king-side)" in {
      val board = new Board().place(king).at('e8).place(rook).at('h8)
      val query = king.movesFrom(position('e8))(List(position('g8)))._1
      var moves = List(History(Move(king, position('e8), position('f8)), None), History(Move(king, position('f8), position('e8)), None))
      query(board, position('g8), moves) must_== Stop
    }

    "require that castling moves can not be done if the king has moved (white, queen-side)" in {
      val king = Piece(White, King)
      val rook = Piece(White, Rook)
      val board = new Board().place(king).at('e1).place(rook).at('a1)
      val query = king.movesFrom(position('e1))(List(position('c1)))._1
      var moves = List(History(Move(king, position('e1), position('f1)), None), History(Move(king, position('f1), position('e1)), None))
      query(board, position('c1), moves) must_== Stop
    }

    "require that castling moves can not be done if the king has moved (white, king-side)" in {
      val king = Piece(White, King)
      val rook = Piece(White, Rook)
      val board = new Board().place(king).at('e1).place(rook).at('h1)
      val query = king.movesFrom(position('e1))(List(position('g1)))._1
      var moves = List(History(Move(king, position('e1), position('f1)), None), History(Move(king, position('f1), position('e1)), None))
      query(board, position('g1), moves) must_== Stop
    }

    "require that castling moves can not be done if the rook has moved (black, queen-side)" in {
      val board = new Board().place(king).at('e8).place(rook).at('a8)
      val query = king.movesFrom(position('e8))(List(position('c8)))._1
      var moves = List(History(Move(rook, position('a8), position('a5)), None), History(Move(king, position('a5), position('a8)), None))
      query(board, position('c8), moves) must_== Stop
    }

    "require that castling moves can not be done if the rook has moved (black, king-side)" in {
      val board = new Board().place(king).at('e8).place(rook).at('h8)
      val query = king.movesFrom(position('e8))(List(position('g8)))._1
      var moves = List(History(Move(rook, position('h8), position('h5)), None), History(Move(king, position('h5), position('h8)), None))
      query(board, position('g8), moves) must_== Stop
    }

    "require that castling moves can not be done if the rook has moved (white, queen-side)" in {
      val king = Piece(White, King)
      val rook = Piece(White, Rook)
      val board = new Board().place(king).at('e1).place(rook).at('a1)
      val query = king.movesFrom(position('e1))(List(position('c1)))._1
      var moves = List(History(Move(rook, position('a1), position('a5)), None), History(Move(king, position('a5), position('a1)), None))
      query(board, position('c1), moves) must_== Stop
    }

    "require that castling moves can not be done if the rook has moved (white, king-side)" in {
      val king = Piece(White, King)
      val rook = Piece(White, Rook)
      val board = new Board().place(king).at('e1).place(rook).at('h1)
      val query = king.movesFrom(position('e1))(List(position('g1)))._1
      var moves = List(History(Move(rook, position('h1), position('h5)), None), History(Move(king, position('h5), position('h1)), None))
      query(board, position('g1), moves) must_== Stop
    }

    "require that castling moves can not be done if there are intervening pieces (black, queen-side)" in {
      val board = new Board().place(king).at('e8).place(rook).at('a8).place(rook).at('b8)
      val query = king.movesFrom(position('e8))(List(position('c8)))._1
      query(board, position('c8), Nil) must_== Stop
    }

    "require that castling moves can not be done if there are intervening pieces (black, king-side)" in {
      val board = new Board().place(king).at('e8).place(rook).at('h8).place(rook).at('g8)
      val query = king.movesFrom(position('e8))(List(position('g8)))._1
      query(board, position('g8), Nil) must_== Stop
    }

    "require that castling moves can not be done if there are intervening pieces (white, queen-side)" in {
      val king = Piece(White, King)
      val rook = Piece(White, Rook)
      val board = new Board().place(king).at('e1).place(rook).at('a1).place(rook).at('b1)
      val query = king.movesFrom(position('e1))(List(position('c1)))._1
      query(board, position('c1), Nil) must_== Stop
    }

    "require that castling moves can not be done if there are intervening pieces (white, king-side)" in {
      val king = Piece(White, King)
      val rook = Piece(White, Rook)
      val board = new Board().place(king).at('e1).place(rook).at('h1).place(rook).at('g1)
      val query = king.movesFrom(position('e1))(List(position('g1)))._1
      query(board, position('g1), Nil) must_== Stop
    }

    "require that castling moves can not be done if the king is in check (black, queen-side)" in {
      val board = new Board().place(king).at('e8).place(rook).at('a8).place(Piece(White, Rook)).at('e1)
      val query = king.movesFrom(position('e8))(List(position('c8)))._1
      query(board, position('c8), Nil) must_== Stop
    }

    "require that castling moves can not be done if the king is in check (black, king-side)" in {
      val board = new Board().place(king).at('e8).place(rook).at('h8).place(Piece(White, Rook)).at('e1)
      val query = king.movesFrom(position('e8))(List(position('g8)))._1
      query(board, position('g8), Nil) must_== Stop
    }

    "require that castling moves can not be done if the king is in check (white, queen-side)" in {
      val king = Piece(White, King)
      val rook = Piece(White, Rook)
      val board = new Board().place(king).at('e1).place(rook).at('a1).place(Piece(Black, Rook)).at('e1)
      val query = king.movesFrom(position('e1))(List(position('c1)))._1
      query(board, position('c1), Nil) must_== Stop
    }

    "require that castling moves can not be done if the king is in check (white, king-side)" in {
      val king = Piece(White, King)
      val rook = Piece(White, Rook)
      val board = new Board().place(king).at('e1).place(rook).at('h1).place(Piece(Black, Rook)).at('e1)
      val query = king.movesFrom(position('e1))(List(position('g1)))._1
      query(board, position('g1), Nil) must_== Stop
    }

    "require that castling moves can not be done if the king passes through check (black, queen-side)" in {
      val board = new Board().place(king).at('e8).place(rook).at('a8).place(Piece(White, Rook)).at('d1)
      val query = king.movesFrom(position('e8))(List(position('c8)))._1
      query(board, position('c8), Nil) must_== Stop
    }

    "require that castling moves can not be done if the king passes through check (black, king-side)" in {
      val board = new Board().place(king).at('e8).place(rook).at('h8).place(Piece(White, Rook)).at('f1)
      val query = king.movesFrom(position('e8))(List(position('g8)))._1
      query(board, position('g8), Nil) must_== Stop
    }

    "require that castling moves can not be done if the king passes through check (white, queen-side)" in {
      val king = Piece(White, King)
      val rook = Piece(White, Rook)
      val board = new Board().place(king).at('e1).place(rook).at('a1).place(Piece(Black, Rook)).at('d1)
      val query = king.movesFrom(position('e1))(List(position('c1)))._1
      query(board, position('c1), Nil) must_== Stop
    }

    "require that castling moves can not be done if the king passes through check (white, king-side)" in {
      val king = Piece(White, King)
      val rook = Piece(White, Rook)
      val board = new Board().place(king).at('e1).place(rook).at('h1).place(Piece(Black, Rook)).at('f1)
      val query = king.movesFrom(position('e1))(List(position('g1)))._1
      query(board, position('g1), Nil) must_== Stop
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
                          implication(board, toPosition, Nil) must_== boardAfterMove
                          implication(boardWithBlackPieceAtTarget, toPosition, Nil) must throwAn[IllegalMoveException]
                          implication(boardWithWhitePieceAtTarget, toPosition, Nil) must_== boardAfterPieceIsTaken
                }
      }
    }


  }
}
