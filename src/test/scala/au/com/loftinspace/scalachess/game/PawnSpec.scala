package au.com.loftinspace.scalachess.game

import Positioning._

object PawnSpec extends GameSpecification {
  val blackPawn = Piece(Black, Pawn)
  val whitePawn = Piece(White, Pawn)

  "a black pawn on rank 7" should {

    val board = new Board(Map(position('d7) -> blackPawn), Nil)

    "be able to move towards rank 1 by 1 or 2 squares, or can capture diagonally towards rank 1 by 1 square" in {
      blackPawn.movesFrom(position('e7)).keySet must containPositionLists(List('e6, 'e5), List('d6), List('f6))
    }

    "require that forward positions can only be moved to if they aren't occupied by the same colour" in {
      blackPawn.movesFrom(position('d7)).elements.foreach {
        element =>
                val query = element._2._1
                element._1.filter(_.file.equals(4)).foreach {
                  position =>
                          query(board, position, Nil) must_== Continue
                          query(new Board(board.pieces(position) = Piece(Black, Pawn), Nil), position, Nil) must_== Stop
                          query(new Board(board.pieces(position) = Piece(White, Pawn), Nil), position, Nil) must_== Stop
                }
      }
    }

    "require that diagonal positions can only be moved to if they are occupied by a piece of the opposing colour" in {
      blackPawn.movesFrom(position('d7)).elements.foreach {
        element =>
                val query = element._2._1
                element._1.filter(!_.file.equals(4)).foreach {
                  position =>
                          query(board, position, Nil) must_== Stop
                          query(new Board(board.pieces(position) = Piece(Black, Pawn), Nil), position, Nil) must_== Stop
                          query(new Board(board.pieces(position) = Piece(White, Pawn), Nil), position, Nil) must_== IncludeAndStop
                }
      }
    }

    "invoke the correct board movements if the option is taken to move forward" in {
      blackPawn.movesFrom(position('d7)).elements.foreach {
        element =>
                val implication = element._2._2
                element._1.filter(_.file.equals(4)).foreach {
                  toPosition =>
                          val boardAfterMove = new Board(Map(toPosition -> blackPawn), Nil)
                          val boardWithBlackPieceAtTarget = new Board(board.pieces(toPosition) = Piece(Black, Pawn), Nil)
                          val boardWithWhitePieceAtTarget = new Board(board.pieces(toPosition) = Piece(White, Pawn), Nil)
                          implication(board, toPosition, Nil) must_== boardAfterMove
                          implication(boardWithWhitePieceAtTarget, toPosition, Nil) must throwAn[IllegalMoveException]
                          implication(boardWithBlackPieceAtTarget, toPosition, Nil) must throwAn[IllegalMoveException]
                }
      }
    }

    "invoke the correct board movements if the option is taken to move diagonally" in {
      blackPawn.movesFrom(position('d7)).elements.foreach {
        element =>
                val implication = element._2._2
                element._1.filter(!_.file.equals(4)).foreach {
                  toPosition =>
                          val boardAfterTaking = new Board(Map(toPosition -> blackPawn), List(Piece(White, Pawn)))
                          val boardWithBlackPieceAtTarget = new Board(board.pieces(toPosition) = Piece(Black, Pawn), Nil)
                          val boardWithWhitePieceAtTarget = new Board(board.pieces(toPosition) = Piece(White, Pawn), Nil)
                          implication(board, toPosition, Nil) must throwAn[IllegalMoveException]
                          implication(boardWithBlackPieceAtTarget, toPosition, Nil) must throwAn[IllegalMoveException]
                          implication(boardWithWhitePieceAtTarget, toPosition, Nil) must_== boardAfterTaking
                }
      }
    }
  }

  "a white pawn on rank 7" should {
    val board = new Board(Map(position('d7) -> whitePawn), Nil)

    "be able to move towards rank 8 by 1 square, or can capture diagonally towards rank 8 by 1 square" in {
      whitePawn.movesFrom(position('e7)).keySet must containPositionLists(List('e8), List('d8), List('f8))
    }

    "require that forward positions can only be moved to if they aren't occupied by the same colour" in {
      whitePawn.movesFrom(position('d7)).elements.foreach {
        element =>
                val query = element._2._1
                element._1.filter(_.file.equals(4)).foreach {
                  position =>
                          query(board, position, Nil) must_== Continue
                          query(new Board(board.pieces(position) = Piece(Black, Pawn), Nil), position, Nil) must_== Stop
                          query(new Board(board.pieces(position) = Piece(White, Pawn), Nil), position, Nil) must_== Stop
                }
      }
    }

    "require that diagonal positions can only be moved to if they are occupied by a piece of the opposing colour" in {
      whitePawn.movesFrom(position('d7)).elements.foreach {
        element =>
                val query = element._2._1
                element._1.filter(!_.file.equals(4)).foreach {
                  position =>
                          query(board, position, Nil) must_== Stop
                          query(new Board(board.pieces(position) = Piece(Black, Pawn), Nil), position, Nil) must_== IncludeAndStop
                          query(new Board(board.pieces(position) = Piece(White, Pawn), Nil), position, Nil) must_== Stop
                }
      }
    }

    "invoke the correct board movements if the option is taken to move forward" in {
      whitePawn.movesFrom(position('d7)).elements.foreach {
        element =>
                val implication = element._2._2
                element._1.filter(_.file.equals(4)).foreach {
                  toPosition =>
                          val boardAfterMove = new Board(Map(toPosition -> whitePawn), Nil)
                          val boardWithBlackPieceAtTarget = new Board(board.pieces(toPosition) = Piece(Black, Pawn), Nil)
                          val boardWithWhitePieceAtTarget = new Board(board.pieces(toPosition) = Piece(White, Pawn), Nil)
                          implication(board, toPosition, Nil) must_== boardAfterMove
                          implication(boardWithWhitePieceAtTarget, toPosition, Nil) must throwAn[IllegalMoveException]
                          implication(boardWithBlackPieceAtTarget, toPosition, Nil) must throwAn[IllegalMoveException]
                }
      }
    }

    "invoke the correct board movements if the option is taken to move diagonally" in {
      whitePawn.movesFrom(position('d7)).elements.foreach {
        element =>
                val implication = element._2._2
                element._1.filter(!_.file.equals(4)).foreach {
                  toPosition =>
                          val boardAfterTaking = new Board(Map(toPosition -> whitePawn), List(Piece(Black, Pawn)))
                          val boardWithBlackPieceAtTarget = new Board(board.pieces(toPosition) = Piece(Black, Pawn), Nil)
                          val boardWithWhitePieceAtTarget = new Board(board.pieces(toPosition) = Piece(White, Pawn), Nil)
                          implication(board, toPosition, Nil) must throwAn[IllegalMoveException]
                          implication(boardWithWhitePieceAtTarget, toPosition, Nil) must throwAn[IllegalMoveException]
                          implication(boardWithBlackPieceAtTarget, toPosition, Nil) must_== boardAfterTaking
                }
      }
    }
  }

  "a black pawn" should {
    "only be able to move 2 steps forward on rank 7" in {
      (for (rank <- 2 to 6) yield Position(rank, 4)).foreach {
        origin =>
                val rank = origin.rank
                blackPawn.movesFrom(origin).keySet must containPositionLists(
                  List(Symbol("d" + (rank - 1))), List(Symbol("c" + (rank - 1))), List(Symbol("e" + (rank - 1))))
      }
    }
  }

  "a white pawn" should {
    "only be able to move 2 steps forward on rank 7" in {
      (for (rank <- 3 to 7) yield Position(rank, 4)).foreach {
        origin =>
                val rank = origin.rank
                whitePawn.movesFrom(origin).keySet must containPositionLists(
                  List(Symbol("d" + (rank + 1))), List(Symbol("c" + (rank + 1))), List(Symbol("e" + (rank + 1))))
      }
    }
  }

  "a black pawn on rank 4" should {
    val board = new Board().place(blackPawn).at('d4).place(whitePawn).at('c4)

    "be able to perform en passant if the adjacent piece is a pawn that has moved two spaces in the last move" in {
      val query = blackPawn.movesFrom(position('d4))(List(position('c3)))._1
      val lastMove = History(Move(whitePawn, position('c2), position('c4)), None)
      query(board, position('c3), List(lastMove)) must_== IncludeAndStop
    }

    "not be able to perform en passant if the adjacent piece is a pawn that has not moved two spaces in the last move" in {
      val query = blackPawn.movesFrom(position('d4))(List(position('c3)))._1
      val lastMove = History(Move(whitePawn, position('c3), position('c4)), None)
      query(board, position('c3), List(lastMove)) must_== Stop
    }

    "not be able to perform en passant if the adjacent piece is not a pawn" in {
      val query = blackPawn.movesFrom(position('d4))(List(position('c3)))._1
      val lastMove = History(Move(Piece(White, Bishop), position('c3), position('c4)), None)
      query(board, position('c3), List(lastMove)) must_== Stop
    }

    "not be able to perform en passant if there is no adjacent piece" in {
      val query = blackPawn.movesFrom(position('d4))(List(position('c3)))._1
      val lastMove = History(Move(Piece(White, Bishop), position('c3), position('c4)), None)
      query(new Board().place(blackPawn).at('d4), position('c3), List(lastMove)) must_== Stop
    }

    "not be able to perform en passant if there is no prior move" in {
      val query = blackPawn.movesFrom(position('d4))(List(position('c3)))._1
      query(new Board().place(blackPawn).at('d4), position('c3), Nil) must_== Stop
    }

    "invoke the correct board movements if the en passant option is taken" in {
      val boardAfterMove = board.take(position('c4)).move(position('d4)).to(position('c3))
      val implication = blackPawn.movesFrom(position('d4))(List(position('c3)))._2
      val lastMove = History(Move(whitePawn, position('c2), position('c4)), None)
      implication(board, position('c3), List(lastMove)) must_== boardAfterMove
    }
  }
  
  "a white pawn on rank 5" should {
    val board = new Board().place(whitePawn).at('d5).place(blackPawn).at('c5)

    "be able to perform en passant if the adjacent piece is a pawn that has moved two spaces in the last move" in {
      val query = whitePawn.movesFrom(position('d5))(List(position('c6)))._1
      val lastMove = History(Move(blackPawn, position('c7), position('c5)), None)
      query(board, position('c6), List(lastMove)) must_== IncludeAndStop
    }

    "not be able to perform en passant if the adjacent piece is a pawn that has not moved two spaces in the last move" in {
      val query = whitePawn.movesFrom(position('d5))(List(position('c6)))._1
      val lastMove = History(Move(blackPawn, position('c6), position('c5)), None)
      query(board, position('c6), List(lastMove)) must_== Stop
    }

    "not be able to perform en passant if the adjacent piece is not a pawn" in {
      val query = whitePawn.movesFrom(position('d5))(List(position('c6)))._1
      val lastMove = History(Move(Piece(White, Bishop), position('c6), position('c5)), None)
      query(board, position('c6), List(lastMove)) must_== Stop
    }

    "not be able to perform en passant if there is no adjacent piece" in {
      val query = whitePawn.movesFrom(position('d5))(List(position('c6)))._1
      val lastMove = History(Move(Piece(White, Bishop), position('c6), position('c5)), None)
      query(new Board().place(whitePawn).at('d5), position('c6), List(lastMove)) must_== Stop
    }

    "not be able to perform en passant if there is no prior move" in {
      val query = whitePawn.movesFrom(position('d5))(List(position('c6)))._1
      query(new Board().place(whitePawn).at('d5), position('c6), Nil) must_== Stop
    }

    "invoke the correct board movements if the en passant option is taken" in {
      val boardAfterMove = board.take(position('c5)).move(position('d5)).to(position('c6))
      val implication = whitePawn.movesFrom(position('d5))(List(position('c6)))._2
      val lastMove = History(Move(blackPawn, position('c7), position('c5)), None)
      implication(board, position('c6), List(lastMove)) must_== boardAfterMove
    }
  }
}