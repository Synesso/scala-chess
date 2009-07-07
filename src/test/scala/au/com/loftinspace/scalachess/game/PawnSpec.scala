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
                          query(board, position) must_== Continue
                          query(new Board(board.pieces(position) = Piece(Black, Pawn), Nil), position) must_== Stop
                          query(new Board(board.pieces(position) = Piece(White, Pawn), Nil), position) must_== Stop
                }
      }
    }

    "require that diagonal positions can only be moved to if they are occupied by a piece of the opposing colour" in {
      blackPawn.movesFrom(position('d7)).elements.foreach {
        element =>
                val query = element._2._1
                element._1.filter(!_.file.equals(4)).foreach {
                  position =>
                          query(board, position) must_== Stop
                          query(new Board(board.pieces(position) = Piece(Black, Pawn), Nil), position) must_== Stop
                          query(new Board(board.pieces(position) = Piece(White, Pawn), Nil), position) must_== IncludeAndStop
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
                          implication(board, toPosition) must_== boardAfterMove
                          implication(boardWithWhitePieceAtTarget, toPosition) must throwAn[IllegalMoveException]
                          implication(boardWithBlackPieceAtTarget, toPosition) must throwAn[IllegalMoveException]
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
                          implication(board, toPosition) must throwAn[IllegalMoveException]
                          implication(boardWithBlackPieceAtTarget, toPosition) must throwAn[IllegalMoveException]
                          implication(boardWithWhitePieceAtTarget, toPosition) must_== boardAfterTaking
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
                          query(board, position) must_== Continue
                          query(new Board(board.pieces(position) = Piece(Black, Pawn), Nil), position) must_== Stop
                          query(new Board(board.pieces(position) = Piece(White, Pawn), Nil), position) must_== Stop
                }
      }
    }

    "require that diagonal positions can only be moved to if they are occupied by a piece of the opposing colour" in {
      whitePawn.movesFrom(position('d7)).elements.foreach {
        element =>
                val query = element._2._1
                element._1.filter(!_.file.equals(4)).foreach {
                  position =>
                          query(board, position) must_== Stop
                          query(new Board(board.pieces(position) = Piece(Black, Pawn), Nil), position) must_== IncludeAndStop
                          query(new Board(board.pieces(position) = Piece(White, Pawn), Nil), position) must_== Stop
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
                          implication(board, toPosition) must_== boardAfterMove
                          implication(boardWithWhitePieceAtTarget, toPosition) must throwAn[IllegalMoveException]
                          implication(boardWithBlackPieceAtTarget, toPosition) must throwAn[IllegalMoveException]
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
                          implication(board, toPosition) must throwAn[IllegalMoveException]
                          implication(boardWithWhitePieceAtTarget, toPosition) must throwAn[IllegalMoveException]
                          implication(boardWithBlackPieceAtTarget, toPosition) must_== boardAfterTaking
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
                  List(Symbol("d" + (rank-1))), List(Symbol("c" + (rank-1))), List(Symbol("e" + (rank-1))))
      }
    }
  }

  "a white pawn" should {
    "only be able to move 2 steps forward on rank 7" in {
      (for (rank <- 3 to 7) yield Position(rank, 4)).foreach {
        origin =>
                val rank = origin.rank
                whitePawn.movesFrom(origin).keySet must containPositionLists(
                  List(Symbol("d" + (rank+1))), List(Symbol("c" + (rank+1))), List(Symbol("e" + (rank+1))))
      }
    }
  }

  // todo - en passant

}