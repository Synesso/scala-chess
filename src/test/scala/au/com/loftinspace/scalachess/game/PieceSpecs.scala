package au.com.loftinspace.scalachess.game

import org.specs._
import Positioning._

object PieceSpec extends PieceSpecification {
  "a piece that is taken" should {
    val takenPiece = systemContext {
      val game = new Game
      val blackQueen = Piece(Black, Queen)
      val whiteQueen = Piece(White, Queen)
      game place whiteQueen at 'e5
      game place blackQueen at 'e5
      Tuple3(game, blackQueen, whiteQueen)
    }

    "be marked as captured".withA(takenPiece) {
      scenario =>
              scenario._3.captured must beTrue
    }
  }
}

object PawnSpec extends PieceSpecification {
  "a pawn that has not yet moved" should {
    val newGame = systemContext {new Game}
    "be able to move either one or two spaces towards opposite colour".withA(newGame) {
      game =>
              val pawnsLocs = for (file <- 'a' to 'h'; rank <- List('2', '7')) yield position(Symbol(file.toString + rank))
              pawnsLocs.foreach {
                loc =>
                        val pawn = (game pieceAt loc).get
                        val direction = if (loc.rank.equals(2)) 1 else -1
                        val expected = Set(loc ^ direction, loc ^ direction * 2).map(_.get)
                        game findMovesFor pawn must containAll(expected)
              }
    }
  }

  "a pawn that has not yet moved and is blocked" should {
    val blockedPawnGame = systemContext {
      val game = new Game
      game place (game pieceAt 'e2 get) at 'e6
      game
    }

    "not be able to move anywhere".withA(blockedPawnGame) {
      game =>
              val pawn = (game pieceAt 'e7).get
              game findMovesFor pawn must beEmpty
    }
  }

  "a pawn that has not yet moved and is blocked from moving 2 squares" should {
    val blockedPawnGame = systemContext {
      val game = new Game
      game place (game pieceAt 'e2 get) at 'e5
      game
    }

    "be able to move one square only".withA(blockedPawnGame) {
      game =>
              val pawn = (game pieceAt 'e7).get
              game findMovesFor pawn must containPositions('e6)
    }
  }

  "a pawn that has moved and is not blocked" should {
    val blockedPawnGame = systemContext {
      val game = new Game
      game move 'e2 to 'e4
      game
    }

    "be able to move 1 square forward".withA(blockedPawnGame) {
      game =>
              val pawn = (game pieceAt 'e4).get
              game findMovesFor pawn must containPositions('e5)
    }
  }

  "a pawn that has opposing pieces on the forward diagonals" should {
    val pawnCanTakeGame = systemContext {
      val game = new Game
      game move 'e2 to 'e4
      game place (Piece(Black, Rook)) at 'd5
      game place (Piece(Black, Pawn)) at 'f5
      game
    }

    "be able to move 1 square forward or take to either diagonal".withA(pawnCanTakeGame) {
      game =>
              val pawn = (game pieceAt 'e4).get
              game findMovesFor pawn must containPositions('e5, 'd5, 'f5)
    }
  }

  "a pawn at the edge of the board that has an opposing piece on a forward diagonal" should {
    val pawnCanTakeGame = systemContext {
      val game = new Game
      game move 'a2 to 'a4
      game place (Piece(Black, Rook)) at 'b5
      game
    }

    "be able to move 1 square forward or take the diagonal".withA(pawnCanTakeGame) {
      game =>
              val pawn = (game pieceAt 'a4).get
              game findMovesFor pawn must containPositions('a5, 'b5)
    }
  }

  "a pawn that is blocked from going forward but has an opposing piece on the diagonal" should {
    val blockedPawnCanTakeGame = systemContext {
      val game = new Game
      game move 'f7 to 'f5
      game place (Piece(Black, Pawn)) at 'f4
      game place (Piece(White, Queen)) at 'e4
      game
    }

    "be able to take on the diagonal only".withA(blockedPawnCanTakeGame) {
      game =>
              val pawn = (game pieceAt 'f5).get
              game findMovesFor pawn must containPositions('e4)
    }
  }

  "a pawn that has same side pieces on the forward diagonals" should {
    val pawnCanTakeGame = systemContext {
      val game = new Game
      game move 'e2 to 'e4
      game place (Piece(White, Rook)) at 'd5
      game place (Piece(White, Pawn)) at 'f5
      game
    }

    "be able to move 1 square forward only".withA(pawnCanTakeGame) {
      game =>
              val pawn = (game pieceAt 'e4).get
              game findMovesFor pawn must containPositions('e5)
    }
  }

  "a pawn that is on the same rank as an opposing pawn that has just moved 2 places" should {
    val pawnEnPassantGame = systemContext {
      val game = new Game
      game move 'e2 to 'e4
      game
    }

    "be able to perform en passant if adjacent".withA(pawnEnPassantGame) {
      game =>
              val pawn = Piece(Black, Pawn)
              game place pawn at 'f4
              game findMovesFor pawn must containPositions('e3, 'f3)
    }

    "not be able to perform en passant if not adjacent".withA(pawnEnPassantGame) {
      game =>
              val pawn = Piece(Black, Pawn)
              game place pawn at 'g4
              game findMovesFor pawn must containPositions('g3)
    }

    "not be able to perform en passant if other moves have been made".withA(pawnEnPassantGame) {
      game =>
              val pawn = Piece(Black, Pawn)
              game move 'a7 to 'a6
              game move 'a2 to 'a3
              game place pawn at 'f4
              game findMovesFor pawn must containPositions('f3)
    }
  }
}

object RookSpec extends PieceSpecification {
  "a rook" should {
    "not be able to move from starting position" in {
      val game = new Game
      val rook = (game pieceAt 'a1 get)
      game findMovesFor rook must beEmpty
    }

    "be able to move along rank and file until blocked" in {
      val game = new Game
      val rook = Piece(Black, Rook)
      game place rook at 'f3
      game findMovesFor rook must containPositions('f2, 'f4, 'f5, 'f6, 'a3, 'b3, 'c3, 'd3, 'e3, 'g3, 'h3)
    }
  }
}

object BishopSpec extends PieceSpecification {
  "a bishop" should {
    "not be able to move from starting position" in {
      val game = new Game
      val bishop = (game pieceAt 'c1 get)
      game findMovesFor bishop must beEmpty
    }

    "be able to move along diagonals until blocked" in {
      val game = new Game
      val bishop = Piece(White, Bishop)
      game place bishop at 'f5
      game findMovesFor bishop must containPositions('e4, 'd3, 'e6, 'd7, 'g4, 'h3, 'g6, 'h7)
    }
  }
}

object KnightSpec extends PieceSpecification {
  "a knight" should {
    "be able to move to vacant positions from starting position" in {
      val game = new Game
      val knight = (game pieceAt 'b1 get)
      game findMovesFor knight must containPositions('a3, 'c3)
    }

    "be able to move to vacant positions from anywhere" in {
      val game = new Game
      val knight = (game pieceAt 'b8 get)
      game place knight at 'f4
      game findMovesFor knight must containPositions('e2, 'g2, 'd3, 'h3, 'e6, 'g6, 'd5, 'h5)
    }
  }
}

object QueenSpec extends PieceSpecification {
  "a queen" should {
    "not be able to move from starting position" in {
      val game = new Game
      val queen = (game pieceAt 'd1 get)
      game findMovesFor queen must beEmpty
    }

    "be able to move along ranks, files and diagonals until blocked" in {
      val game = new Game
      val queen = Piece(Black, Queen)
      game place queen at 'f5
      game findMovesFor queen must containPositions('f2, 'f3, 'f4, 'f6, 'a5, 'b5, 'c5, 'd5, 'e5, 'g5, 'h5,
        'g6, 'e4, 'd3, 'c2, 'h3, 'g4, 'e6)
    }
  }
}

object KingSpec extends PieceSpecification {
  "a king" should {
    "not be able to move from starting position" in {
      val game = new Game
      val king = (game pieceAt 'e1 get)
      game findMovesFor king must beEmpty
    }
    "be able to move one space in any direction when none place it in check" in {
      val game = new Game
      val king = Piece(White, King)
      game place king at 'd4
      game findMovesFor king must containPositions('c3, 'c4, 'c5, 'd3, 'd5, 'e3, 'e4, 'e5)
    }
    "be able to castle with the king's rook" in {
      // todo - quite a pickle!
    }
  }
}

