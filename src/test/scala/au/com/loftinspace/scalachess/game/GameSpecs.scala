package au.com.loftinspace.scalachess.game

import org.specs._
import Positioning._

object GameSpec extends Specification with SystemContexts {
  "a game" should {
    val game = systemContext{ new Game }

    "report what piece is at any coordinate".withA(game) { game =>
      val pawn = Piece(Black, Pawn)
      game place pawn at 'e6
      game pieceAt 'e6 must beSome[Piece].which(_.equals(pawn))
      game pieceAt 'f6 must beNone
    }

    "provide the taken piece, if any, when a move is made".withA(game) { game =>
      val queen = (game pieceAt 'd1).get
      val rook = (game pieceAt 'a8).get
      game place queen at 'f4 must beNone
      game place rook at 'f4 must beSome[Piece].which(_.equals(queen))
    }

    "provide the taken piece when en passant is made".withA(game) { game =>
      val takenPawn = (game pieceAt 'f7).get
      game place Piece(White, Pawn) at 'g4
      game move 'g4 to 'g5
      game move 'f7 to 'f5
      game move 'g5 to 'f6 must beSome[Piece].which(_ == takenPawn)
      game.capturedPieces must containAll(Set(takenPawn))
      takenPawn.captured must beTrue
    }

    "reject any attempt to check a piece at a non-coordinate".withA(game) { game =>
      game pieceAt 'a0 must throwAn[IllegalArgumentException]
      game pieceAt 'z4 must throwAn[IllegalArgumentException]
      game pieceAt 'harvey must throwAn[IllegalArgumentException]
    }

    "reject any attempt to place a piece at a non-coordinate".withA(game) { game =>
      val king = Piece(Black, King)
      game place king at 'e0 must throwAn[IllegalArgumentException]
      game place king at 'fruit must throwAn[IllegalArgumentException]
    }

    "reset and replace the pieces on the board to the starting positions".withA(game) { game =>
      val pieces = Rook :: Bishop :: Knight :: Queen :: King :: Knight :: Bishop :: Rook :: Nil
      game.reset
      for (file <- 'a' to 'h') {
        game pieceAt Symbol(file.toString + 1) must beSome[Piece].which(_ == Piece(White, pieces(file-'a')))
        game pieceAt Symbol(file.toString + 2) must beSome[Piece].which(_ == Piece(White, Pawn))
        game pieceAt Symbol(file.toString + 3) must beNone
        game pieceAt Symbol(file.toString + 4) must beNone
        game pieceAt Symbol(file.toString + 5) must beNone
        game pieceAt Symbol(file.toString + 6) must beNone
        game pieceAt Symbol(file.toString + 7) must beSome[Piece].which(_ == Piece(Black, Pawn))
        game pieceAt Symbol(file.toString + 8) must beSome[Piece].which(_ == Piece(Black, pieces(file-'a')))
      }
    }

    "keep a record of the taken pieces".withA(game) { game =>
      val rook = (game pieceAt 'a1).get
      val bishop = (game pieceAt 'c8).get
      game place rook at 'e4
      game place bishop at 'e4
      game.capturedPieces must contain(rook)
      game.capturedPieces must notContain(bishop)
      game.capturedPieces must haveSize(1)
    }

    "permit a valid move".withA(game) { game =>
      val captured = game move 'e2 to 'e4
      captured must beNone
    }

    "disallow any move from a non-location".withA(game) { game =>
      game move 'z2 to 'e4 must throwAn[IllegalArgumentException]
    }

    "disallow any move to a non-location".withA(game) { game =>
      game move 'e2 to 'z4 must throwAn[IllegalArgumentException]
    }

    "disallow any move from an unoccupied position".withA(game) { game =>
      game move 'e3 to 'e4 must throwAn[IllegalMoveException]
    }

    "disallow moves not permitted by the piece being moved".withA(game) { game =>
      game move 'e2 to 'f3 must throwAn[IllegalMoveException]
    }

    "report zero moves made when new".withA(game) { game =>
      game.moves must beEmpty
    }

    "report the moves made when in progress".withA(game) { game =>
      val whitePawn = (game pieceAt 'e2).get
      val blackPawn = (game pieceAt 'd7).get
      game move 'e2 to 'e4
      game move 'd7 to 'd5
      game move 'e4 to 'd5
      game.moves must containInOrder(List(
        Move(whitePawn, position('e2), position('e4), None),
        Move(blackPawn, position('d7), position('d5), None),
        Move(whitePawn, position('e4), position('d5), Some(blackPawn))))
    }
  }
}
