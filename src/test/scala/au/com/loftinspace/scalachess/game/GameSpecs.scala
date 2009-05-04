package au.com.loftinspace.scalachess.game

import org.specs._
import Positioning._

object GameSpec extends Specification with SystemContexts {

  "a game" should {
    val game = systemContext{ new Game }

    "allow a piece to be placed anywhere on the board".withA(game) { game =>
      val pawn = Piece(White, Pawn)
      val allCoordinates = for {
        file <- 'a' to 'h'
        row <- 1 to 8
      } yield Symbol(file.toString + row)
      allCoordinates.foreach { coord =>
        game place pawn at coord must beNone
        pawn.position must_== Some(position(coord))
      }
    }

    "report what piece is at any coordinate".withA(game) { game =>
      val pawn = Piece(Black, Pawn)
      game place pawn at 'e6
      game pieceAt 'e6 must beSome[Piece].which(_.equals(pawn))
      game pieceAt 'f6 must beNone
    }

    "have no more than one reference to the same piece".withA(game) { game =>
      val pawn = Piece(Black, Pawn)
      game place pawn at 'd7
      game place pawn at 'd6
      game pieceAt 'd7 must beNone
      game pieceAt 'd6 must beSome[Piece].which(_.equals(pawn))
    }

    "provide the taken piece, if any, when a move is made".withA(game) { game =>
      val queen = Piece(Black, Queen)
      val rook = Piece(White, Rook)
      game place queen at 'e8 must beNone
      game place rook at 'e8 must beSome[Piece].which(_.equals(queen))
    }

    "reject any attempt to check a piece at a non-coordinate".withA(game) { game =>
      game pieceAt 'a0 must throwAn[IllegalArgumentException]
      game pieceAt 'harvey must throwAn[IllegalArgumentException]
      game pieceAt null must throwAn[IllegalArgumentException]
    }

    "reject any attempt to place a piece at a non-coordinate".withA(game) { game =>
      val king = Piece(Black, King)
      game place king at 'e0 must throwAn[IllegalArgumentException]
      game place king at 'fruit must throwAn[IllegalArgumentException]
      game place king at null must throwAn[IllegalArgumentException]
    }

    "reset and replace the pieces on the board to the starting positions".withA(game) { game =>
      val pieces = Rook :: Bishop :: Knight :: Queen :: King :: Knight :: Bishop :: Rook :: Nil
      game.reset
      for (i <- 1 to 8) {
        game pieceAt Symbol("a"+i) must beSome[Piece].which(_ == Piece(White, pieces(i-1)))
        game pieceAt Symbol("b"+i) must beSome[Piece].which(_ == Piece(White, Pawn))
        game pieceAt Symbol("c"+i) must beNone
        game pieceAt Symbol("d"+i) must beNone
        game pieceAt Symbol("e"+i) must beNone
        game pieceAt Symbol("f"+i) must beNone
        game pieceAt Symbol("g"+i) must beSome[Piece].which(_ == Piece(Black, Pawn))
        game pieceAt Symbol("h"+i) must beSome[Piece].which(_ == Piece(Black, pieces(i-1)))
      }
    }

    "keep a record of the taken pieces".withA(game) { game =>
      val rook = Piece(Black, King)
      val bishop = Piece(White, Bishop)
      game place rook at 'f8
      game place bishop at 'f8
      game.capturedPieces must contain(rook)
      game.capturedPieces must notContain(bishop)
      game.capturedPieces must haveSize(1)
    }
  }
}
