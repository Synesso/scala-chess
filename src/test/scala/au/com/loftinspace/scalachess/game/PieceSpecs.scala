package au.com.loftinspace.scalachess.game

import org.specs._
import org.scalacheck.Prop._
import Positioning._

object PieceSpec extends Specification with SystemContexts {

  "a piece that is taken" should {
    val takenPiece = systemContext {
      val game = new Game
      val blackQueen = Piece(Black, Queen)
      val whiteQueen = Piece(White, Queen)
      game place whiteQueen at 'e5
      game place blackQueen at 'e5
      Tuple3(game, blackQueen, whiteQueen)
    }

    "be marked as captured".withA(takenPiece) { scenario =>
      scenario._3.captured must beTrue
    }
  }

  "a pawn at the start of a game" should {
    val newGame = systemContext {new Game}

    "be able to move either one or two spaces towards opposite colour".withA(newGame) { game =>
      val pawnsLocs = for (file <- 'a' to 'h'; rank <- List('2', '7')) yield position(Symbol(file.toString + rank))
      pawnsLocs.foreach { loc =>
        val pawn = (game pieceAt loc).get
        val direction = if (loc.rank.equals(2)) 1 else -1
        val expected = Set(loc ^ direction, loc ^ direction*2).map(_.get)
        game findMovesFor pawn must containAll(expected)
      }
    }
  }

  "a pawn that is blocked" should {
    val blockedPawnGame = systemContext {
      val game = new Game
//      game move 'e2 to 'e4
//      game move 'e7 to 'e5
      game
    }
    "no-op" in {}
  }
}
