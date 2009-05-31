package au.com.loftinspace.scalachess.game

import org.specs._
import matcher.Matcher
import Positioning._

object GameSpec extends Specification with SystemContexts {
  import GameContexts._

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
      val pieces = Rook :: Knight :: Bishop :: Queen :: King :: Bishop :: Knight :: Rook :: Nil
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

    "provide a list of piece options given a list of positions".withA(game) { game =>
      val positions = position('a8) :: position('d7) :: position('g4) :: position('d7) :: Nil
      val pieces = Some(Piece(Black, Rook)) :: Some(Piece(Black, Pawn)) :: None :: Some(Piece(Black, Pawn)) :: Nil
      game piecesAt positions must containInOrder(pieces)
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
/*

    "move the king's rook when the king is castling to king's side".withA(progressedGame) { game =>
      val whiteKing = (game pieceAt 'e1).get
      val whiteKingsRook = (game pieceAt 'h1).get
      game move 'e1 to 'g1
      game.moves.last must imply(Move(whiteKingsRook, position('h1), position('f1), None))
//      whiteKingsRook.position must beSome[Piece].which(_.equals(position('f1)))
//      game pieceAt 'h1 must beNone
    }
 */

    "report whether the board matches a given subset of positions".withA(progressedGame) { game =>
      val scenario = Map('e1 -> Some(Piece(White, King)), 'f1 -> None, 'g1 -> None, 'h1 -> Some(Piece(White, Rook)))
      game presents scenario must beTrue
      val unmatchedScenario = Map('d1 -> Some(Piece(White, King)), 'f1 -> None, 'g1 -> None, 'h1 -> Some(Piece(White, Rook)))
      game presents unmatchedScenario must beFalse
    }
  }
  case class imply(move: Move) extends Matcher[Move] {
    def apply(m: => Move) = (move equals m, "expected " + move + " and got it", "expected " + move + ", but got " + m)
  }
}

object GameContexts extends SystemContexts {
  val progressedGame = systemContext {
    val game = new Game
    game move 'e2 to 'e4
    game move 'e7 to 'e5
    game move 'd2 to 'd4
    game move 'd7 to 'd5
    game move 'g1 to 'f3
    game move 'g8 to 'f6
    game move 'b1 to 'c3
    game move 'b8 to 'c6
    game move 'f1 to 'c4
    game move 'f8 to 'c5
    game move 'c1 to 'f4
    game move 'c8 to 'f5
    game
  }
}