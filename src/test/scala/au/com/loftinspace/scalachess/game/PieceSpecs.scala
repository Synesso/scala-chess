package au.com.loftinspace.scalachess.game

import org.specs._
import org.scalacheck.Prop._

object PieceSpec extends Specification with ScalaCheck {
//  implicit def enableInteger(i: Int) = new IntegerEnabler(i)
//
//  "any piece that is not taken" should {
//    "know its position on the board in chess notation" in {
//      for (i <- 1 to 8; j <- 1 to 8) {
//        val piece = new Piece(White, King, i, j)
//        val expectedLocationString = ("abcdefgh".charAt(i-1)).toString + j
//        piece.location must beSome[String].which(_.equals(expectedLocationString))
//      }
//    }
//  }
//
//  "any piece that has been captured" should {
//    "report that is has no location" in {
//      val piece = new Piece(Black, Pawn, Captured)
//      piece.location must beNone
//    }
//  }
//
//  "a piece" should {
//    "know which game it belongs to" in {
//      val game = new Game
//      val piece = new Piece(Black, Rook, Captured)
//      piece includeIn_: game
//      piece.game must_== game
//    }
//  }
//
//  "a pawn that has already been moved and is not blocked" should {
//    val pawn = new Piece(White, Pawn, 4, 4)
//    pawn includeIn_: new Game
//
//    "be able to move one square towards opposite colour" in {
//      pawn canMove Straight towards Black must beTrue
//    }
//    "not be able to move more than one square towards opposite colour" in {
//      forAll { n: Int => n >= 2 ==>
//              (pawn canMove Straight by n towards Black equals false)
//      } must pass
//    }
//    "not be able to move sideways towards king" in {
//      pawn canMove Straight towards King must beFalse
//    }
//    "not be able to move sideways towards queen" in {
//      pawn canMove Straight towards Queen must beFalse
//    }
//    "not be able to move towards its own colour" in {
//      pawn canMove Straight towards White must beFalse
//    }
//    "not be able to move diagonally towards black king" in {
//      pawn canMove Diagonally towards (Black, King) must beFalse
//    }
//    "not be able to move diagonally towards black queen" in {
//      pawn canMove Diagonally towards (Black, Queen) must beFalse
//    }
//    "not be able to move diagonally towards white king" in {
//      pawn canMove Diagonally towards (White, King) must beFalse
//    }
//    "not be able to move diagonally towards white queen" in {
//      pawn canMove Diagonally towards (White, Queen) must beFalse
//    }
//  }
//
//  "a pawn that has already moved and is blocked" should {
////    val pawn = new Piece(White, Pawn, 4, 4)
////    pawn includeIn_: new Game
////    new Piece(White, Pawn, 4, 5) includeIn_: pawn.game
//    "not be able to move forward at all" in {
////      forAll { n: Int => n >= 1 ==>
////              (pawn canMove Straight by n towards Black equals false)
////      } must pass
//    }
//  }

  //    "be able to take diagonally towards opposite colour" in {}

  //    "be able to move two squares towards opposite colour ('launch') on its first move" in {
  //
  //    }

  //    "not be able to move one square towards opposite colour when blocked" in {
  //      //            new Piece(Black, Pawn, 4, 5) includeIn_:
  //      //            pawn canMove Straight towards Black must beFalse
  //    }
}