package au.com.loftinspace.scalachess.game

import Positioning._

case class Board(pieces: Map[Position, Piece], taken: List[Piece]) {
  def this() = this (Map(), Nil)

  def place(p: Piece) = {
    case class Placement() {
      def at(s: Symbol): Board = at(position(s))
      def at(destination: Position): Board = new Board(pieces(destination) = p, taken)
    }
    new Placement
  }

  def take(p: Position) = new Board((pieces - p), (pieces(p) :: taken))

  def move(orig: Position) = {
    case class Movement() {
      def to(dest: Position) = {
        if (pieces contains dest) throw new IllegalMoveException("Cannot move to occupied " + dest)
        else pieces.get(orig).map(piece => new Board((pieces - orig)(dest) = piece, taken))
                .getOrElse {throw new IllegalMoveException("No piece at " + orig + " to move")}
      }
    }
    new Movement
  }

  def promote(p: Position) = {
    case class Promotion() {
      def to(r: Role): Board = r match {
        case King => throw new IllegalPromotionException("Cannot promote to King")
        case _ => pieces.get(p).map(piece => piece.role match {
          case Pawn => new Board(pieces(p) = Piece(piece.colour, r), taken)
          case _ => throw new IllegalPromotionException("Can only promote pawns")
        }).getOrElse {throw new IllegalPromotionException("No piece at " + p + " to promote")}
      }
    }
    new Promotion
  }

  def reset = {
    val lineUp = Rook :: Knight :: Bishop :: Queen :: King :: Bishop :: Knight :: Rook :: Nil
    val pairs = for (rank <- 1 :: 2 :: 7 :: 8 :: Nil; file <- 1 to 8) yield (position(rank, file),
            rank match {
              case 1 => Piece(White, lineUp(file - 1))
              case 2 => Piece(White, Pawn)
              case 7 => Piece(Black, Pawn)
              case 8 => Piece(Black, lineUp(file - 1))
            })
    new Board(pairs.foldLeft(Map.empty[Position, Piece]) {(acc, next) => acc(next._1) = next._2}, Nil)
  }
}