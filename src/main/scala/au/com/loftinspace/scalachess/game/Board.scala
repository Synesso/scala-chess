package au.com.loftinspace.scalachess.game

import Positioning._

case class Board(pieces: Map[Position, Piece], taken: List[Piece]) { // todo - boardhistory to be added here.
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

  /**
   * Note, will not identify threats to position from en passant. (Not required).
   */
  def threatsTo(colour: Colour) = {
    case class Threat() {
      def at(s: Symbol): List[Position] = at(position(s))
      def at(pos: Position): List[Position] = {

        // check forward diagonals for opposing colour pawn
        // check surrounding 8 squares for opposing colour king
        // check surrounding 8 knight squares for opposing colour knight
        // check file and ranks expansions for opposing rook or queen
        // check diagonal expansions for opposing bishop or queen

        Nil
      }
    }
    new Threat
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

  def rewind(delta: Delta) = {
    val oldPieces = delta.pieces.foldLeft(pieces) {
      (acc, next) =>
              if (next._2._1.isDefined) acc(next._1) = next._2._1.get else acc - next._1
    }
    new Board(oldPieces, delta.taken.map(taken - _).getOrElse(taken))
  }

  def unwind(delta: Delta) = {
    val newPieces = delta.pieces.foldLeft(pieces) {
      (acc, next) =>
              if (next._2._2.isDefined) acc(next._1) = next._2._2.get else acc - next._1
    }
    new Board(newPieces, delta.taken.map(_ :: taken).getOrElse(taken))
  }

}

case class Delta(pieces: Map[Position, (Option[Piece], Option[Piece])], taken: Option[Piece]) {
  def enPassantTo: Option[Position] = {
    if (taken.equals(None) && pieces.size.equals(2)) {
      val first = pieces.keys.next
      first.rank match {
        case 2 if pieces.contains(Position(4, first.file)) &&
                pieces(first).equals(Some(Piece(White, Pawn)), None) &&
                pieces(Position(4, first.file)).equals(None, Some(Piece(White, Pawn))) => Some(Position(4, first.file))
        case 4 if pieces.contains(Position(2, first.file)) &&
                pieces(first).equals(None, Some(Piece(White, Pawn))) &&
                pieces(Position(2, first.file)).equals(Some(Piece(White, Pawn)), None)  => Some(first)
        case 5 if pieces.contains(Position(7, first.file)) &&
                pieces(first).equals(None, Some(Piece(Black, Pawn))) &&
                pieces(Position(7, first.file)).equals(Some(Piece(Black, Pawn)), None) => Some(first)
        case 7 if pieces.contains(Position(5, first.file)) &&
                pieces(first).equals(Some(Piece(Black, Pawn)), None) &&
                pieces(Position(5, first.file)).equals(None, Some(Piece(Black, Pawn))) => Some(Position(5, first.file))

        case _ => None
      }
    } else None
  }
}