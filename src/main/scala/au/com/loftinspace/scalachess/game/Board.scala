package au.com.loftinspace.scalachess.game

import Positioning._

case class Board(pieces: Map[Position, Piece], taken: List[Piece]) {
  def this() = this (Map(), Nil)

  /**
   * Place a piece on the board (at a position)
   * @return a new board
   */
  def place(p: Piece) = {
    case class Placement() {
      def at(s: Symbol): Board = at(position(s))

      def at(destination: Position): Board = new Board(pieces(destination) = p, taken)
    }
    new Placement
  }

  /**
   * Take (capture) the piece at the given position
   * @return a new board
   */
  def take(p: Position) = new Board((pieces - p), (pieces(p) :: taken))

  /**
   * Move the piece at the given position (to a new position)
   * @return a new board
   */
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

  /**
   * Promote the piece at the given position to a new role
   * @return a new board
   */
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
   * Finds all positions which contain a threat to the given colour (at a given position).
   * Note, will not identify threats to position from en passant (as this behaviour is not required).
   * @return the positions of threat
   */
  def threatsTo(colour: Colour) = {
    case class Threat() {
      def at(s: Symbol): List[Position] = at(position(s))

      def at(pos: Position): List[Position] = {
        def expand(direction: Seq[Option[Position] => Option[Position]]): Option[Position] = {
          def next(from: Option[Position]): Option[Position] = {
            val nextPose = direction.foldLeft(from) {(acc, next) => next(acc)}
            if (nextPose.isEmpty) return None
            if (pieces.contains(nextPose.get)) Some(nextPose.get) else next(nextPose)
          }
          next(Some(pos))
        }

        def opposing(r: Role) = {
          case class OpposingRoleCheck() {
            def at(p: Position) = pieces.get(p).map(_.equals(Piece(opposite of colour, r))).getOrElse(false)
          }
          new OpposingRoleCheck
        }

        val forward = if (White.equals(colour)) ^ _ else v _
        val pawns: List[Position] = Set(forward(pos < 1), forward(pos > 1)).filter(_.isDefined).foldLeft(Nil: List[Position]) {
          (acc, next) =>
                  if (opposing(Pawn).at(next.get)) next.get :: acc else acc
        }
        val rankFileVectors: List[Position] = (expand(Seq(< _)) :: expand(Seq(^ _)) :: expand(Seq(> _)) :: expand(Seq(v _)) :: Nil)
                .filter(_.isDefined).foldLeft(Nil: List[Position]) {
          (acc, next) =>
                  if (opposing(Rook).at(next.get) || opposing(Queen).at(next.get)) next.get :: acc else acc
        }
        val diagonalVectors: List[Position] = (expand(Seq(< _, ^ _)) :: expand(Seq(> _, ^ _)) :: expand(Seq(> _, v _)) :: expand(Seq(< _, v _)) :: Nil)
                .filter(_.isDefined).foldLeft(Nil: List[Position]) {
          (acc, next) =>
                  if (opposing(Bishop).at(next.get) || opposing(Queen).at(next.get)) next.get :: acc else acc
        }
        val knights: List[Position] = {
          val options = radialBasedPositions(pos, List(-2, -1, 1, 2), (rank, file) => Math.abs(rank) != Math.abs(file))
          options.toList.filter(opposing(Knight).at(_))
        }
        val kings: List[Position] = {
          val options = radialBasedPositions(pos, -1 to 1, (rank, file) => (rank != 0 || file != 0))
          options.toList.filter(opposing(King).at(_))
        }

        pawns ::: rankFileVectors ::: diagonalVectors ::: knights ::: kings
      }
    }
    new Threat
  }

  /**
   * Layout the board for a new game.
   * @return a new board
   */
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

  /**
   * Alter the board to the previous state with the given history.
   * @return a new board
   */
  def rewind(history: History) = {
    history.implication match {
      case Some(Take(p)) => new Board(
        pieces - history.move.to + {history.move.from -> pieces(history.move.to)} + {p -> taken.head}, taken.tail)
      case _ => new Board(pieces - history.move.to + {history.move.from -> pieces(history.move.to)}, taken)
    }
  }

  /**
   * Alter the board to a future state with the given history.
   * @return a new board
   */
  def unwind(history: History) = {
    history.implication match {
      case Some(Take(p)) => new Board(
        pieces - p - history.move.from + {history.move.to -> pieces(history.move.from)}, pieces(p) :: taken)
      case _ => new Board(pieces - history.move.from + {history.move.to -> pieces(history.move.from)}, taken)
    }
  }

}



case class Delta(pieces: Map[Position, (Option[Piece], Option[Piece])], taken: Option[Piece]) {
  /**
   * The destination position of an en passant move, if that is what this delta represents.
   */
  def enPassantTo: Option[Position] = {
    if (taken.equals(None) && pieces.size.equals(2)) {
      val first = pieces.keys.next
      first.rank match {
        case 2 if pieces.contains(Position(4, first.file)) &&
                pieces(first).equals(Some(Piece(White, Pawn)), None) &&
                pieces(Position(4, first.file)).equals(None, Some(Piece(White, Pawn))) => Some(Position(4, first.file))
        case 4 if pieces.contains(Position(2, first.file)) &&
                pieces(first).equals(None, Some(Piece(White, Pawn))) &&
                pieces(Position(2, first.file)).equals(Some(Piece(White, Pawn)), None) => Some(first)
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