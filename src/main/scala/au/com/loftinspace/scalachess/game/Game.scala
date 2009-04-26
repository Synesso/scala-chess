package au.com.loftinspace.scalachess.game

class Game {
    var pieces = Set[Piece]()

    def includeIn_:[P <: Piece](piece: P): Unit = {
        piece.game = this
        pieces += piece
    }

    class Placement {
//        def at(s: Symbol): Unit = { // to_be: Option[Piece] = {
//            val p = s.asPosition()
//            println("Placing unknown piece at " + p.column + ", " + p.row)
//        }
    }
    def place(c: Colour, r: Role) = new Placement
}
