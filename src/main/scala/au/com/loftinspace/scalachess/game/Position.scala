package au.com.loftinspace.scalachess.game

abstract class Position
case class InPosition(column: Int, row: Int) extends Position
case object Captured extends Position

class PositionAwareSymbol(s: Symbol) {
    def asPosition = new InPosition(1, 1)
}

object SymbolEnabler {
    implicit def makeSymbolPositionAware(symbol: Symbol) = new PositionAwareSymbol(symbol)
}
