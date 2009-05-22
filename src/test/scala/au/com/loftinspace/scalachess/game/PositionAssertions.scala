package au.com.loftinspace.scalachess.game

import org.specs.matcher._

object PositionAssertions extends IterableMatchers {
  def containPositions(s: Symbol*) = containAll(s.map(position(_)))
}
