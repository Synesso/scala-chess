package au.com.loftinspace.scalachess.game

import org.specs.{Specification, SystemContexts}
import Positioning._

trait PieceSpecification extends Specification with SystemContexts {
  def containPositions(s: Symbol*) = haveTheSameElementsAs(s.map(position(_)))
}