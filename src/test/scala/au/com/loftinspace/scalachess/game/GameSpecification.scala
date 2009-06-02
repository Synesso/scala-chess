package au.com.loftinspace.scalachess.game

import org.specs.{Specification, SystemContexts}
import Positioning._

trait GameSpecification extends Specification with GameContexts {
  def containPositions(s: Symbol*) = haveTheSameElementsAs(s.map(position(_)))
  def positions(s: Symbol*) = s.map(position(_))
}