package au.com.loftinspace.scalachess.game

import org.specs.matcher.Matcher
import org.specs.{Specification, SystemContexts}
import Positioning._

trait GameSpecification extends Specification {
  def containPositions(s: Symbol*) = haveTheSameElementsAs(s.map(position(_)))
  def containPositionLists(s: List[Symbol]*) = haveTheSameElementsAs(s.map(list => list.map(sym => position(sym))))
  def positions(s: Symbol*) = s.map(position(_))
}