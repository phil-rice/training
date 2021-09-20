package one.xingyi.exercise2.CharacterD

import one.xingyi.exercise2.CharacterD.ErrorStrategy.logAndRollBack
import one.xingyi.exercise2.CharacterD.NonFunctional._

import java.util.concurrent.atomic.AtomicInteger
case class Character(health: Int = 100, isAlive: Boolean = true)
object Character {
  def checkHealth(health: Int): Boolean = if (health >= 0) true else false

}
case class Attack(character: Character, damage: Int)
case class Heal(character: Character, damage: Int)
case class PickUpItem(character: Character, itemToPickUp: String)

object Attack {
  val attackCounter = new AtomicInteger(0)
  implicit val errorStrategyForAttack: ErrorStrategy[Attack, Character] = logAndRollBack((a: Attack) => a.character) _

  implicit val metricCounterForAttack = new MetricCounter[Attack] {
    override def apply(): Unit = attackCounter.incrementAndGet()
  }

  def doDamageWithNonFunctionals: Attack => Character = applyNormalNonFunctional(doDamage) apply (doDamage)

  def doDamage(attack: Attack): Character = {
    import attack._
    val updatedHealth = character.health - damage
    new Character(updatedHealth, Character.checkHealth(updatedHealth))
  }


}

class ComposeNonFunctionals[From, To](val txs: (From => To) => (From => To)*) extends ((From => To) => (From => To)) {
  override def apply(bizlogic: From => To): From => To = txs.foldLeft(bizlogic)((acc, v) => v(acc))
}

trait MetricCounter[T] {
  def apply()
}

trait ErrorStrategy[From, To] {
  def apply(e: Exception, from: From): To
}

object ErrorStrategy {
  def logAndRollBack[From, To](rollbackFn: From => To)(e: Exception, from: From): To = {
    println(e)
    rollbackFn(from)
  }

}
object NonFunctional {

  def addLogging[From, To](fn: From => To): From => To =
    from => {
      val result = fn(from)
      println(from + " => " + result)
      result
    }

  def applyNormalNonFunctional[From, To](fn: From => To)(implicit counter: MetricCounter[From], errorStrategy: ErrorStrategy[From, To]) =
    compose[From, To](
      addLogging,
      addErrorHandling,
      addMetrics
    )


  type Decorator[From, To] = (From => To) => (From => To)

  def compose[From, To](decorators: Decorator[From, To]*): Decorator[From, To] =
    rawFn => decorators.foldLeft(rawFn)((acc, decorator) => decorator(acc))


  //currying to change everything to one input one output
  //functional composition


  def addMetrics[From, To](fn: From => To)(implicit counter: MetricCounter[From]): From => To =
    from => {
      counter()
      fn(from)
    }


  def addErrorHandling[From, To](fn: From => To)(implicit errorStrategy: ErrorStrategy[From, To]): From => To =
    from => {
      try {
        fn(from)
      } catch {
        case e: Exception => errorStrategy(e, from)
      }
    }
}