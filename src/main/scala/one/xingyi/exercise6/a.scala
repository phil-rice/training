package one.xingyi.exercise6

import NonFunctional._
import one.xingyi.exercise6.ErrorStrategy.logAndRollBack

import java.util.concurrent.atomic.AtomicInteger
case class Character(health: Int = 100, isAlive: Boolean = true)
object Character {
  def checkHealth(health: Int): Boolean = if (health >= 0) true else false

}
case class Attack(character: Character, damage: Int)
case class Heal(character: Character, damage: Int)
case class PickUpItem(character: Character, itemToPickUp: String)

trait DefaultNonFunctionals[From, To] {
  def apply(fn: From => To): (From => To)
}
object DefaultNonFunctionals {
  implicit def nonFunctional[From, To](implicit errorStrategy: ErrorStrategy[From, To], metricCounter: MetricCounter[From]): DefaultNonFunctionals[From, To] = new DefaultNonFunctionals[From, To] {
    override def apply(fn: From => To): From => To = compose[From, To](
      addLogging,
      addErrorHandling,
      addMetrics
    )(fn)
  }
}

object Attack {
  val attackCounter = new AtomicInteger(0)
  implicit val errorStrategyForAttack: ErrorStrategy[Attack, Character] = logAndRollBack((a: Attack) => a.character) _
  implicit val metricCounterForAttack = new MetricCounter[Attack] {
    override def apply(): Unit = attackCounter.incrementAndGet()
  }

  def doDamageWithNonFunctionals(implicit nonFunctionals: DefaultNonFunctionals[Attack, Character]): Attack => Character = nonFunctionals(doDamage)

  def doDamage(attack: Attack): Character = {
    import attack._
    val updatedHealth = character.health - damage
    new Character(updatedHealth, Character.checkHealth(updatedHealth))
  }


}

trait NonFunctional[From, To] extends ((From => To) => (From => To)) {
  def name: String
}
case class ComposeNonFunctionals[From, To](txs: Iterable[NonFunctional[From, To]]) extends NonFunctional[From, To] {
  override def apply(bizlogic: From => To): From => To = txs.foldLeft(bizlogic)((acc, v) => v(acc))
    override def name: String = s"compose(${txs.map(_.name).mkString(",")}"
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


  def addLogging1[From, To]: NonFunctional[From, To] = new NonFunctional[From, To] {
    override def name: String = "logging"
    override def apply(fn: From => To): From => To =
      from => {
        val result = fn(from)
        println(from + " => " + result)
        result
      }
  }

  def addName[From, To](theName: String)(fn: ((From => To) => (From => To))): NonFunctional[From, To] = new NonFunctional[From, To] {
    override def name: String = theName
    override def apply(raw: From => To): From => To = fn(raw)
  }

  def applyNormalNonFunctional[From, To](fn: From => To)(implicit counter: MetricCounter[From], errorStrategy: ErrorStrategy[From, To]) =
    compose[From, To](
      addLogging,
      addErrorHandling,
      addMetrics
    )


  type Decorator[From, To] = (From => To) => (From => To)

  def compose[From, To](decorators: NonFunctional[From, To]*) = new ComposeNonFunctionals(decorators)

  //currying to change everything to one input one output
  //functional composition


  def addMetrics[From, To](implicit counter: MetricCounter[From]): NonFunctional[From, To] =
    addName("metrics") { fn =>
      from =>
        counter()
        fn(from)
    }

  def addLogging[From, To]: NonFunctional[From, To] = addName("logging") { fn =>
    from =>
      val result = fn(from)
      println(from + " => " + result)
      result
  }


  def addErrorHandling[From, To](implicit errorStrategy: ErrorStrategy[From, To]) = addName[From, To]("error") {
    fn =>
      from =>
        try fn(from)
        catch {
          case e: Exception => errorStrategy(e, from)
        }
  }
}