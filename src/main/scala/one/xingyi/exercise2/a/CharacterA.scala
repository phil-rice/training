package one.xingyi.exercise2.a

import java.util.concurrent.atomic.AtomicInteger

sealed trait LiveStatus
case object Alive extends LiveStatus
case object Dead extends LiveStatus

case class HitPoints(hp: Int) extends AnyVal {
  def lessThanZero: Boolean = hp < 0
  def -(other: HitPoints): HitPoints = HitPoints(hp - other.hp)
}

trait Logger {
  def info(msg: String)
  def error(msg: String, e: Throwable)
}
object PrintlnLogger extends Logger {
  override def info(msg: String): Unit = println(msg)
  override def error(msg: String, e: Throwable): Unit = {e.printStackTrace(); println(msg) }
}

case class Attack(ch: Character, damage: Int)

object Attack {
  def damage(attack: Attack): Character = {
    import attack._
    import ch._
    if (hitPoints.lessThanZero) ch else {
      val newHitPoints = hitPoints - hitPoints
      val result = if (newHitPoints.lessThanZero)
        ch.copy(alive = Dead, hitPoints = HitPoints(0))
      else
        ch.copy(hitPoints = newHitPoints)
      result
    }
  }

}

case class Character(name: String, alive: LiveStatus = Alive, hitPoints: HitPoints = HitPoints(1000), logger: Logger) {
  import logger._
  private val damageCount = new AtomicInteger()

}
object NonFunctional {
  val counter = new AtomicInteger()

  type NonFunctional[From, To] = (From => To) => (From => To)

  def compose[From, To](nonFunctionalRequirements: NonFunctional[From, To]*): NonFunctional[From, To] =
    bizLogic => nonFunctionalRequirements.foldLeft(bizLogic)((acc, v) => v(acc))

  def defaultErrorStrategy[From, To]: (Exception, From) => To = (e, from) => throw e

  def applyNormalNonFunctionals[From, To](fn: From => To, errorStrategy: (Exception, From) => To): NonFunctional[From, To] =
    compose(addLogging, addErrorHandling(errorStrategy), addMatrix(counter))


  def addLogging[From, To](fn: From => To): From => To =
    from => {
      val result = fn(from)
      println(from + " => " + result)
      result
    }

  def addMatrix[From, To](counter: AtomicInteger)(fn: From => To): From => To = {
    from =>
      counter.incrementAndGet()
      fn(from)
  }

  def addErrorHandling[From, To](errorStrategy: (Exception, From) => To)(fn: From => To): From => To =
    from => try {fn(from) } catch {case e: Exception => errorStrategy(e, from)}
  def addLogging2[From,To] = addSideeffect[From,To]((from, to) => println(s"Called $from ==> $to")) _
  def addMetrics[From,To] = addSideeffect[From,To]((from, to) => counter.incrementAndGet()) _

  def addSideeffect[From,To](sideeffect: (From,To) => Unit)(fn: From => To): From => To = {
    from =>
      val result = fn(from)
      sideeffect(from,result)
      result
  }


}


