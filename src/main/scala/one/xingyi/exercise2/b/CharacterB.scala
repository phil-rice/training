package one.xingyi.exercise2.b

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

case class Attack(ch: Character, hitPoints: HitPoints)
object Attack {
  def resolve(attack: Attack): Character = {
    import attack._
    if (hitPoints.lessThanZero) ch else {
      val newHitPoints = attack.hitPoints - hitPoints
      if (newHitPoints.lessThanZero)
        ch.copy(alive = Dead, hitPoints = HitPoints(0))
      else
        ch.copy(hitPoints = newHitPoints)
    }
  }
}

object Nonfunctionals {
  type Bizlogic = Attack => Character
  type BizlogicTx = Bizlogic => Bizlogic

  def errorHandler: BizlogicTx = {
    fn =>
      attack =>
        try {fn(attack) } catch {
          case e: Exception => attack.ch.logger.error(s"Unexpected error damaging $this for ${attack.hitPoints}", e); throw e
        }
  }
  def sideeffect(block: (Attack, Character) => Unit): BizlogicTx = { fn => attack => val result = fn(attack); block(attack, result); result }
  def metrics: BizlogicTx = sideeffect((attack, _) => attack.ch.damageCount.incrementAndGet())
  def log: BizlogicTx = sideeffect((attack, result) => attack.ch.logger.info(s"Damaged $this for ${attack.hitPoints} producing $result"))
}

case class Character(name: String, alive: LiveStatus = Alive, hitPoints: HitPoints = HitPoints(1000), logger: Logger) {
  val damageCount = new AtomicInteger()

}
