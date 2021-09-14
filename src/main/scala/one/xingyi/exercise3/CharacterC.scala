package one.xingyi.exercise3

import one.xingyi.exercise3.Character.{applyDamage, killIfHpLessThanZero}

import java.util.concurrent.atomic.AtomicInteger

sealed trait LiveStatus
case object Alive extends LiveStatus
case object Dead extends LiveStatus

case class HitPoints(hp: Int) extends AnyVal {
  def lessThanZero: Boolean = hp < 0
  def -(other: HitPoints): HitPoints = HitPoints(hp - other.hp)
}

case class Attack(ch: Character, hitPoints: HitPoints)

object Attack {
  val damageCount = new AtomicInteger()
  implicit def errorMessageForAttack: ErrorMessage[Attack] = (attack, e) => s"Unexpected error damaging $this for ${attack.hitPoints}"
  implicit def metricsForAttack: Metrics[Attack] = attack => damageCount.incrementAndGet()
  implicit def logMessage: LogMessage[Attack, Character] = (attack, result) => s"Damaged $this for ${attack.hitPoints} producing $result"
  implicit def validateAttack: Validate[Attack] = Validate.failIf(_.hitPoints.lessThanZero, "validate.attack.negativehp")

  def resolve: Attack => Character = Validate.applyAndDefaultIfFail[Attack, Character](
    ifSucceed = attack => killIfHpLessThanZero(applyDamage(attack.ch, attack.hitPoints)),
    ifFailure = (attack, errors) => attack.ch)

  def resolveOrErrors: Attack => Either[List[String], Character] =
    Validate.applyAndMap[Attack, Character](attack => killIfHpLessThanZero(applyDamage(attack.ch, attack.hitPoints)))
}


object Character {
  def applyDamage(c: Character, hp: HitPoints) = c.copy(hitPoints = c.hitPoints - hp)
  def killIfHpLessThanZero(c: Character): Character = if (c.hitPoints.lessThanZero) c.copy(hitPoints = HitPoints(0), alive = Dead) else c

}
case class Character(name: String, alive: LiveStatus = Alive, hitPoints: HitPoints = HitPoints(1000))