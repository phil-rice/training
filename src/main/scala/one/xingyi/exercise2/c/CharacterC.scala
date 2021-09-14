package one.xingyi.exercise2.c
import java.util.concurrent.atomic.AtomicInteger

case class Character(hitpoint: Int, alive: Boolean)

trait Logger {
  def info(msg: String)
  def error(msg: String, e: Exception)
}
object PrintlnLogger extends Logger {
  override def info(msg: String) = println(msg)
  override def error(msg: String, e: Exception) = println(msg)
}

//SOLID Principles
// S - Single Responsibility
// Dependency Injection

//  Score 3/10 ... win is it actually works....

object Character {
  val dead = Character(0, false)
  val count = new AtomicInteger()
  val logger = PrintlnLogger

  //decorator pattern : Gang of 4  book
  def addErrorHandling[A, B](bizlogic: (A, B) => A, errorMsgFn: (A, B) => String): (A, B) => A = {
    (a, b) =>
      try {
        bizlogic(a, b)
      } catch {
        case e: Exception =>
          logger.error(errorMsgFn(a, b), e)
          throw e
      }
  }

  def addMetrics[A, B](bizlogic: (A, B) => A, count: AtomicInteger): (A, B) => A = {
    (a, b) =>
      count.incrementAndGet()
      bizlogic(a, b)
  }

  def addLogging[A, B](bizlogic: (A, B) => A, logMsg: (A, B, A) => String, logger: Logger): (A, B) => A = {
    (a, b) =>
      val result = bizlogic(a, b)
      logger.info(logMsg(a, b, result))
      result
  }

  def heal(c: Character, healAmount: Int): Character = ???

  //at least 5 responsibilitites
  def attack(ch: Character, damage: Int): Character =
    if (damage < 0) ch else { // validation
      val newHitpoints = ch.hitpoint - damage //bizlogic about the effects of damage...finally some biz logic...
      if (newHitpoints < 0) dead else //bizlogic about checking for death
        ch.copy(hitpoint = newHitpoints)
    }
  //composition of all the bits

}
