package one.xingyi.exercise1
import java.util.concurrent.atomic.AtomicInteger


object Character {
  val counter = new AtomicInteger()
}
object NonFunctionals {
  //simples rules... make it easier
  // Everything should be a function that goes from one thing to one thing


}

case class Attack(ch: Character, hp: Int)

object Attack {

  //S - Single Responsibility
  //D - dependency injection

  def addErrorHandling[From,To](bizlogic: From => To, errorHandler: (Exception, From) => To): From => To = {
    attack =>
      try {
        bizlogic(attack)
      } catch {
        case e: Exception => errorHandler(e, attack)
      }
  }

  def attackErrorHandler(e: Exception, attack: Attack) = {
    println(s"You had an unexpected error $e")
    e.printStackTrace()
    attack.ch
  }


  def addMetrics(bizlog: Attack => Character): Attack => Character = {
    attack => {
      Character.counter.incrementAndGet()
      bizlog(attack)
    }
  }

  def fullDamage = addMetrics(addErrorHandling(damage, attackErrorHandler))

  def damage(attack: Attack): Character = {
    import attack._
    println(s"attacked for $hp")
    if (hp < 0) ch else if (!ch.alive) ch else {
      val newHitpoints = ch.hp - attack.hp
      if (newHitpoints < 0) {
        println(s"and is still alive with $newHitpoints")
        ch.copy(hp = newHitpoints)
      } else {
        println(s"and was killed")
        Character(0, false)
      }
    }
  }

}


case class Character(hp: Int = 1000, alive: Boolean = true) {
}


trait Functor[F[_]] {
  def map[T, T1](f: F[T], fn: T => T1): F[T1]
}
object Functor {
  def apply[F[_], T, T1](f: F[T], fn: T => T1)(implicit functor: Functor[F]): F[T1] = functor.map(f, fn)
  implicit def functorForList: Functor[List] = new Functor[List] {
    override def map[T, T1](f: List[T], fn: T => T1): List[T1] = f.map(fn)
  }
  implicit def functorForOption: Functor[Option] = new Functor[Option] {
    override def map[T, T1](f: Option[T], fn: T => T1): Option[T1] = f.map(fn)
  }
}

trait Exercise1 {
  def lift[T, T1](fn: T => T1): (List[T] => List[T1]) = _.map(fn)
  def liftV[T, T1](fn: T => T1): (Vector[T] => Vector[T1]) = _.map(fn)
  def liftS[T, T1](fn: T => T1): (Set[T] => Set[T1]) = _.map(fn)


  def liftC[T, T1](fn: T => T1)(list: List[T]): List[T1] = list.map(fn)
  def liftF[F[_] : Functor, T, T1](fn: T => T1): (F[T] => F[T1]) = f => Functor[F, T, T1](f, fn)
  def liftFC[F[_] : Functor, T, T1](fn: T => T1)(f: F[T]): F[T1] = Functor[F, T, T1](f, fn)
}

object Exercise1 extends App with Exercise1 {
  val list = List(1, 2, 3)
  val vector = Vector(1, 2, 3)

  //curried Named a mathematician
  def myFunc[T, T1](fn: T => T1)(list: List[T]): List[T1] = list.map(fn)
//  def myFuncV[F[_], T, T1](fn: T => T1)(list: F[T]): F[T1] = list.map(fn)
//
//
//  println(myFuncV((x: Int) => x + 3)(vector))

  //
  //  println(liftV((x: Int) => x.toString)(vector))
  //
  //  val addOneToEveryElement: (List[Int] => List[Int]) = lift((i: Int) => i + 1)
  //  val addOneToEveryElementF: (List[Int] => List[Int]) = liftF((i: Int) => i + 1)
  //
  //  println("lift")
  //  println(addOneToEveryElement(list))
  //  println("liftF")
  //  println(addOneToEveryElementF(list))
}
