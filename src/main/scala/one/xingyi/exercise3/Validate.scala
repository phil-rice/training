package one.xingyi.exercise3

trait Validate[T] {
  def apply(t: T): List[String]
}
object Validate {
  def failIf[T](failFn: T => Boolean, msg: String): Validate[T] = t => if (failFn(t)) List(msg) else Nil
  def applyAndMap[T, T1](fn: T => T1)(implicit validate: Validate[T]): (T => Either[List[String], T1]) = {
    t =>
      val errors = validate(t)
      if (errors.isEmpty) Right(fn(t)) else Left(errors)
  }
  def applyAndDefaultIfFail[From, To](ifSucceed: From => To, ifFailure: (From, List[String]) => To)(implicit valididate: Validate[From]): From => To = {
    (from: From) =>
      val errors = valididate(from)
      if (errors.isEmpty) ifSucceed(from) else ifFailure(from, errors)
  }
}
