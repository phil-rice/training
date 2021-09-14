package one.xingyi.exercise3

trait ErrorHandler[From, To] {
  def apply(from: From, e: Throwable): To
}
object ErrorHandler {
  implicit def apply[From, To](from: From, e: Throwable)(implicit errorHandler: ErrorHandler[From, To]) = errorHandler(from, e)
  implicit def logAndRethrow[From: ErrorMessage, To](implicit logger: Logger): ErrorHandler[From, To] = { (from, t) => logger.error(ErrorMessage(from, t), t); throw t }
}

trait ErrorMessage[From] {
  def apply(from: From, e: Throwable): String
}
object ErrorMessage {
  def apply[From](f: From, e: Throwable)(implicit errorMessage: ErrorMessage[From]) = errorMessage(f, e)
  def errorMessage[From]: ErrorMessage[From] = (f, e) => s"Unexpected error ${e.getClass.getSimpleName} ${e.getMessage}"
}
