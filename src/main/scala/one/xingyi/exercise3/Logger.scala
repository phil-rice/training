package one.xingyi.exercise3

trait Logger {
  def info(msg: String)
  def error(msg: String, e: Throwable)
}

trait LogMessage[From, To] {
  def apply(from: From, to: To): String
}
object LogMessage {
  def apply[From, To](from: From, to: To)(implicit logMessage: LogMessage[From, To]): String = logMessage(from, to)
}

object PrintlnLogger extends Logger {
  override def info(msg: String): Unit = println(msg)
  override def error(msg: String, e: Throwable): Unit = {e.printStackTrace(); println(msg) }
}

