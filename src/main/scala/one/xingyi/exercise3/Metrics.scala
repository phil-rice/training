package one.xingyi.exercise3

object Metrics {
  def apply[From](f: From)(implicit metrics: Metrics[From]) = metrics.addOneToCount(f)
}
trait Metrics[From] {
  def addOneToCount(f: From)
}
