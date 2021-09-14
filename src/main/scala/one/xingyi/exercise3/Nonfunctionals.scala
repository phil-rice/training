package one.xingyi.exercise3
import one.xingyi.exercise3.Nonfunctionals.{Bizlogic, BizlogicTx}


object Nonfunctionals {
  type Bizlogic[From, To] = From => To
  type BizlogicTx[From, To] = Bizlogic[From, To] => Bizlogic[From, To]
  def sideeffect[From, To](block: (From, To) => Unit): BizlogicTx[From, To] = { fn => attack => val result = fn(attack); block(attack, result); result }

  def errorHandler[From, To](implicit errorHandler: ErrorHandler[From, To]): BizlogicTx[From, To] =
    fn => attack => try {fn(attack) } catch {case e: Exception => errorHandler(attack, e)}

  def metrics[From: Metrics, To]: BizlogicTx[From, To] = sideeffect((attack, _) => Metrics.apply(attack))
  def log[From, To](implicit logger: Logger, logMessage: LogMessage[From, To]): BizlogicTx[From, To] = sideeffect((from, to) => logger.info(LogMessage(from, to)))

  def compose[From, To](txs: BizlogicTx[From, To]*): BizlogicTx[From, To] =
    fn => txs.foldLeft(fn)((acc, v) => v(acc))

  def nonfunctionals[From: Metrics, To](implicit logger: Logger, logMessage: LogMessage[From, To], handler: ErrorHandler[From, To]): BizlogicTx[From, To] =
    compose[From, To](metrics, log, errorHandler)
}

object NonfunctionalsLanguage extends NonfunctionalsLanguage
trait NonfunctionalsLanguage {
  implicit class NonFuncOps[From, To](nf: BizlogicTx[From, To]) {
    def |+|(other: BizlogicTx[From, To]): BizlogicTx[From, To] = (bizLogic: Bizlogic[From, To]) => nf(other(bizLogic))
  }
}
