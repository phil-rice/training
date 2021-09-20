package one.xingyi.exercise6
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class NonFunctionalsTests extends AnyFlatSpec with should.Matchers {

  def fn[From, To](expected: From, to: To): (From => To) = { from => from shouldBe expected; to }


  behavior of "NonFunctionals with names"

  def fnWithMetrics(from: String)(implicit metricCounter: MetricCounter[String]) =
    NonFunctional.addMetrics[String, String] apply (fn("a", "b"))

  it should "metrics should call the original function and update the counter" in {
    var count = 0
    implicit val counter: MetricCounter[String] = () => count += 1
    val fnWithMetrics = NonFunctional.addMetrics[String,String] apply(fn("a", "b"))
    count shouldBe 0
    fnWithMetrics("a") shouldBe "b"
    count shouldBe 1
    fnWithMetrics("a") shouldBe "b"
    count shouldBe 2
  }


  behavior of "applyNormalNonFunctional"

  it should "be the composition of logging, metrics and error handling" in {
    implicit val counter: MetricCounter[String] = () => {}
    implicit val defErrorStrategy: ErrorStrategy[String, String] = (e, from) => throw e

    val ComposeNonFunctionals(txs) = NonFunctional.applyNormalNonFunctional(fn("a", "b"))
    txs.map(x =>x.name) shouldBe List("logging", "error", "metrics")
  }
}
