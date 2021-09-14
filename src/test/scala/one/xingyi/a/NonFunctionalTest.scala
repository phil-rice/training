package one.xingyi.a
import one.xingyi.exercise2.CharacterD.NonFunctional
import org.mockito.Matchers.anyChar
import org.mockito.Mockito
import org.mockito.Mockito.{mock, when}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import java.util.concurrent.atomic.AtomicInteger

class NonFunctionalTest extends AnyFlatSpec with should.Matchers {


  //  def addMatrix[From, To](counter: AtomicInteger)(fn: From => To): From => To =
  //    from => {
  //      counter.incrementAndGet()
  //      fn(from)
  //    }


  def fn[A, B](expected: A, result: B): (A => B) = {
    actual =>
      actual shouldBe expected
      result
  }

  val bizLogic = fn("a", "b")
  def bizLogic2(s: String) = s + "_wascalled"

  behavior of "Non Functionals - metrics"

  it should "return the results of the business logic when called" in {
    val decoratedBizLogic = NonFunctional.addMatrix(new AtomicInteger())(bizLogic2)
    decoratedBizLogic("a") shouldBe "a_wascalled"
  }

  it should "increment the counter every time the bizlogic is called" in {
    val counter = new AtomicInteger()
    val decoratedBizLogic = NonFunctional.addMatrix(counter)(bizLogic)
    counter.get() shouldBe 0
    decoratedBizLogic("a")
    counter.get shouldBe 1
  }


}
