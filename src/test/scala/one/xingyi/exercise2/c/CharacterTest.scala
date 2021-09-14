package one.xingyi.exercise2.c
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class CharacterTest extends  AnyFlatSpec with should.Matchers{

  behavior of "Character"


  it should "take damage" in {
     Character.attack(Character(1000, true), 100) shouldBe Character(900, true)
     Character.attack(Character(1000, true), 1001) shouldBe Character(0, false)
     Character.attack(Character(1000, true), 1000) shouldBe Character(0, true)
  }

}
