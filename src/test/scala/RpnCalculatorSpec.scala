import com.rpn.RpnCalculator
import org.scalatest._

final class RpnCalculatorSpec extends WordSpec with Matchers {
  "A RpnCalculator" should {
    "return the same digit when we have a single digit" in {
      val calculation = "10"
      RpnCalculator.calculate(calculation) shouldBe "10"
    }

    "return two numbers" in {
      val calculation = "10 20"
      RpnCalculator.calculate(calculation) shouldBe "10 20"
    }

    "return numbers when there are no calculations" in {
      val calculation = "10 20 30 1 2 3"
      RpnCalculator.calculate(calculation) shouldBe "10 20 30 1 2 3"
    }

    "add two numbers" in {
      val calculation = "10 20 +"
      RpnCalculator.calculate(calculation) shouldBe "30"
    }

    "calculate a complex calculation" in {
      val calculation = "3 5 8 * 7 + *"
      RpnCalculator.calculate(calculation) shouldBe "141"
    }
  }
}
