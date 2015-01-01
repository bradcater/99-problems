import org.scalatest._

import arithmetic._

class ArithmeticSpec extends FlatSpec with Matchers {
  "P31" should "find isPrime" in {
    (7: S99Int).isPrime should be (true)
  }
  "P32" should "find gcd" in {
    (new S99Int(0)).gcd(36, 63) should be (9)
  }
  "P33" should "find isCoprimeTo" in {
    (35: S99Int).isCoprimeTo(64) should be (true)
  }
  "P34" should "find totient" in {
    (10: S99Int).totient should be (4)
  }
}
