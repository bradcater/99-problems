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
  "P35" should "find primeFactors" in {
    (315: S99Int).primeFactors should be (List(3, 3, 5, 7))
  }
  "P36" should "find primeFactorMultiplicity" in {
    (315: S99Int).primeFactorMultiplicity should be (Map(3 -> 2, 5 -> 1, 7 -> 1))
  }
  "P37" should "find phi" in {
    (10: S99Int).phi should be (4)
  }
  "P39" should "find listPrimesinRange" in {
    (new S99Int(0)).listPrimesinRange(7 to 31) should be (List(7, 11, 13, 17, 19, 23, 29, 31))
  }
  "P40" should "find goldbach" in {
    (28: S99Int).goldbach should be ((5, 23))
  }
  "P41" should "find printGoldbachList" in {
    (new S99Int(0)).printGoldbachList(9 to 20) should be ()
    //(new S99Int(0)).printGoldbachListLimited(2 to 3000, 50) should be ()
  }
}
