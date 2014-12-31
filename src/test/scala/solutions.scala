import org.scalatest._

class SolutionsSpec extends FlatSpec with Matchers {
  "P1" should "find last" in {
    (new Solutions()).last(List(1, 1, 2, 3, 5, 8)) should be (8)
  }
  "P2" should "find penultimate" in {
    (new Solutions()).penultimate(List(1, 1, 2, 3, 5, 8)) should be (5)
  }
  "P3" should "find nth" in {
    (new Solutions()).nth(2, List(1, 1, 2, 3, 5, 8)) should be (2)
  }
  "P4" should "find length" in {
    (new Solutions()).length(List(1, 1, 2, 3, 5, 8)) should be (6)
  }
  "P5" should "find reverse" in {
    (new Solutions()).reverse(List(1, 1, 2, 3, 5, 8)) should be (List(8, 5, 3, 2, 1, 1))
  }
  "P6" should "find isPalindrome" in {
    (new Solutions()).isPalindrome(List(1, 2, 3, 2, 1)) should be (true)
    (new Solutions()).isPalindrome(List(1, 2, 3, 1)) should be (false)
  }
  "P7" should "find flatten" in {
    (new Solutions()).flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be (List(1, 1, 2, 3, 5, 8))
  }
  "P8" should "find compress" in {
    (new Solutions()).compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List('a, 'b, 'c, 'a, 'd, 'e))
  }
  "P9" should "find pack" in {
    (new Solutions()).pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }
  "P10" should "find encode" in {
    (new Solutions()).encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }
  "P11" should "find encodeModified" in {
    (new Solutions()).encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  }
  "P12" should "find decode" in {
    (new Solutions()).decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should be (List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }
}
