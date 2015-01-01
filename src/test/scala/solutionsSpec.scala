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
  "P14" should "find duplicate" in {
    (new Solutions()).duplicate(List('a, 'b, 'c, 'c, 'd)) should be (List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }
  "P15" should "find duplicateN" in {
    (new Solutions()).duplicateN(3, List('a, 'b, 'c, 'c, 'd)) should be (List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }
  "P16" should "find drop" in {
    (new Solutions()).drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }
  "P17" should "find split" in {
    (new Solutions()).split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be ((List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
  "P18" should "find slice" in {
    (new Solutions()).slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('d, 'e, 'f, 'g))
  }
  "P19" should "find rotate" in {
    (new Solutions()).rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    (new Solutions()).rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }
  "P20" should "find removeAt" in {
    (new Solutions()).removeAt(1, List('a, 'b, 'c, 'd)) should be ((List('a, 'c, 'd),'b))
  }
  "P21" should "find insertAt" in {
    (new Solutions()).insertAt('new, 1, List('a, 'b, 'c, 'd)) should be (List('a, 'new, 'b, 'c, 'd))
  }
  "P22" should "find range" in {
    (new Solutions()).range(4, 9) should be (List(4, 5, 6, 7, 8, 9))
  }
  "P23" should "find randomSelect" in {
    (new Solutions()).length((new Solutions()).randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))) should be (3)
  }
  "P24" should "find lotto" in {
    (new Solutions()).length((new Solutions()).lotto(6, 49)) should be (6)
  }
  "P25" should "find randomPermute" in {
    (new Solutions()).length((new Solutions()).randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))) should be (6)
  }
  "P26" should "find combinations" in {
    (new Solutions()).combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)) should be (List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), List('a, 'b, 'f), List('a, 'c, 'd), List('a, 'c, 'e), List('a, 'c, 'f), List('a, 'd, 'e), List('a, 'd, 'f), List('a, 'e, 'f), List('b, 'c, 'd), List('b, 'c, 'e), List('b, 'c, 'f), List('b, 'd, 'e), List('b, 'd, 'f), List('b, 'e, 'f), List('c, 'd, 'e), List('c, 'd, 'f), List('c, 'e, 'f), List('d, 'e, 'f)))
  }
  "P28" should "find lsort" in {
    (new Solutions()).lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) should be (List(List('o), List('m, 'n), List('d, 'e), List('d, 'e), List('f, 'g, 'h), List('a, 'b, 'c), List('i, 'j, 'k, 'l)))
  }
  "P28" should "find lsortFreq" in {
    (new Solutions()).lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) should be (List(List('i, 'j, 'k, 'l), List('o), List('f, 'g, 'h), List('a, 'b, 'c), List('d, 'e), List('m, 'n), List('d, 'e)))
  }
}
