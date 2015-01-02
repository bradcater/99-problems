import org.scalatest._

class ListsSpec extends FlatSpec with Matchers {
  "P1" should "find last" in {
    (new Lists()).last(List(1, 1, 2, 3, 5, 8)) should be (8)
  }
  "P2" should "find penultimate" in {
    (new Lists()).penultimate(List(1, 1, 2, 3, 5, 8)) should be (5)
  }
  "P3" should "find nth" in {
    (new Lists()).nth(2, List(1, 1, 2, 3, 5, 8)) should be (2)
  }
  "P4" should "find length" in {
    (new Lists()).length(List(1, 1, 2, 3, 5, 8)) should be (6)
  }
  "P5" should "find reverse" in {
    (new Lists()).reverse(List(1, 1, 2, 3, 5, 8)) should be (List(8, 5, 3, 2, 1, 1))
  }
  "P6" should "find isPalindrome" in {
    (new Lists()).isPalindrome(List(1, 2, 3, 2, 1)) should be (true)
    (new Lists()).isPalindrome(List(1, 2, 3, 1)) should be (false)
  }
  "P7" should "find flatten" in {
    (new Lists()).flatten(List(List(1, 1), 2, List(3, List(5, 8)))) should be (List(1, 1, 2, 3, 5, 8))
  }
  "P8" should "find compress" in {
    (new Lists()).compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List('a, 'b, 'c, 'a, 'd, 'e))
  }
  "P9" should "find pack" in {
    (new Lists()).pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }
  "P10" should "find encode" in {
    (new Lists()).encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }
  "P11" should "find encodeModified" in {
    (new Lists()).encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) should be (List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  }
  "P12" should "find decode" in {
    (new Lists()).decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) should be (List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }
  "P14" should "find duplicate" in {
    (new Lists()).duplicate(List('a, 'b, 'c, 'c, 'd)) should be (List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }
  "P15" should "find duplicateN" in {
    (new Lists()).duplicateN(3, List('a, 'b, 'c, 'c, 'd)) should be (List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }
  "P16" should "find drop" in {
    (new Lists()).drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }
  "P17" should "find split" in {
    (new Lists()).split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be ((List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
  "P18" should "find slice" in {
    (new Lists()).slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('d, 'e, 'f, 'g))
  }
  "P19" should "find rotate" in {
    (new Lists()).rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    (new Lists()).rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) should be (List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }
  "P20" should "find removeAt" in {
    (new Lists()).removeAt(1, List('a, 'b, 'c, 'd)) should be ((List('a, 'c, 'd),'b))
  }
  "P21" should "find insertAt" in {
    (new Lists()).insertAt('new, 1, List('a, 'b, 'c, 'd)) should be (List('a, 'new, 'b, 'c, 'd))
  }
  "P22" should "find range" in {
    (new Lists()).range(4, 9) should be (List(4, 5, 6, 7, 8, 9))
  }
  "P23" should "find randomSelect" in {
    (new Lists()).length((new Lists()).randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))) should be (3)
  }
  "P24" should "find lotto" in {
    (new Lists()).length((new Lists()).lotto(6, 49)) should be (6)
  }
  "P25" should "find randomPermute" in {
    (new Lists()).length((new Lists()).randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))) should be (6)
  }
  "P26" should "find combinations" in {
    (new Lists()).combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)) should be (List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), List('a, 'b, 'f), List('a, 'c, 'd), List('a, 'c, 'e), List('a, 'c, 'f), List('a, 'd, 'e), List('a, 'd, 'f), List('a, 'e, 'f), List('b, 'c, 'd), List('b, 'c, 'e), List('b, 'c, 'f), List('b, 'd, 'e), List('b, 'd, 'f), List('b, 'e, 'f), List('c, 'd, 'e), List('c, 'd, 'f), List('c, 'e, 'f), List('d, 'e, 'f)))
  }
  "P28" should "find lsort" in {
    (new Lists()).lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) should be (List(List('o), List('m, 'n), List('d, 'e), List('d, 'e), List('f, 'g, 'h), List('a, 'b, 'c), List('i, 'j, 'k, 'l)))
  }
  "P28" should "find lsortFreq" in {
    (new Lists()).lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) should be (List(List('i, 'j, 'k, 'l), List('o), List('f, 'g, 'h), List('a, 'b, 'c), List('d, 'e), List('m, 'n), List('d, 'e)))
  }
}
