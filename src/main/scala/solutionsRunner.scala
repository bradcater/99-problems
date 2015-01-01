object SolutionsRunner extends App {
  // P01 (*) Find the last element of a list.
  // Example:
  // scala> last(List(1, 1, 2, 3, 5, 8))
  // res0: Int = 8
  def last(l: List[Any]) : Any = l match {
    case Nil      => -1
    case h :: Nil => h
    case _        => last(l.tail)
  }
  printf("last(List(1,2,3)): %d\n",
    last(List(1,2,3)))
  // P02 (*) Find the last but one element of a list.
  // Example:
  // scala> penultimate(List(1, 1, 2, 3, 5, 8))
  // res0: Int = 5
  def penultimate(l: List[Int]) : Int = l match {
    case Nil             => -1
    case h1 :: h2 :: Nil => h1
    case _               => penultimate(l.tail)
  }
  printf("penultimate(List(1,2,3)): %d\n",
    penultimate(List(1,2,3)))
  // P03 (*) Find the Kth element of a list.
  // By convention, the first element in the list is element 0.
  // Example:
  // 
  // scala> nth(2, List(1, 1, 2, 3, 5, 8))
  // res0: Int = 2
  def nth(k: Int, l: List[Any]) : Any = {
    def loop(c: Int, lst: List[Any]) : Any = {
      if (c == k) {
        lst.head
      } else {
        loop(c + 1, lst.tail)
      }
    }
    loop(0, l)
  }
  printf("nth(2, List(1,2,3,4)): %d\n",
    nth(2, List(1,2,3,4)))
  // P04 (*) Find the number of elements of a list.
  // Example:
  // scala> length(List(1, 1, 2, 3, 5, 8))
  // res0: Int = 6
  def length(l: List[Any]) : Int = {
    def loop(c: Int, lst: List[Any]) : Int = lst match {
      case Nil => c
      case _   => loop(c + 1, lst.tail)
    }
    loop(0, l)
  }
  printf("length(List(1,2,3,4)): %d\n",
    length(List(1,2,3,4)))
  def empty(l: List[Any]) : Boolean = {
    length(l) == 0
  }
  // P05 (*) Reverse a list.
  // Example:
  // scala> reverse(List(1, 1, 2, 3, 5, 8))
  // res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  def reverse(l: List[Any]) : List[Any] = {
    def loop(acc: List[Any], lst: List[Any]) : List[Any] = lst match {
      case Nil => acc
      case _   => loop(lst.head :: acc, lst.tail)
    }
    loop(Nil, l)
  }
  printf("reverse(List(1,2,3,4)): %s\n",
    reverse(List(1,2,3,4)).mkString(" "))
  // P06 (*) Find out whether a list is a palindrome.
  // Example:
  // scala> isPalindrome(List(1, 2, 3, 2, 1))
  // res0: Boolean = true
  def isPalindrome(l: List[Any]) : Boolean = {
    if (empty(l) || length(l) == 1) {
      true
    } else if (l.head == last(l)) {
      isPalindrome(reverse(reverse(l.tail).tail))
    } else {
      false
    }
  }
  printf("isPalindrome(List(1,2,3,4)): %b\n",
    isPalindrome(List(1,2,3,4)))
  printf("isPalindrome(List(1,2,2,1)): %b\n",
    isPalindrome(List(1,2,2,1)))
  // P07 (**) Flatten a nested list structure.
  // Example:
  // scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  // res0: List[Any] = List(1, 1, 2, 3, 5, 8)
  def flatten(l: List[Any]) : List[Any] = {
    def loop(acc: List[Any], lst: List[Any]) : List[Any] = {
      if (empty(lst)) {
        acc
      } else if (lst.head.isInstanceOf[List[Any]]) {
        loop(reverse(flatten(lst.head.asInstanceOf[List[Any]])) ::: acc, lst.tail)
      } else {
        loop(lst.head.asInstanceOf[Any] :: acc, lst.tail)
      }
    }
    reverse(loop(Nil, l))
  }
  printf("flatten(List(List(1,2,3),4)): %s\n",
    flatten(List(List(1,2,3),4)))
  // P08 (**) Eliminate consecutive duplicates of list elements.
  // If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
  // Example:
  // 
  // scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
  def compress(l: List[Any]) : List[Any] = {
    def loop(acc: List[Any], lst: List[Any]) : List[Any] = {
      if (empty(lst)) {
        acc
      } else if (empty(acc)) {
        loop(lst.head :: acc, lst.tail)
      } else if (acc.head == lst.head) {
        loop(acc, lst.tail)
      } else {
        loop(lst.head :: acc, lst.tail)
      }
    }
    reverse(loop(Nil, l))
  }
  printf("compress(List(1,2,2,3,3,3,4)): %s\n",
    compress(List(1,2,2,3,3,3,4)))
  // P09 (**) Pack consecutive duplicates of list elements into sublists.
  // If a list contains repeated elements they should be placed in separate sublists.
  // Example:
  // 
  // scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  def pack(l: List[Any]) : List[List[Any]] = {
    def loop(acc: List[List[Any]], lst: List[Any]) : List[List[Any]] = {
      if (empty(lst)) {
        acc
      } else if (empty(acc)) {
        loop(List(List(lst.head)), lst.tail)
      } else if (acc.head.head == lst.head) {
        loop(List(lst.head :: acc.head) ::: acc.tail, lst.tail)
      } else {
        loop(List(List(lst.head)) ::: acc, lst.tail)
      }
    }
    loop(Nil, reverse(l))
  }
  printf("pack(List(1,2,2,3,3,3,4)): %s\n",
    pack(List(1,2,2,3,3,3,4)).mkString(" "))
  // P10 (*) Run-length encoding of a list.
  // Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
  // Example:
  // 
  // scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  def encode(l: List[Any]) : List[(Int, Any)] = {
    def loop(acc: List[(Int, Any)], lst: List[List[Any]]) : List[(Int, Any)] = lst match {
      case Nil => acc
      case _   => loop((length(lst.head), lst.head.head) :: acc, lst.tail)
    }
    loop(Nil, pack(reverse(l)))
  }
  printf("encode(List(1,2,2,3,3,3,4)): %s\n",
    encode(List(1,2,2,3,3,3,4)).mkString(" "))
  // scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  def encodeModified(l: List[Any]) : List[Any] = {
    def loop(acc: List[Any], lst: List[(Int, Any)]) : List[Any] = {
      if (empty(lst)) {
        acc
      } else if (lst.head._1 == 1) {
        loop(lst.head._2 :: acc, lst.tail)
      } else {
        loop(lst.head :: acc, lst.tail)
      }
    }
    reverse(loop(Nil, encode(l)))
  }
  printf("encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)): %s\n",
    encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)).mkString(" "))
  // P12 (**) Decode a run-length encoded list.
  // Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
  // Example:
  // 
  // scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  // res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  def decode(l: List[(Int, Any)]) : List[Any] = {
    def loop(acc: List[Any], lst: List[(Int, Any)]) : List[Any] = {
      if (empty(lst)) {
        acc
      } else if (lst.head._1 == 0) {
        loop(acc, lst.tail)
      } else {
        loop(lst.head._2 :: acc, (lst.head._1 - 1, lst.head._2) :: lst.tail)
      }
    }
    reverse(loop(Nil, l))
  }
  printf("decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))): %s\n",
    decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))))
  // P14 (*) Duplicate the elements of a list.
  // Example:
  // scala> duplicate(List('a, 'b, 'c, 'c, 'd))
  // res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  def duplicate(l: List[Any]) : List[Any] = {
    duplicateN(2, l)
  }
  printf("duplicate(List(1,2,3)): %s\n",
    duplicate(List(1,2,3)).mkString(" "))
  // P15 (**) Duplicate the elements of a list a given number of times.
  // Example:
  // scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
  // res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  def duplicateN(n: Int, l: List[Any]) : List[Any] = {
    def loop(r: Int, acc: List[Any], lst: List[Any]) : List[Any] = {
      if (empty(lst)) {
        acc
      } else if (r == n) {
        loop(0, acc, lst.tail)
      } else {
        loop(r + 1, lst.head :: acc, lst)
      }
    }
    loop(0, Nil, reverse(l))
  }
  printf("duplicateN(3, List(1, 2, 3, 3, 4)): %s\n",
    duplicateN(3, List(1, 2, 3, 3, 4)))
  // P16 (**) Drop every Nth element from a list.
  // Example:
  // scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  def drop(n: Int, l: List[Any]) : List[Any] = {
    def loop(r: Int, acc: List[Any], lst: List[Any]) : List[Any] = {
      if (empty(lst)) {
        acc
      } else if (r == n) {
        loop(1, acc, lst.tail)
      } else {
        loop(r + 1, lst.head :: acc, lst.tail)
      }
    }
    reverse(loop(1, Nil, l))
  }
  printf("drop(3, List(1,2,3,4,5,6,7,8,9,10)): %s\n",
    drop(3, List(1,2,3,4,5,6,7,8,9,10)).mkString(" "))
  // P17 (*) Split a list into two parts.
  // The length of the first part is given. Use a Tuple for your result.
  // Example:
  // 
  // scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  def split(n: Int, l: List[Any]) : (List[Any], List[Any]) = {
    def loop(r: Int, acc: List[Any], lst: List[Any]) : (List[Any], List[Any]) = {
      if (r == n) {
        (reverse(acc), lst)
      } else {
        loop(r + 1, lst.head :: acc, lst.tail)
      }
    }
    loop(0, Nil, l)
  }
  printf("split(3, List(1,2,3,4,5,6,7,8,9,10)): %s\n",
    split(3, List(1,2,3,4,5,6,7,8,9,10)))
  // P18 (**) Extract a slice from a list.
  // Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
  // Example:
  // 
  // scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res0: List[Symbol] = List('d, 'e, 'f, 'g)
  def slice(a: Int, b: Int, l: List[Any]) : List[Any] = {
    def loop(i: Int, acc: List[Any], lst: List[Any]) : List[Any] = {
      if (i >= b) {
        reverse(acc)
      } else if (i >= a) {
        loop(i + 1, lst.head :: acc, lst.tail)
      } else {
        loop(i + 1, acc, lst.tail)
      }
    }
    loop(0, Nil, l)
  }
  printf("slice(3, 7, List(1,2,3,4,5,6,7,8,9,10)): %s\n",
    slice(3, 7, List(1,2,3,4,5,6,7,8,9,10)))
  // P19 (**) Rotate a list N places to the left.
  // Examples:
  // scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
  // 
  // scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  def rotate(n: Int, l: List[Any]) : List[Any] = {
    if (n < 0) {
      rotate(length(l) + n, l)
    } else {
      slice(n, length(l), l) ::: slice(0, n, l)
    }
  }
  printf("rotate(3, List(1,2,3,4,5,6,7,8,9,10)): %s\n",
    rotate(3, List(1,2,3,4,5,6,7,8,9,10)))
  printf("rotate(-2, List(1,2,3,4,5,6,7,8,9,10)): %s\n",
    rotate(-2, List(1,2,3,4,5,6,7,8,9,10)))
  // P20 (*) Remove the Kth element from a list.
  // Return the list and the removed element in a Tuple. Elements are numbered from 0.
  // Example:
  // 
  // scala> removeAt(1, List('a, 'b, 'c, 'd))
  // res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
  def removeAt(k: Int, l: List[Any]) : (List[Any], Any) = {
    (slice(0, k, l) ::: slice(k + 1, length(l), l), nth(k, l))
  }
  printf("removeAt(1, List(1,2,3,4)): %s\n",
    removeAt(1, List(1,2,3,4)))
  // P21 (*) Insert an element at a given position into a list.
  // Example:
  // scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
  // res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
  def insertAt(elm: Any, n: Int, l: List[Any]) : List[Any] = {
    slice(0, n, l) ::: List(elm) ::: slice(n, length(l), l)
  }
  printf("insertAt(5, 1, List(1,2,3,4)): %s\n",
    insertAt(5, 1, List(1,2,3,4)))
  // P22 (*) Create a list containing all integers within a given range.
  // Example:
  // scala> range(4, 9)
  // res0: List[Int] = List(4, 5, 6, 7, 8, 9)
  def range(a: Int, b: Int) : List[Int] = {
    def loop(acc: List[Int], i: Int) : List[Int] = {
      if (i < a) {
        acc
      } else {
        loop(i :: acc, i - 1)
      }
    }
    loop(Nil, b)
  }
  printf("range(4, 9): %s\n",
    range(4, 9))
  // P23 (**) Extract a given number of randomly selected elements from a list.
  // Example:
  // scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
  // res0: List[Symbol] = List('e, 'd, 'a)
  // Hint: Use the solution to problem P20
  // 
  // scala> removeAt(1, List('a, 'b, 'c, 'd))
  // res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
  def randomSelect(n: Int, l: List[Any]) : List[Any] = {
    def loop(acc: List[(List[Any], Any)]) : List[Any] = {
      if (length(acc) > n) {
        extractRemoved(acc)
      } else {
        loop(removeAt((new scala.util.Random).nextInt(length(acc.head._1)), acc.head._1) :: acc)
      }
    }
    def extractRemoved(lst: List[(List[Any], Any)]) : List[Any] = {
      def loop(acc: List[Any], lstt: List[(List[Any], Any)]) : List[Any] = {
        if (empty(lstt)) {
          acc
        } else {
          loop(lstt.head._2 :: acc, lstt.tail)
        }
      }
      reverse(removeAt(0, loop(Nil, lst))._1)
    }
    loop(List((l, 0)))
  }
  printf("randomSelect(3, List(1,2,3,4,5,6,7)): %s\n",
    randomSelect(3, List(1,2,3,4,5,6,7)))
  // P24 (*) Lotto: Draw N different random numbers from the set 1..M.
  // Example:
  // scala> lotto(6, 49)
  // res0: List[Int] = List(23, 1, 17, 33, 21, 37)
  def lotto(n: Int, max: Int) : List[Int] = {
    randomSelect(n, range(1, max)).asInstanceOf[List[Int]]
  }
  printf("lotto(6, 49): %s\n",
    lotto(6, 49))
  // P25 (*) Generate a random permutation of the elements of a list.
  // Hint: Use the solution of problem P23.
  // Example:
  // 
  // scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
  // res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
  def randomPermute(l: List[Any]) : List[Any] = {
    randomSelect(length(l), l)
  }
  printf("randomPermute(List(1,2,3,4,5)): %s\n",
    randomPermute(List(1,2,3,4,5)))
  // P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
  // In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
  // Example:
  // 
  // scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
  // List(List('a), List('b), List('c))
  // 'a + combinations(n - 1, 'b...)
  // 'b + combinations(n - 1, 'c...)
  // res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
  def combinations(n: Int, l: List[Any]) : List[List[Any]] = {
    if (n > length(l)) {
      List()
    } else if (n == 1) {
      l.map(List(_))
    } else {
      combinations(n - 1, l.tail).map(l.head :: _) ::: combinations(n, l.tail)
    }
  }
  printf("combinations(3, List('a,'b,'c,'d)): %s\n",
    combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)))
  // P27 (**) Group the elements of a set into disjoint subsets.
  // a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a function that generates all the possibilities.
  // Example:
  // 
  // scala> group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
  // res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
  // b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
  // 
  // Example:
  // 
  // scala> group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida"))
  // res0: List[List[List[String]]] = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
  // Note that we do not want permutations of the group members; i.e. ((Aldo, Beat), ...) is the same solution as ((Beat, Aldo), ...). However, we make a difference between ((Aldo, Beat), (Carla, David), ...) and ((Carla, David), (Aldo, Beat), ...).
  // 
  // You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients".
  // 
  // P28 (**) Sorting a list of lists according to length of sublists.
  // a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of the list according to their length. E.g. short lists first, longer lists later, or vice versa.
  // Example:
  // 
  // scala> lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
  // res0: List[List[Symbol]] = List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l))
  // b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements according to their length frequency; i.e. in the default, sorting is done ascendingly, lists with rare lengths are placed, others with a more frequent length come later.
  // 
  // Example:
  // 
  // scala> lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o)))
  // res1: List[List[Symbol]] = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
  // Note that in the above example, the first two lists in the result have length 4 and 1 and both lengths appear just once. The third and fourth lists have length 3 and there are two list of this length. Finally, the last three lists have length 2. This is the most frequent length.
  def qsort(l: List[List[Any]], f: (List[Any], List[Any]) => Boolean) : List[List[Any]] = {
    if (length(l) <= 1) {
      l
    } else if (length(l) == 2 && f(l(0), l(1))) {
      l
    } else if (length(l) == 2) {
      List(l(1), l(0))
    } else {
      qsort(l.tail.filter(f(_, l.head)), f) ::: List(l.head) ::: qsort(l.tail.filter(!f(_, l.head)), f)
    }
  }
  def lsort(l: List[List[Any]]) : List[List[Any]] = {
    qsort(l, (a: List[Any], b: List[Any]) => length(a) <= length(b))
  }
  printf("lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))): %s\n",
    lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))))
  def lsortFreq(l: List[List[Any]]) : List[List[Any]] = {
    qsort(l, (a: List[Any], b: List[Any]) => length(l.filter(length(_) == length(a))) <= length(l.filter(length(_) == length(b))))
  }
  printf("lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))): %s\n",
    lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))))
}
