import scala.annotation.tailrec

// Problems taken from:
//   http://aperiodic.net/phil/scala/s-99/
final class Lists {
  // P01 (*) Find the last element of a list.
  // Example:
  // scala> last(List(1, 1, 2, 3, 5, 8))
  // res0: Int = 8
  @tailrec def last[T](l: List[T]) : T = l match {
    case h :: Nil => h
    case _        => last(l.tail)
  }
  // P02 (*) Find the last but one element of a list.
  // Example:
  // scala> penultimate(List(1, 1, 2, 3, 5, 8))
  // res0: Int = 5
  @tailrec def penultimate[T](l: List[T]) : T = l match {
    case h1 :: h2 :: Nil => h1
    case _               => penultimate(l.tail)
  }
  // P03 (*) Find the Kth element of a list.
  // By convention, the first element in the list is element 0.
  // Example:
  // 
  // scala> nth(2, List(1, 1, 2, 3, 5, 8))
  // res0: Int = 2
  def nth[T](k: Int, l: List[T]) : T = {
    @tailrec def loop(c: Int, lst: List[T]) : T = c match {
      case `k` => lst.head
      case _   => loop(c + 1, lst.tail)
    }
    loop(0, l)
  }
  // P04 (*) Find the number of elements of a list.
  // Example:
  // scala> length(List(1, 1, 2, 3, 5, 8))
  // res0: Int = 6
  def length[T](l: List[T]) : Int = {
    @tailrec def loop(c: Int, lst: List[T]) : Int = lst match {
      case Nil => c
      case _   => loop(c + 1, lst.tail)
    }
    loop(0, l)
  }
  def empty[T](l: List[T]) : Boolean = {
    length(l) == 0
  }
  // P05 (*) Reverse a list.
  // Example:
  // scala> reverse(List(1, 1, 2, 3, 5, 8))
  // res0: List[Int] = List(8, 5, 3, 2, 1, 1)
  def reverse[T](l: List[T]) : List[T] = {
    @tailrec def loop(acc: List[T], lst: List[T]) : List[T] = lst match {
      case Nil => acc
      case _   => loop(lst.head :: acc, lst.tail)
    }
    loop(Nil, l)
  }
  // P06 (*) Find out whether a list is a palindrome.
  // Example:
  // scala> isPalindrome(List(1, 2, 3, 2, 1))
  // res0: Boolean = true
  @tailrec def isPalindrome[T](l: List[T]) : Boolean = {
    if (empty(l) || length(l) == 1) true
    else if (l.head == last(l)) isPalindrome(reverse(reverse(l.tail).tail))
    else false
  }
  // P07 (**) Flatten a nested list structure.
  // Example:
  // scala> flatten(List(List(1, 1), 2, List(3, List(5, 8))))
  // res0: List[Any] = List(1, 1, 2, 3, 5, 8)
  // FIXME This has a warning about T being unchecked.
  def flatten[T](l: List[T]) : List[T] = {
    @tailrec def loop(acc: List[T], lst: List[T]) : List[T] = {
      if (empty(lst)) {
        acc
      } else if (lst.head.isInstanceOf[List[T]]) {
        loop(reverse(flatten(lst.head.asInstanceOf[List[T]])) ::: acc, lst.tail)
      } else {
        loop(lst.head.asInstanceOf[T] :: acc, lst.tail)
      }
    }
    reverse(loop(Nil, l))
  }
  // P08 (**) Eliminate consecutive duplicates of list elements.
  // If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
  // Example:
  // 
  // scala> compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[Symbol] = List('a, 'b, 'c, 'a, 'd, 'e)
  def compress[T](l: List[T]) : List[T] = {
    @tailrec def loop(acc: List[T], lst: List[T]) : List[T] = {
      if (empty(lst)) acc
      else if (acc.head == lst.head) loop(acc, lst.tail)
      else loop(lst.head :: acc, lst.tail)
    }
    reverse(loop(List(l.head), l.tail))
  }
  // P09 (**) Pack consecutive duplicates of list elements into sublists.
  // If a list contains repeated elements they should be placed in separate sublists.
  // Example:
  // 
  // scala> pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[List[Symbol]] = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e))
  def pack[T](l: List[T]) : List[List[T]] = {
    @tailrec def loop(acc: List[List[T]], lst: List[T]) : List[List[T]] = {
      if (empty(lst)) {
        acc
      } else if (acc.head.head == lst.head) {
        loop(List(lst.head :: acc.head) ::: acc.tail, lst.tail)
      } else {
        loop(List(List(lst.head)) ::: acc, lst.tail)
      }
    }
    loop(List(List(reverse(l).head)), reverse(l).tail)
  }
  // P10 (*) Run-length encoding of a list.
  // Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
  // Example:
  // 
  // scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  def encode[T](l: List[T]) : List[(Int, T)] = {
    @tailrec def loop(acc: List[(Int, T)], lst: List[List[T]]) : List[(Int, T)] = lst match {
      case Nil => acc
      case _   => loop((length(lst.head), lst.head.head) :: acc, lst.tail)
    }
    loop(Nil, pack(reverse(l)))
  }
  // P11 (*) Modified run-length encoding.
  // Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N, E) terms.
  // Example:
  // 
  // scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
  // FIXME Use generic type T.
  def encodeModified(l: List[Any]) : List[Any] = {
    @tailrec def loop(acc: List[Any], lst: List[(Int, Any)]) : List[Any] = {
      if (empty(lst)) acc
      else if (lst.head._1 == 1) loop(lst.head._2 :: acc, lst.tail)
      else loop(lst.head :: acc, lst.tail)
    }
    reverse(loop(Nil, encode(l)))
  }
  // P12 (**) Decode a run-length encoded list.
  // Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
  // Example:
  // 
  // scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
  // res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  // FIXME Use generic type T.
  def decode(l: List[(Int, Any)]) : List[Any] = {
    @tailrec def loop(acc: List[Any], lst: List[(Int, Any)]) : List[Any] = {
      if (empty(lst)) acc
      else if (lst.head._1 == 0) loop(acc, lst.tail)
      else loop(lst.head._2 :: acc, (lst.head._1 - 1, lst.head._2) :: lst.tail)
    }
    reverse(loop(Nil, l))
  }
  // P13 (**) Run-length encoding of a list (direct solution).
  // Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written (like P09's pack); do all the work directly.
  // Example:
  // 
  // scala> encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  // res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
  // P14 (*) Duplicate the elements of a list.
  // Example:
  // scala> duplicate(List('a, 'b, 'c, 'c, 'd))
  // res0: List[Symbol] = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
  def duplicate[T](l: List[T]) : List[T] = {
    duplicateN(2, l)
  }
  // P15 (**) Duplicate the elements of a list a given number of times.
  // Example:
  // scala> duplicateN(3, List('a, 'b, 'c, 'c, 'd))
  // res0: List[Symbol] = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
  def duplicateN[T](n: Int, l: List[T]) : List[T] = {
    @tailrec def loop(r: Int, acc: List[T], lst: List[T]) : List[T] = {
      if (empty(lst)) acc
      else if (r == n) loop(0, acc, lst.tail)
      else loop(r + 1, lst.head :: acc, lst)
    }
    loop(0, Nil, reverse(l))
  }
  // P16 (**) Drop every Nth element from a list.
  // Example:
  // scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
  def drop[T](n: Int, l: List[T]) : List[T] = {
    @tailrec def loop(r: Int, acc: List[T], lst: List[T]) : List[T] = {
      if (empty(lst)) acc
      else if (r == n) loop(1, acc, lst.tail)
      else loop(r + 1, lst.head :: acc, lst.tail)
    }
    reverse(loop(1, Nil, l))
  }
  // P17 (*) Split a list into two parts.
  // The length of the first part is given. Use a Tuple for your result.
  // Example:
  // 
  // scala> split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res0: (List[Symbol], List[Symbol]) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  def split(n: Int, l: List[Any]) : (List[Any], List[Any]) = {
    @tailrec def loop(r: Int, acc: List[Any], lst: List[Any]) : (List[Any], List[Any]) = r match {
      case `n` => (reverse(acc), lst)
      case _   => loop(r + 1, lst.head :: acc, lst.tail)
    }
    loop(0, Nil, l)
  }
  // P18 (**) Extract a slice from a list.
  // Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including the Kth element of the original list. Start counting the elements with 0.
  // Example:
  // 
  // scala> slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res0: List[Symbol] = List('d, 'e, 'f, 'g)
  def slice[T](a: Int, b: Int, l: List[T]) : List[T] = {
    @tailrec def loop(i: Int, acc: List[T], lst: List[T]) : List[T] = {
      if (i >= b) reverse(acc)
      else if (i >= a) loop(i + 1, lst.head :: acc, lst.tail)
      else loop(i + 1, acc, lst.tail)
    }
    loop(0, Nil, l)
  }
  // P19 (**) Rotate a list N places to the left.
  // Examples:
  // scala> rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res0: List[Symbol] = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
  // 
  // scala> rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
  // res1: List[Symbol] = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
  @tailrec def rotate[T](n: Int, l: List[T]) : List[T] = {
    if (n < 0) rotate(length(l) + n, l)
    else slice(n, length(l), l) ::: slice(0, n, l)
  }
  // P20 (*) Remove the Kth element from a list.
  // Return the list and the removed element in a Tuple. Elements are numbered from 0.
  // Example:
  // 
  // scala> removeAt(1, List('a, 'b, 'c, 'd))
  // res0: (List[Symbol], Symbol) = (List('a, 'c, 'd),'b)
  def removeAt[T](k: Int, l: List[T]) : (List[T], T) = {
    (slice(0, k, l) ::: slice(k + 1, length(l), l), nth(k, l))
  }
  // P21 (*) Insert an element at a given position into a list.
  // Example:
  // scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
  // res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
  def insertAt[T](elm: T, n: Int, l: List[T]) : List[T] = {
    slice(0, n, l) ::: List(elm) ::: slice(n, length(l), l)
  }
  // P22 (*) Create a list containing all integers within a given range.
  // Example:
  // scala> range(4, 9)
  // res0: List[Int] = List(4, 5, 6, 7, 8, 9)
  def range(a: Int, b: Int) : List[Int] = {
    @tailrec def loop(acc: List[Int], i: Int) : List[Int] = {
      if (i < a) acc
      else loop(i :: acc, i - 1)
    }
    loop(Nil, b)
  }
  // P23 (**) Extract a given number of randomly selected elements from a list.
  // Example:
  // scala> randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h))
  // res0: List[Symbol] = List('e, 'd, 'a)
  // Hint: Use the solution to problem P20
  // 
  def randomSelect(n: Int, l: List[Any]) : List[Any] = {
    @tailrec def loop(acc: List[(List[Any], Any)]) : List[Any] = {
      if (length(acc) > n) extractRemoved(acc)
      else loop(removeAt((new scala.util.Random).nextInt(length(acc.head._1)), acc.head._1) :: acc)
    }
    def extractRemoved(lst: List[(List[Any], Any)]) : List[Any] = {
      @tailrec def loop(acc: List[Any], lstt: List[(List[Any], Any)]) : List[Any] = {
        if (empty(lstt)) acc
        else loop(lstt.head._2 :: acc, lstt.tail)
      }
      reverse(removeAt(0, loop(Nil, lst))._1)
    }
    loop(List((l, 0)))
  }
  // P24 (*) Lotto: Draw N different random numbers from the set 1..M.
  // Example:
  // scala> lotto(6, 49)
  // res0: List[Int] = List(23, 1, 17, 33, 21, 37)
  def lotto(n: Int, max: Int) : List[Int] = {
    randomSelect(n, range(1, max)).asInstanceOf[List[Int]]
  }
  // P25 (*) Generate a random permutation of the elements of a list.
  // Hint: Use the solution of problem P23.
  // Example:
  // 
  // scala> randomPermute(List('a, 'b, 'c, 'd, 'e, 'f))
  // res0: List[Symbol] = List('b, 'a, 'd, 'c, 'e, 'f)
  def randomPermute(l: List[Any]) : List[Any] = {
    randomSelect(length(l), l)
  }
  // P26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list.
  // In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficient). For pure mathematicians, this result may be great. But we want to really generate all the possibilities.
  // Example:
  // 
  // scala> combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
  // res0: List[List[Symbol]] = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...
  def combinations[T](n: Int, l: List[T]) : List[List[T]] = {
    if (n > length(l)) List()
    else if (n == 1) l.map(List(_))
    else combinations(n - 1, l.tail).map(l.head :: _) ::: combinations(n, l.tail)
  }
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
  def qsort[T](l: List[List[T]], f: (List[T], List[T]) => Boolean) : List[List[T]] = {
    if ((length(l) <= 1) || (length(l) == 2 && f(l(0), l(1)))) {
      l
    } else if (length(l) == 2) {
      List(l(1), l(0))
    } else {
      qsort(l.tail.filter(f(_, l.head)), f) ::: List(l.head) ::: qsort(l.tail.filter(!f(_, l.head)), f)
    }
  }
  def lsort[T](l: List[List[T]]) : List[List[T]] = {
    qsort(l, (a: List[T], b: List[T]) => length(a) <= length(b))
  }
  def lsortFreq[T](l: List[List[T]]) : List[List[T]] = {
    qsort(l, (a: List[T], b: List[T]) => length(l.filter(length(_) == length(a))) <= length(l.filter(length(_) == length(b))))
  }
}
