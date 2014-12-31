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
        return lst.head
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
    if (length(l) == 0 || length(l) == 1) {
      return true
    } else if (l.head == last(l)) {
      return isPalindrome(reverse(reverse(l.tail).tail))
    } else {
      return false
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
      if (lst.length == 0) {
        return acc
      } else if (lst.head.isInstanceOf[List[Any]]) {
        return loop(reverse(flatten(lst.head.asInstanceOf[List[Any]])) ++ acc, lst.tail)
      } else {
        return loop(lst.head.asInstanceOf[Any] :: acc, lst.tail)
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
      if (length(lst) == 0) {
        return acc
      } else if (length(acc) == 0) {
        return loop(lst.head :: acc, lst.tail)
      } else if (acc.head == lst.head) {
        return loop(acc, lst.tail)
      } else {
        return loop(lst.head :: acc, lst.tail)
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
      if (length(lst) == 0) {
        return acc
      } else if (length(acc) == 0) {
        return loop(List(List(lst.head)), lst.tail)
      } else if (acc.head.head == lst.head) {
        return loop(List(lst.head :: acc.head) ++ acc.tail, lst.tail)
      } else {
        return loop(List(List(lst.head)) ++ acc, lst.tail)
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
      if (length(lst) == 0) {
        return acc
      } else if (lst.head._1 == 1) {
        return loop(lst.head._2 :: acc, lst.tail)
      } else {
        return loop(lst.head :: acc, lst.tail)
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
      if (length(lst) == 0) {
        return acc
      } else if (lst.head._1 == 0) {
        return loop(acc, lst.tail)
      } else {
        return loop(lst.head._2 :: acc, (lst.head._1 - 1, lst.head._2) :: lst.tail)
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
      if (length(lst) == 0) {
        return acc
      } else if (r == n) {
        return loop(0, acc, lst.tail)
      } else {
        return loop(r + 1, lst.head :: acc, lst)
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
      if (length(lst) == 0) {
        return acc
      } else if (r == n) {
        return loop(1, acc, lst.tail)
      } else {
        return loop(r + 1, lst.head :: acc, lst.tail)
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
        return (reverse(acc), lst)
      } else {
        return loop(r + 1, lst.head :: acc, lst.tail)
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
        return reverse(acc)
      } else if (i >= a) {
        return loop(i + 1, lst.head :: acc, lst.tail)
      } else {
        return loop(i + 1, acc, lst.tail)
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
      slice(n, length(l), l) ++ slice(0, n, l)
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
    (slice(0, k, l) ++ slice(k + 1, length(l), l), nth(k, l))
  }
  printf("removeAt(1, List(1,2,3,4)): %s\n",
    removeAt(1, List(1,2,3,4)))
  // P21 (*) Insert an element at a given position into a list.
  // Example:
  // scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
  // res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)
  def insertAt(elm: Any, n: Int, l: List[Any]) : List[Any] = {
    slice(0, n, l) ++ List(elm) ++ slice(n, length(l), l)
  }
  printf("insertAt(5, 1, List(1,2,3,4)): %s\n",
    insertAt(5, 1, List(1,2,3,4)))
}
