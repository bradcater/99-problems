package logic {
  object S99Logic {
  }
  class S99Logic {
    // P46 (**) Truth tables for logical expressions.
    // Define functions and, or, nand, nor, xor, impl, and equ (for logical equivalence) which return true or false according to the result of their respective operations; e.g. and(A, B) is true if and only if both A and B are true.
    // scala> and(true, true)
    // res0: Boolean = true
    // 
    // scala> xor(true. true)
    // res1: Boolean = false
    def and(a: Boolean, b: Boolean) : Boolean = (a, b) match {
      case (true, true) => true
      case _            => false
    }
    def or(a: Boolean, b: Boolean) : Boolean = (a, b) match {
      case (false, false) => false
      case _              => true
    }
    def not(a: Boolean) : Boolean = a match {
      case true  => false
      case false => true
    }
    def equ(a: Boolean, b: Boolean): Boolean = or(and(a, b), and(not(a), not(b)))
    def xor(a: Boolean, b: Boolean): Boolean = not(equ(a, b))
    def nor(a: Boolean, b: Boolean): Boolean = not(or(a, b))
    def nand(a: Boolean, b: Boolean): Boolean = not(and(a, b))
    def impl(a: Boolean, b: Boolean): Boolean = or(not(a), b)
    // A logical expression in two variables can then be written as an function of two variables, e.g: (a: Boolean, b: Boolean) => and(or(a, b), nand(a, b))
    // 
    // Now, write a function called table2 which prints the truth table of a given logical expression in two variables.
    // 
    // scala> table2((a: Boolean, b: Boolean) => and(a, or(a, b)))
    // A     B     result
    // true  true  true
    // true  false true
    // false true  false
    // false false false
    def table2(f: (Boolean, Boolean) => Boolean) {
      println("A     B     result")
      for {a <- List(true, false);
           b <- List(true, false)} {
        printf("%-5s %-5s %-5s\n", a, b, f(a, b))
      }
    }
    // P47 (*) Truth tables for logical expressions (2).
    // Continue problem P46 by redefining and, or, etc as operators. (i.e. make them methods of a new class with an implicit conversion from Boolean.) not will have to be left as a object method.
    // scala> table2((a: Boolean, b: Boolean) => a and (a or not(b)))
    // A     B     result
    // true  true  true
    // true  false true
    // false true  false
    // false false false
    // P48 (**) Truth tables for logical expressions (3).
    // Omitted for now.
    // P49 (**) Gray code.
    // An n-bit Gray code is a sequence of n-bit strings constructed according to certain rules. For example,
    // n = 1: C(1) = ("0", "1").
    // n = 2: C(2) = ("00", "01", "11", "10").
    // n = 3: C(3) = ("000", "001", "011", "010", "110", "111", "101", "100").
    // Find out the construction rules and write a function to generate Gray codes.
    // 
    // scala> gray(3)
    // res0 List[String] = List(000, 001, 011, 010, 110, 111, 101, 100)
    // See if you can use memoization to make the function more efficient.
    // 2-bit list: 00, 01, 11, 10   
    // Reflected:    10, 11, 01, 00
    // Prefix old entries with 0:  000, 001, 011, 010,  
    // Prefix new entries with 1:    110, 111, 101, 100
    // Concatenated: 000, 001, 011, 010, 110, 111, 101, 100
    def gray(b: Int) : List[String] = {
      def reverse(l: List[String]) : List[String] = {
        def loop(acc: List[String], lst: List[String]) : List[String] = {
          if (lst.size == 0) acc
          else loop(lst.head :: acc, lst.tail)
        }
        loop(Nil, l)
      }
      def prefixWith(p: String, l: List[String]) : List[String] = {
        l.map{p + _}
      }
      def loop(bb: Int, l: List[String]) : List[String] = {
        if (bb == b) {
          l
        } else {
          loop(bb + 1, prefixWith("0", l) ::: prefixWith("1", reverse(l)))
        }
      }
      loop(2, List("00", "01", "11", "10"))
    }
  }
}
