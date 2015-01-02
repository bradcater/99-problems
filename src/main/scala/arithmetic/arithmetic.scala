import scala.annotation.tailrec

package arithmetic {
  final class S99Int(val start: Int) {
    import S99Int._
    // P31 (**) Determine whether a given integer number is prime.
    // scala> 7.isPrime
    // res0: Boolean = true
    def isPrime: Boolean =
      (start > 1) && (primes takeWhile { _ <= Math.sqrt(start) } forall { start % _ != 0 })
    // P32 (**) Determine the greatest common divisor of two positive integer numbers.
    // Use Euclid's algorithm.
    // scala> gcd(36, 63)
    // res0: Int = 9
    @tailrec def gcd(a: Int, b: Int) : Int = {
      if (a == b) {
        a
      } else if (a > b) {
        gcd(a - b, b)
      } else {
        gcd(a, b - a)
      }
    }
    // P33 (*) Determine whether two positive integer numbers are coprime.
    // Two numbers are coprime if their greatest common divisor equals 1.
    // scala> 35.isCoprimeTo(64)
    // res0: Boolean = true
    def isCoprimeTo(n: Int) : Boolean = {
      gcd(this, n) == 1
    }
    // P34 (**) Calculate Euler's totient function phi(m).
    // Euler's so-called totient function phi(m) is defined as the number of positive integers r (1 <= r <= m) that are coprime to m.
    // scala> 10.totient
    // res0: Int = 4
    def totient : Int = {
      (1 to this) filter { this.isCoprimeTo(_) } length
    }
    // P35 (**) Determine the prime factors of a given positive integer.
    // Construct a flat list containing the prime factors in ascending order.
    // scala> 315.primeFactors
    // res0: List[Int] = List(3, 3, 5, 7)
    def primeFactors : List[Int] = {
      def reverse[@specialized(Int) T](l: List[T]) : List[T] = {
        @tailrec def loop[@specialized(Int) T](acc: List[T], lst: List[T]) : List[T] = lst match {
          case Nil => acc
          case _   => loop(lst.head :: acc, lst.tail)
        }
        loop(Nil, l)
      }
      @tailrec def loop(acc: List[Int], n: Int, ps: Stream[Int]): List[Int] = {
        if (n.isPrime) n :: acc
        else if (n % ps.head == 0) loop(ps.head :: acc, n / ps.head, ps)
        else loop(acc, n, ps.tail)
      }
      reverse(loop(Nil, this, primes))
    }
    // P36 (**) Determine the prime factors of a given positive integer (2).
    // Construct a list containing the prime factors and their multiplicity.
    // scala> 315.primeFactorMultiplicity
    // res0: List[(Int, Int)] = List((3,2), (5,1), (7,1))
    // Alternately, use a Map for the result.
    // 
    // scala> 315.primeFactorMultiplicity
    // res0: Map[Int,Int] = Map(3 -> 2, 5 -> 1, 7 -> 1)
    def primeFactorMultiplicity : Map[Int, Int] = {
      @tailrec def loop(acc: Map[Int,Int], r: List[Int]) : Map[Int,Int] = {
        if (r.size == 0) {
          acc
        } else {
          loop(acc + (r.head -> (acc.getOrElse(r.head, 0) + 1)), r.tail)
        }
      }
      loop(Map.empty[Int,Int], this.primeFactors)
    }
    // P37 (**) Calculate Euler's totient function phi(m) (improved).
    // See problem P34 for the definition of Euler's totient function. If the list of the prime factors of a number m is known in the form of problem P36 then the function phi(m>) can be efficiently calculated as follows: Let [[p1, m1], [p2, m2], [p3, m3], ...] be the list of prime factors (and their multiplicities) of a given number m. Then phi(m) can be calculated with the following formula:
    // phi(m) = (p1-1)*p1(m1-1) * (p2-1)*p2(m2-1) * (p3-1)*p3(m3-1) * ...
    // 
    // Note that ab stands for the bth power of a.
    def prod(l: List[Double]) : Double = {
      @tailrec def loop(p: Double, lst: List[Double]) : Double = {
        if (lst.size == 0) p
        else loop(p * lst.head, lst.tail)
      }
      loop(1, l)
    }
    def phi : Double = {
      prod(this.primeFactorMultiplicity.keys.toList.map{k => (k - 1) * math.pow(k, this.primeFactorMultiplicity(k) - 1)})
    }
    // P38 (*) Compare the two methods of calculating Euler's totient function.
    // Use the solutions of problems P34 and P37 to compare the algorithms. Try to calculate phi(10090) as an example.
    // P39 (*) A list of prime numbers.
    // Given a range of integers by its lower and upper limit, construct a list of all prime numbers in that range.
    // scala> listPrimesinRange(7 to 31)
    // res0: List[Int] = List(7, 11, 13, 17, 19, 23, 29, 31)
    def listPrimesinRange(r: Range) : List[Int] = {
      primes dropWhile { _ < r.toList(0)} takeWhile { _ <= r.toList.last } toList
    }
    // P40 (**) Goldbach's conjecture.
    // Goldbach's conjecture says that every positive even number greater than 2 is the sum of two prime numbers. E.g. 28 = 5 + 23. It is one of the most famous facts in number theory that has not been proved to be correct in the general case. It has been numerically confirmed up to very large numbers (much larger than Scala's Int can represent). Write a function to find the two prime numbers that sum up to a given even integer.
    // scala> 28.goldbach
    // res0: (Int, Int) = (5,23)
    def goldbach : (Int, Int) = {
      @tailrec def loop(ps: Stream[Int]) : (Int, Int) = {
        if ((this - ps.head).isPrime) (ps.head, this - ps.head)
        else loop(ps.tail)
      }
      loop(primes)
    }
    // P41 (**) A list of Goldbach compositions.
    // Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.
    // scala> printGoldbachList(9 to 20)
    // 10 = 3 + 7
    // 12 = 5 + 7
    // 14 = 3 + 11
    // 16 = 3 + 13
    // 18 = 5 + 13
    // 20 = 3 + 17
    def printGoldbachList(r: Range) : Unit = {
      @tailrec def loop(l: List[Int]) : Unit = {
        if (l.size == 0) {
          ()
        } else {
          printf("%d = %d + %d\n",
            l.head, l.head.goldbach._1, l.head.goldbach._2)
          loop(l.tail)
        }
      }
      loop(r filter (_ % 2 == 0) toList)
    }
    // In most cases, if an even number is written as the sum of two prime numbers, one of them is very small. Very rarely, the primes are both bigger than, say, 50. Try to find out how many such cases there are in the range 2..3000.
    // 
    // Example (minimum value of 50 for the primes):
    // 
    // scala> printGoldbachListLimited(1 to 2000, 50)
    // 992 = 73 + 919
    // 1382 = 61 + 1321
    // 1856 = 67 + 1789
    // 1928 = 61 + 1867
    def printGoldbachListLimited(r: Range, n: Int) : Unit = {
      @tailrec def loop(l: List[Int]) : Unit = {
        if (l.size == 0) {
          ()
        } else {
          if (l.head.goldbach._1 >= n) {
            printf("%d = %d + %d\n",
              l.head, l.head.goldbach._1, l.head.goldbach._2)
          }
          loop(l.tail)
        }
      }
      loop(r filter (_ % 2 == 0) toList)
    }
  }
  
  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
    implicit def S99Int2Int(i: S99Int): Int = i.start
    val primes = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime })
  }
}
