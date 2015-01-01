package arithmetic {
  class S99Int(val start: Int) {
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
    def gcd(a: Int, b: Int) : Int = {
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
  }
  
  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
    implicit def S99Int2Int(i: S99Int): Int = i.start
    val primes = Stream.cons(2, Stream.from(3, 2) filter { _.isPrime })
  }
}
