import org.scalatest._

import miscellaneous._

class MiscellaneousSpec extends FlatSpec with Matchers {
  "P90" should "place 8 queens" in {
    val solutions = (new S99M()).place
    solutions.size should be (92)
    solutions.toSet.intersect(Set(List(0,4,7,5,2,6,1,3))).size should be (1)
    //println(solutions(0))
    //println(solutions.size)
    //List(0, 4, 7, 5, 2, 6, 1, 3)
    //   0 1 2 3 4 5 6 7
    // 0 x
    // 1             x
    // 2         x
    // 3               x
    // 4   x
    // 5       x
    // 6           x
    // 7    x
  }
}

