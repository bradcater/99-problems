import org.scalatest._

import miscellaneous._

class MiscellaneousSpec extends FlatSpec with Matchers {
  "P90" should "place 8 queens" in {
    //println((new S99M()).place(0))
    //List(0,3,6,2,5,1,7,4)
    //   0 1 2 3 4 5 6 7
    // 0 x
    // 1       x
    // 2             x
    // 3     x
    // 4           x
    // 5   x
    // 6               x
    // 7         x
    (new S99M()).place.toSet.intersect(List(List(0,3,6,2,5,1,7,4)).toSet).size should be (1)
  }
}

