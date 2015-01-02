import org.scalatest._

import miscellaneous._

class MiscellaneousSpec extends FlatSpec with Matchers {
  "P90" should "place 8 queens" in {
    val solutions = (new S99M()).eightQueens
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
  "P91" should "find the knight's tour" in {
    val solution = (new S99M()).knightsTour
    solution.size should be (64)
    solution.toSet.size should be (64)
  }
  "P97" should "solve Sudoku" in {
    val solution = (new S99M()).solveSudoku(List(
      List(0, 0, 4, 8, 0, 0, 0, 1, 7),
      List(6, 7, 0, 9, 0, 0, 0, 0, 0),
      List(5, 0, 8, 0, 3, 0, 0, 0, 4),
      List(3, 0, 0, 7, 4, 0, 1, 0, 0),
      List(0, 6, 9, 0, 0, 0, 7, 8, 0),
      List(0, 0, 1, 0, 6, 9, 0, 0, 5),
      List(1, 0, 0, 0, 8, 0, 3, 0, 6),
      List(0, 0, 0, 0, 0, 6, 0, 9, 1),
      List(2, 4, 0, 0, 0, 1, 5, 0, 0)
    ))
    solution.flatten.toSet.size should be (9)
  }
}
