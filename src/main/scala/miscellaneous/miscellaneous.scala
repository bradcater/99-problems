import scala.annotation.tailrec
import scala.util.control.Breaks._

package miscellaneous {
  class S99M {
    // P90 (**) Eight queens problem
    // This is a classical problem in computer science. The objective is to place eight queens on a chessboard so that no two queens are attacking each other; i.e., no two queens are in the same row, the same column, or on the same diagonal.
    // Hint: Represent the positions of the queens as a list of numbers 1..N. Example: List(4, 2, 7, 3, 6, 8, 5, 1) means that the queen in the first column is in row 4, the queen in the second column is in row 2, etc. Use the generate-and-test paradigm.
    def place : List[List[Int]] = {
      def mapToBoardIndices(l: List[Int]) : Set[Int] = {
        Set(
          l(0),      l(1) + 8,
          l(2) + 16, l(3) + 24,
          l(4) + 32, l(5) + 40,
          l(6) + 48, l(7) + 56
        )
      }
      def noOverlapExists(s: Set[Int], groups: List[Set[Int]]) : Boolean = {
        !groups.exists{st => s.intersect(st).size > 1}
      }
      //  0  1  2  3  4  5  6  7
      //  8  9 10 11 12 13 14 15
      // 16 17 18 19 20 21 22 23
      // 24 25 26 27 28 29 30 31
      // 32 33 34 35 36 37 38 39
      // 40 41 42 43 44 45 46 47
      // 48 49 50 51 52 53 54 55
      // 56 57 58 59 60 61 62 63
      val colSets = List(
        Set( 0,  8, 16, 24, 32, 40, 48, 56),  Set( 1,  9, 17, 25, 33, 41, 49, 57), 
        Set( 2, 10, 18, 26, 34, 42, 50, 58),  Set( 3, 11, 19, 27, 35, 43, 51, 59), 
        Set( 4, 12, 20, 28, 36, 44, 52, 60),  Set( 5, 13, 21, 29, 37, 45, 53, 61), 
        Set( 6, 14, 22, 30, 38, 46, 54, 62),  Set( 7, 15, 23, 31, 39, 47, 55, 63)
      )
      def validCols(s: Set[Int]) : Boolean = {
        noOverlapExists(s, colSets)
      }
      val rowSets = List(
        Set( 0,  1,  2,  3,  4,  5,  6,  7),  Set( 8,  9, 10, 11, 12, 13, 14, 15), 
        Set(16, 17, 18, 19, 20, 21, 22, 23),  Set(24, 25, 26, 27, 28, 29, 30, 31), 
        Set(32, 33, 34, 35, 36, 37, 38, 39),  Set(40, 41, 42, 43, 44, 45, 46, 47), 
        Set(48, 49, 50, 51, 52, 53, 54, 55),  Set(56, 57, 58, 59, 60, 61, 62, 63)
      )
      def validRows(s: Set[Int]) : Boolean = {
        noOverlapExists(s, rowSets)
      }
      val rightDiagonalSets = List(
        Set( 0,  9, 18, 27, 36, 45, 54, 63), Set( 1, 10, 19, 28, 37, 46, 55),
        Set( 2, 11, 20, 29, 38, 47),         Set( 3, 12, 21, 30, 39),
        Set( 4, 13, 22, 31),                 Set( 5, 14, 23),
        Set( 6, 15),
        Set( 8, 17, 26, 35, 44, 53, 62),     Set(16, 25, 34, 43, 52, 61),
        Set(24, 33, 42, 51, 60),             Set(32, 41, 50, 59),
        Set(40, 49, 58),                     Set(48, 57)
      )
      val leftDiagonalSets = List(
        Set( 7, 14, 21, 28, 35, 42, 49, 56), Set( 6, 13, 20, 27, 34, 41, 48),
        Set( 5, 12, 19, 26, 33, 40),         Set( 4, 11, 18, 25, 32),
        Set( 3, 10, 17, 24),                 Set( 2,  9, 16),
        Set( 1,  8),
        Set(15, 22, 29, 36, 43, 50, 57),     Set(23, 30, 37, 44, 51, 58),
        Set(31, 38, 45, 52, 59),             Set(39, 46, 53, 60),
        Set(47, 54, 61),                     Set(55, 62)
      )
      def validDiagonals(s: Set[Int]) : Boolean = {
        noOverlapExists(s, rightDiagonalSets) && noOverlapExists(s, leftDiagonalSets)
      }
      def validBoard(l: List[Int]) : Boolean = {
        val s = mapToBoardIndices(l)
        validCols(s) && validRows(s) && validDiagonals(s)
      }
      // The entry in the ith position is the row index of the queen in the ith
      // column.
      List(0, 1, 2, 3, 4, 5, 6, 7).permutations.toList.filter{validBoard(_)}
    }
  }
}
