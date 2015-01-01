import scala.annotation.tailrec
import scala.util.control.Breaks._

package miscellaneous {
  class S99M {
    // P90 (**) Eight queens problem
    // This is a classical problem in computer science. The objective is to place eight queens on a chessboard so that no two queens are attacking each other; i.e., no two queens are in the same row, the same column, or on the same diagonal.
    // Hint: Represent the positions of the queens as a list of numbers 1..N. Example: List(4, 2, 7, 3, 6, 8, 5, 1) means that the queen in the first column is in row 4, the queen in the second column is in row 2, etc. Use the generate-and-test paradigm.
    def place : List[List[Int]] = {
      def reverse(l: List[Any]) : List[Any] = {
        @tailrec def loop(acc: List[Any], lst: List[Any]) : List[Any] = lst match {
          case Nil => acc
          case _   => loop(lst.head :: acc, lst.tail)
        }
        loop(Nil, l)
      }
      def mapToBoardIndices(l: List[Int]) : List[Int] = {
        @tailrec def loop(acc: List[Int], lst: List[Int]) : List[Int] = {
          if (lst.size == 0) acc
          else loop(8 * acc.size + lst.head :: acc, lst.tail)
        }
        reverse(loop(Nil, l)).asInstanceOf[List[Int]]
      }
      def checkLists(l: List[Int], groups: List[List[Int]]) : Boolean = {
        groups.filter{lst => mapToBoardIndices(l).toSet.intersect(lst.toSet).size > 1}.size == 0
      }
      //  0  1  2  3  4  5  6  7
      //  8  9 10 11 12 13 14 15
      // 16 17 18 19 20 21 22 23
      // 24 25 26 27 28 29 30 31
      // 32 33 34 35 36 37 38 39
      // 40 41 42 43 44 45 46 47
      // 48 49 50 51 52 53 54 55
      // 56 57 58 59 60 61 62 63
      def validCols(l: List[Int]) : Boolean = {
        checkLists(l, List(
          List( 0,  8, 16, 24, 32, 40, 48, 56),  List( 1,  9, 17, 25, 33, 41, 49, 57), 
          List( 2, 10, 18, 26, 34, 42, 50, 58),  List( 3, 11, 19, 27, 35, 43, 51, 59), 
          List( 4, 12, 20, 28, 36, 44, 52, 60),  List( 5, 13, 21, 29, 37, 45, 54, 62), 
          List( 6, 14, 22, 30, 38, 46, 54, 62),  List( 7, 15, 23, 31, 39, 47, 55, 63)
        ))
      }
      def validRows(l: List[Int]) : Boolean = {
        checkLists(l, List(
          List( 0,  1,  2,  3,  4,  5,  6,  7),  List( 8,  9, 10, 11, 12, 13, 14, 15), 
          List(16, 17, 18, 19, 20, 21, 22, 23),  List(24, 25, 26, 27, 28, 29, 30, 31), 
          List(32, 33, 34, 35, 36, 37, 38, 39),  List(40, 41, 42, 43, 44, 45, 46, 47), 
          List(48, 49, 50, 51, 52, 53, 54, 55),  List(56, 57, 58, 59, 60, 61, 62, 63)
        ))
      }
      def validDiagonals(l: List[Int]) : Boolean = {
        checkLists(l, List(
          List( 0,  9, 18, 27, 36, 45, 54, 63), List( 1, 10, 19, 28, 37, 46, 56),
          List( 2, 11, 20, 29, 38, 47, 57),     List( 3, 12, 21, 30, 39),
          List( 4, 13, 22, 31),                 List( 5, 14, 23),
          List( 6, 15),
          List( 8, 17, 26, 35, 44, 53, 62),     List(16, 25, 34, 43, 52, 61),
          List(24, 33, 42, 51, 60),             List(32, 41, 50, 59),
          List(40, 49, 58),                     List(48, 57)
        )) && checkLists(l, List(
          List( 7, 14, 21, 28, 35, 42, 49, 56), List( 6, 13, 20, 27, 34, 41, 48),
          List( 5, 12, 19, 26, 33, 40),         List( 4, 11, 18, 25, 32),
          List( 3, 10, 17, 24),                 List( 2,  9, 16),
          List( 1,  8),
          List(15, 22, 29, 36, 43, 50, 57),     List(23, 30, 37, 44, 51, 58),
          List(31, 38, 45, 52, 59),             List(39, 46, 53, 60),
          List(47, 54, 61),                     List(55, 62)
        ))
      }
      def validBoard(l: List[Int]) : Boolean = {
        validCols(l) && validRows(l) && validDiagonals(l)
      }
      List(0, 1, 2, 3, 4, 5, 6, 7).permutations.toList.filter{validBoard(_)}
    }
  }
}
