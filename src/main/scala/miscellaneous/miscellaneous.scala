import scala.annotation.tailrec
import scala.collection.mutable.LinkedHashSet
import scala.collection.mutable.Map

package miscellaneous {
  class S99M {
    def prettyPrintMatrix[T](mm: List[List[T]]) : Unit = {
      mm.foreach{r => println(r)}
    }
    // P90 (**) Eight queens problem
    // This is a classical problem in computer science. The objective is to place eight queens on a chessboard so that no two queens are attacking each other; i.e., no two queens are in the same row, the same column, or on the same diagonal.
    // Hint: Represent the positions of the queens as a list of numbers 1..N. Example: List(4, 2, 7, 3, 6, 8, 5, 1) means that the queen in the first column is in row 4, the queen in the second column is in row 2, etc. Use the generate-and-test paradigm.
    def eightQueens : List[List[Int]] = {
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
    // P91 (**) Knight's tour.
    // Another famous problem is this one: How can a knight jump on an NÃ—N chessboard in such a way that it visits every square exactly once?
    // Hints: Represent the squares by pairs of their coordinates of the form (X, Y), where both X and Y are integers between 1 and N. (Alternately, define a Point class for the same purpose.) Write a function jumps(N, (X, Y)) to list the squares that a knight can jump to from (X, Y) on a NÃ—N chessboard. And finally, represent the solution of our problem as a list of knight positions (the knight's tour).
    // 
    // It might be nice to find more than one tour, but a computer will take a long time trying to find them all at once. Can you make a lazy list that only calculates the tours as needed?
    // 
    // Can you find only "closed tours", where the knight can jump from its final position back to its starting position?
    def knightsTour : List[(Int, Int)] = {
      def jumps(p: (Int, Int)) : List[(Int, Int)] = {
        var l = List[(Int, Int)]()
        if (p._1 > 0) {
          // x -
          //   -
          //   (p._1,p._2)
          if (p._2 > 1) {
            l = (p._1 - 1, p._2 - 2) :: l
          }
          //   (p._1, p._2)
          //   -
          // x -
          if (p._2 < 6) {
            l = (p._1 - 1, p._2 + 2) :: l
          }
          if (p._1 > 1) {
            // x - -
            //     (p._1, p._2)
            if (p._2 > 0) {
              l = (p._1 - 2, p._2 - 1) :: l
            }
            //     (p._1, p._2)
            // x - -
            if (p._2 < 7) {
              l = (p._1 - 2, p._2 + 1) :: l
            }
          }
        }
        if (p._1 < 7) {
          // - x
          // -
          // (p._1, p._2)
          if (p._2 > 1) {
            l = (p._1 + 1, p._2 - 2) :: l
          }
          // (p._1, p._2)
          // -
          // - x
          if (p._2 < 6) {
            l = (p._1 + 1, p._2 + 2) :: l
          }
          if (p._1 < 6) {
            // - - x
            // (p._1, p._2)
            if (p._2 > 0) {
              l = (p._1 + 2, p._2 - 1) :: l
            }
            // (p._1, p._2)
            // - - x
            if (p._2 < 7) {
              l = (p._1 + 2, p._2 + 1) :: l
            }
          }
        }
        l
      }
      def dfs(start: (Int, Int), visited: LinkedHashSet[(Int, Int)], remVisitedCount: Int) : List[(Int, Int)] = {
        if (remVisitedCount == 0) {
          return (visited + start).toList
        }
        // This sortWith is a heuristic that says to visit nodes in order of
        // ascending degree. Intuitively, this is visiting the less accessible
        // nodes before the more accessible nodes.
        val js = jumps(start).sortWith{(a,b) => jumps(a).size < jumps(b).size}
        for (i <- (0 to js.size - 1)) {
          if (!visited.contains(js(i))) {
            val np = dfs(js(i), visited + start, remVisitedCount - 1)
            if (np.size > 0) return np
          }
        }
        List[(Int, Int)]()
      }
      dfs((0,0), LinkedHashSet(), 63)
    }
    def setMatrixValue[T](mm: List[List[T]], r: Int, c: Int, v: T) : List[List[T]] = {
      setValue(mm, r, setValue(mm(r), c, v))
    }
    def reverse[T](lst: List[T]) : List[T] = {
      @tailrec def loop(acc: List[T], lstt: List[T]) : List[T] = lstt match {
        case Nil => acc
        case _   => loop(lstt.head :: acc, lstt.tail)
      }
      loop(Nil, lst)
    }
    def setValue[T](l: List[T], idx: Int, v: T) : List[T] = {
      @tailrec def loop(acc: List[T], lst: List[T], curIdx: Int) : List[T] = {
        if (lst.isEmpty) {
          acc
        } else if (curIdx == idx) {
          loop(v :: acc, lst.tail, curIdx + 1)
        } else {
          loop(lst.head :: acc, lst.tail, curIdx + 1)
        }
      }
      reverse(loop(Nil, l, 0))
    }
    def setMultiValue[T](l: List[T], idx: Int, v: List[T]) : List[T] = {
      @tailrec def loop(acc: List[T], lst: List[T], v_rem: List[T], curIdx: Int) : List[T] = {
        if (lst.isEmpty) {
          acc
        } else if (curIdx == idx && v_rem.size > 0) {
          loop(v_rem.head :: acc, lst.tail, v_rem.tail, curIdx)
        } else {
          loop(lst.head :: acc, lst.tail, v_rem, curIdx + 1)
        }
      }
      reverse(loop(Nil, l, v, 0))
    }
    // P97 (**) Sudoku. (alternate solution)
    // Sudoku puzzles go like this:
    //    Problem statement                 Solution
    // 
    //     .  .  4 | 8  .  . | .  1  7      9  3  4 | 8  2  5 | 6  1  7
    //             |         |                      |         |
    //     6  7  . | 9  .  . | .  .  .      6  7  2 | 9  1  4 | 8  5  3
    //             |         |                      |         |
    //     5  .  8 | .  3  . | .  .  4      5  1  8 | 6  3  7 | 9  2  4
    //     --------+---------+--------      --------+---------+--------
    //     3  .  . | 7  4  . | 1  .  .      3  2  5 | 7  4  8 | 1  6  9
    //             |         |                      |         |
    //     .  6  9 | .  .  . | 7  8  .      4  6  9 | 1  5  3 | 7  8  2
    //             |         |                      |         |
    //     .  .  1 | .  6  9 | .  .  5      7  8  1 | 2  6  9 | 4  3  5
    //     --------+---------+--------      --------+---------+--------
    //     1  .  . | .  8  . | 3  .  6      1  9  7 | 5  8  2 | 3  4  6
    //             |         |                      |         |
    //     .  .  . | .  .  6 | .  9  1      8  5  3 | 4  7  6 | 2  9  1
    //             |         |                      |         |
    //     2  4  . | .  .  1 | 5  .  .      2  4  6 | 3  9  1 | 5  7  8
    // Every spot in the puzzle belongs to a (horizontal) row and a (vertical) column, as well as to one single 3Ã—3 square (which we call "square" for short). At the beginning, some of the spots carry a single-digit number between 1 and 9. The problem is to fill the missing spots with digits in such a way that every number between 1 and 9 appears exactly once in each row, in each column, and in each square.
    def solveSudoku(m: List[List[Int]]) : List[List[Int]] = {
      def getColumn(mm: List[List[Int]], c: Int) : Set[Int] = {
        mm.map{_(c)}.filter{_ > 0}.toSet
      }
      def getRow(mm: List[List[Int]], r: Int) : Set[Int] = {
        mm(r).filter{_ > 0}.toSet
      }
      // Label the squares as
      // 0 | 1 | 2
      // 3 | 4 | 5
      // 6 | 7 | 8
      val ninthIndices = Map.empty[Int, List[(Int, Int)]] + (0 -> List(
          (0,0),(0,1),(0,2),
          (1,0),(1,1),(1,2),
          (2,0),(2,1),(2,2)
        )) + (1 -> List(
          (0,3),(0,4),(0,5),
          (1,3),(1,4),(1,5),
          (2,3),(2,4),(2,5)
        )) + (2 -> List(
          (0,6),(0,7),(0,8),
          (1,6),(1,7),(1,8),
          (2,6),(2,7),(2,8)
        )) + (3 -> List(
          (3,0),(3,1),(3,2),
          (4,0),(4,1),(4,2),
          (5,0),(5,1),(5,2)
        )) + (4 -> List(
          (3,3),(3,4),(3,5),
          (4,3),(4,4),(4,5),
          (5,3),(5,4),(5,5)
        )) + (5 -> List(
          (3,6),(3,7),(3,8),
          (4,6),(4,7),(4,8),
          (5,6),(5,7),(5,8)
        )) + (6 -> List(
          (6,0),(6,1),(6,2),
          (7,0),(7,1),(7,2),
          (8,0),(8,1),(8,2)
        )) + (7 -> List(
          (6,3),(6,4),(6,5),
          (7,3),(7,4),(7,5),
          (8,3),(8,4),(8,5)
        )) + (8 -> List(
          (6,6),(6,7),(6,8),
          (7,6),(7,7),(7,8),
          (8,6),(8,7),(8,8)
        ))
      def getNinth(mm: List[List[Int]], s: Int) : Set[Int] = {
        ninthIndices(s).map{p => mm(p._1)(p._2)}.toSet
      }
      def getSquare(mm: List[List[Int]], r: Int, c: Int) : Set[Int] = {
        if (r < 6) {
          if (r < 3) {
            if (c < 6) {
              getNinth(mm, if(c < 3) 0 else 1)
            } else {
              getNinth(mm, 2)
            }
          } else {
            if (c < 6) {
              getNinth(mm, if(c < 3) 3 else 4)
            } else {
              getNinth(mm, 5)
            }
          }
        } else if (c < 6) {
          getNinth(mm, if(c < 3) 6 else 7)
        } else {
          getNinth(mm, 8)
        }
      }
      def solved(mm: List[List[Int]]) : Boolean = {
        !mm.exists{r => r.exists{_ == 0}}
      }
      val goodNumbers = Set(0,1,2,3,4,5,6,7,8,9)
      def solve(mm: List[List[Int]]) : List[List[Int]] = {
        if (solved(mm)) return mm
        for (r <- (0 to 8)) {
          for (c <- (0 to 8)) {
            if (mm(r)(c) == 0) {
              val availableNumbers = goodNumbers &~ (getRow(mm, r) | getColumn(mm, c) | getSquare(mm, r, c))
              if (availableNumbers.size == 1) {
                return solve(setMatrixValue(mm, r, c, availableNumbers.toList.head))
              }
            }
          }
        }
        prettyPrintMatrix(mm)
        sys.error("Why can't this be solved?")
      }
      solve(m)
    }
    // P99 (***) Crossword puzzle.
    // Given an empty (or almost empty) framework of a crossword puzzle and a set of words. The problem is to place the words into the framework.
    // The particular crossword puzzle is specified in a text file which first lists the words (one word per line) in an arbitrary order. Then, after an empty line, the crossword framework is defined. In this framework specification, an empty character location is represented by a dot (.). In order to make the solution easier, character locations can also contain predefined character values. The puzzle opposite is defined in the file p99a.dat, other examples are p99b.dat and p99d.dat. There is also an example of a puzzle (p99c.dat) which does not have a solution.
    // 
    // Words are strings of at least two characters. A horizontal or vertical sequence of character places in the crossword puzzle framework is called a site. Our problem is to find a compatible way of placing words onto sites.
    // 
    // Hints: (1) The problem is not easy. You will need some time to thoroughly understand it. So, don't give up too early! And remember that the objective is a clean solution, not just a quick-and-dirty hack!
    // 
    // (2) For efficiency reasons it is important, at least for larger puzzles, to sort the words and the sites in a particular order. For this part of the problem, the solution of P28 may be very helpful.
    // LINUX
    // PROLOG
    // PERL
    // ONLINE
    // GNU
    // XML
    // NFS
    // SQL
    // EMACS
    // WEB
    // MAC
    // 
    // ......  .
    // . .  .  .
    // . ..... .
    // . . . ...
    //   . ... .
    //  ...
    //
    // PROLOG  E
    // E N  N  M
    // R LINUX A
    // L I F MAC
    //   N SQL S
    //  WEB
    // acrossStarts
    //   expected: (0,0), (2,2), (3,6), (4,4), (5,1)
    //   received: List((0,0,6), (2,2,5), (3,6,3), (4,4,3), (5,1,3))
    // downStarts
    //   expected: (0,0), (0,2), (0,5), (0,8), (2,4), (2,6)
    //   received: List((0,0,4), (0,2,6), (2,4,3), (0,5,3), (2,6,3), (0,8,5))
    def solveCrossword(words: List[String], puzzle: List[List[String]]) : List[List[String]] = {
      def incMapKey[T](m: Map[T, Int], k: T) : Map[T, Int] = {
        if (m contains k) m - k + (k -> (m(k) + 1))
        else m + (k -> 1)
      }
      def charFrequencies(ws: List[String]) = {
        @tailrec def loop(m: Map[Char, Int], cur_w: String, r_ws: List[String]) : Map[Char, Int] = {
          if (empty(r_ws)) m
          else if (cur_w.isEmpty) loop(m, r_ws.head, r_ws.tail)
          else loop(incMapKey(m, cur_w.head), cur_w.tail, r_ws)
        }
        loop(Map.empty[Char, Int], ws.head, ws.tail)
      }
      def wordCharFrequencyScore(w: String, m: Map[Char, Int]) : Int = {
        w.map{m(_)}.foldLeft(0){_+_}
      }
      def empty[T](l: List[T]) : Boolean = l match {
        case h :: r => false
        case _      => true
      }
      def getRunLength(row: List[String], c: Int) : (Int, Int) = {
        @tailrec def loop(cur_c: Int) : (Int, Int) = {
          if (cur_c >= row.size || row(cur_c) != ".") (c, cur_c - c)
          else loop(cur_c + 1)
        }
        loop(c)
      }
      def rowOrColumnStarts(row: List[String]) : List[(Int, Int)] = {
        (0 to row.size - 1).toList.filter{
          c => row(c) == "." && (c == 0 || (c > 0 && (c == row.size - 1 || row(c + 1) == ".") && row(c - 1) == " "))
        }.map{
          c => getRunLength(row, c)
        }.filter{
          d => d._2 > 1
        }
      }
      // (row, column, length)
      def getAcrossStarts(p: List[List[String]]) : List[(Int, Int, Int)] = {
        (0 to p.size - 1).toList.map{
          r => rowOrColumnStarts(p(r)).map{d => (r, d._1, d._2)}
        }.flatten
      }
      // (row, column, length)
      def getDownStarts(p: List[List[String]]) : List[(Int, Int, Int)] = {
        (0 to p(0).size - 1).toList.map{
          c => rowOrColumnStarts(p.map{r => r(c)}).map{d => (d._1, c, d._2)}
        }.flatten
      }
      // (isAcross, row, column, length) -> List((is_a, r, c, l), ...)
      def getIntersections(sites: List[(Boolean, Int, Int, Int)]) : Map[(Boolean, Int, Int, Int), List[(Boolean, Int, Int, Int)]] = {
        var m = collection.mutable.Map.empty[(Boolean, Int, Int, Int), List[(Boolean, Int, Int, Int)]]
        sites
          .filter{case (is_a, r, c, l) => is_a}
          .foreach{case (is_a, r, c, l) => m((is_a, r, c, l)) = sites
            .filter{case (is_a, r, c, l) => !is_a}
            .filter{case (c_is_a, c_r, c_c, l_c) => c <= c_c && c + l >= c_c && r >= c_r && r <= c_r + l_c}
          }
        sites
          .filter{case (is_a, r, c, l) => !is_a}
          .foreach{case (is_a, r, c, l) => m((is_a, r, c, l)) = sites
            .filter{case (is_a, r, c, l) => is_a}
            .filter{case (r_is_a, r_r, r_c, l_r) => r_c <= c && r_c + l_r >= c && r_r >= r && r_r <= r + l}
          }
        m
      }

      def wordsBySize(ws: List[String]) : Map[Int, List[String]] = {
        @tailrec def loop(m: Map[Int, List[String]], rem: List[String]) : Map[Int, List[String]] = {
          if (empty(rem)) m
          else if (m contains rem.head.size) loop(m + (rem.head.size -> (rem.head :: (m(rem.head.size)))), rem.tail)
          else loop(m + (rem.head.size -> List(rem.head)), rem.tail)
        }
        loop(Map.empty[Int, List[String]], words)
      }
      def wordFits(w: String, p: List[List[String]], r: Int, c: Int, isAcross: Boolean) : Boolean = {
        if (isAcross) {
          !(0 to w.size - 1).toList.exists{i => (p(r)(c + i) != ".") && (p(r)(c + i) != w.slice(i, i+1))}
        } else {
          !(0 to w.size - 1).toList.exists{j => (p(r + j)(c) != ".") && (p(r + j)(c) != w.slice(j, j+1))}
        }
      }
      // FIXME This could be a lot faster. Instead of repeated calls to
      // setMatrixValue, we should augment setMatrixValue to take a List of
      // values and a List of indices.
      def placeWord(w: String, p: List[List[String]], r: Int, c: Int, isAcross: Boolean) : List[List[String]] = {
        @tailrec def loop(w_rem: String, pz: List[List[String]], r_rem: Int, c_rem: Int) : List[List[String]] = w_rem match {
          case "" => pz
          case _  => loop(w_rem.tail, setMatrixValue(pz, r_rem, c_rem, w_rem.head + ""), r_rem + 1, c_rem)
        }
        isAcross match {
          case true  => setValue(p, r, setMultiValue(p(r), c, w.toList.map{chr => chr + ""}))
          case false => loop(w, p, r, c)
        }
      }
      def completedPuzzle(p: List[List[String]]) : Boolean = {
        !p.exists{r => r.exists{_ == "."}}
      }
      def remove[T](l: List[T], o: T) : List[T] = {
        l diff List(o)
      }
      def removeValue(m: Map[Int, List[String]], k: String) : Map[Int, List[String]] = {
        if (m(k.size).size == 1) m - k.size
        else m - k.size + (k.size -> remove(m(k.size), k))
      }
      def sortListsByCharFrequency(m: Map[Int, List[String]], charFs : Map[Char, Int]) : Map[Int, List[String]] = {
        @tailrec def loop(mm: Map[Int, List[String]], remKeys : List[Int]) : Map[Int, List[String]] = {
          if (empty(remKeys)) mm
          else loop(
            mm + (remKeys.head -> m(remKeys.head).sortWith{(a,b) => wordCharFrequencyScore(a, charFs) < wordCharFrequencyScore(b, charFs)}),
            remKeys.tail
          )
        }
        loop(Map.empty[Int, List[String]], m.keySet.toList)
      }
      // return (row, column)
      def collisionCell(a: (Boolean, Int, Int, Int), b: (Boolean, Int, Int, Int)) : (Int, Int) = {
        if (a._1) (a._2, b._3)
        else (b._2, a._3)
      }
      def solveOne(p: List[List[String]], starts: List[(Boolean, Int, Int, Int)], wbs: Map[Int, List[String]], ic: Map[(Boolean, Int, Int, Int), List[(Boolean, Int, Int, Int)]], charFs : Map[Char, Int]) : List[List[String]] = {
        solveTwo(
          p,
          starts.sortWith{(a,b) => if(ic(a).size == ic(b).size) a._4 < b._4 else ic(a).size < ic(b).size},
          sortListsByCharFrequency(wbs, charFs),
          ic,
          charFs
        )
      }
      def solveTwo(p: List[List[String]], starts: List[(Boolean, Int, Int, Int)], wbs: Map[Int, List[String]], ic: Map[(Boolean, Int, Int, Int), List[(Boolean, Int, Int, Int)]], charFs : Map[Char, Int]) : List[List[String]] = {
        //prettyPrintMatrix(p)
        if (wbs.isEmpty) {
          return p
        } else {
          for ((is_a, r, c, l) <- starts) {
            for (w <- wbs(l)) {
              //printf("working on %s (%d,%d)\n",
              //  w, r, c)
              if (wordFits(w, p, r, c, is_a)) {
                //printf("working on %s (%d,%d)\n",
                //  w, r, c)
                if (!ic.contains((is_a, r, c, w.size)) ||
                  ic((is_a, r, c, w.size)).exists{
                    case (ic_isAcross, ic_r, ic_c, ic_l) => 
                      // Either the collision cell is already filled in, or
                      // there exists at least 1 word that has the correct
                      // letter in the collision cell.
                      val cc = collisionCell((is_a, r, c, l), (ic_isAcross, ic_r, ic_c, ic_l))
                      //println(cc)
                      //println(p(cc._1)(cc._2))
                      //println(ic_l)
                      //println(remove(wbs.getOrElse(ic_l, List()), w).map{
                      //  ww => if (is_a) {
                      //    (is_a, ww, ww.slice(r - cc._1, r - cc._1 + 1), w, w.slice(cc._2 - c, cc._2 - c + 1))
                      //  } else {
                      //    (is_a, ww, ww.slice(cc._2 - c, cc._2 - c + 1), w, w.slice(r - cc._1, r - cc._1 + 1))
                      //  }
                      //})
                      val matchChar = if(is_a) w.slice(cc._2 - c, cc._2 - c + 1) else w.slice(r - cc._1, r - cc._1 + 1)
                      p(cc._1)(cc._2) != "." || remove(wbs.getOrElse(ic_l, List()), w).exists{
                        ww => if (is_a) {
                          ww.slice(r - cc._1, r - cc._1 + 1) == matchChar
                        } else {
                          ww.slice(cc._2 - c, cc._2 - c + 1) == matchChar
                        }
                      }
                }) {
                  //printf("word %s fits in (%d,%d) %s\n",
                  //  w, r, c, if(is_a) "across" else "down")
                  val sol = solveTwo(placeWord(w, p, r, c, is_a), remove(starts, (is_a, r, c, l)), removeValue(wbs, w), ic, charFs)
                  if (completedPuzzle(sol)) return sol
                }
              }
            }
          }
          p
        }
      }
      println(getAcrossStarts(puzzle))
      println(getDownStarts(puzzle))
      solveOne(
        puzzle,
        getAcrossStarts(puzzle).map{start => (true, start._1, start._2, start._3)} ::: getDownStarts(puzzle).map{start => (false, start._1, start._2, start._3)},
        wordsBySize(words),
        getIntersections(getAcrossStarts(puzzle).map{start => (true, start._1, start._2, start._3)} ::: getDownStarts(puzzle).map{start => (false, start._1, start._2, start._3)}),
        charFrequencies(words)
      )
    }
  }
}
