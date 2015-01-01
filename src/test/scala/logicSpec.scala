import org.scalatest._

import logic._

class LogicSpec extends FlatSpec with Matchers {
  "P46" should "find and" in {
    (new S99Logic()).and(true, true) should be (true)
    (new S99Logic()).and(true, false) should be (false)
    (new S99Logic()).and(false, true) should be (false)
    (new S99Logic()).and(false, false) should be (false)
  }
  "P46" should "find or" in {
    (new S99Logic()).or(true, true) should be (true)
    (new S99Logic()).or(true, false) should be (true)
    (new S99Logic()).or(false, true) should be (true)
    (new S99Logic()).or(false, false) should be (false)
  }
  "P46" should "find not" in {
    (new S99Logic()).not(true) should be (false)
    (new S99Logic()).not(false) should be (true)
  }
  "P46" should "find equ" in {
    (new S99Logic()).equ(true, true) should be (true)
    (new S99Logic()).equ(true, false) should be (false)
    (new S99Logic()).equ(false, true) should be (false)
    (new S99Logic()).equ(false, false) should be (true)
  }
  "P46" should "find xor" in {
    (new S99Logic()).xor(true, true) should be (false)
    (new S99Logic()).xor(true, false) should be (true)
    (new S99Logic()).xor(false, true) should be (true)
    (new S99Logic()).xor(false, false) should be (false)
  }
  "P46" should "find nor" in {
    (new S99Logic()).nor(true, true) should be (false)
    (new S99Logic()).nor(true, false) should be (false)
    (new S99Logic()).nor(false, true) should be (false)
    (new S99Logic()).nor(false, false) should be (true)
  }
  "P46" should "find nand" in {
    (new S99Logic()).nand(true, true) should be (false)
    (new S99Logic()).nand(true, false) should be (true)
    (new S99Logic()).nand(false, true) should be (true)
    (new S99Logic()).nand(false, false) should be (true)
  }
  "P46" should "find impl" in {
    (new S99Logic()).impl(false, true) should be (true)
    (new S99Logic()).impl(true, false) should be (false)
    (new S99Logic()).impl(false, true) should be (true)
    (new S99Logic()).impl(false, false) should be (true)
  }
  "P49" should "find gray" in {
    (new S99Logic()).gray(3) should be (List("000", "001", "011", "010", "110", "111", "101", "100"))
  }
}
