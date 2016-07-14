package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {

  trait TestTrees {
    val t1 = Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5)
    val t2 = Fork(Fork(Leaf('a', 2), Leaf('b', 3), List('a', 'b'), 5), Leaf('d', 4), List('a', 'b', 'd'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a', 'b', 'd'))
    }
  }

  test("times simple list") {
    assert(times(List('a', 'b', 'a')).sortBy(c => c._1) === List(('a', 2), ('b', 1)))
  }

  test("times list of the same chars") {
    assert(times(List('a', 'a', 'a')) === List(('a', 3)))
  }

  test("times list of all different chars") {
    assert(times(List('b', 'c', 'd')).sortBy(c => c._1) === List(('b', 1), ('c', 1), ('d', 1)))
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 3)))
  }

  test("singleton returns true for a single node tree") {
    assert(singleton(List(Leaf('c', 1))))
  }

  test("singleton returns false for a multi node tree") {
    new TestTrees() {
      assert(!singleton(List(t1, t2)))
    }
  }

  test("singleton returns false for an empty list") {
    assert(!singleton(List()))
  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  //
//  test("until of some leaf list") {
//    val leafs = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
//    assert(until(singleton, combine)(leafs) ===
//      List(Fork(Leaf('x', 4), Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3),List('x', 'e', 't'), 7)))
//  }
//
//  test("createCodeTree of some non empty text") {
//    val text = "xxtetxx"
//    assert(createCodeTree(text.toList) ==
//      Fork(Leaf('x', 4), Fork(Leaf('e', 1), Leaf('t', 2), List('e', 't'), 3),List('x', 'e', 't'), 7))
//  }


  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode and quickEncode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }

}
