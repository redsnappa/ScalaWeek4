package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
  trait TestTrees {
    val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
    val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
  }

  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }

  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }

  test("testing times") {
    new TestTrees {
      assert(times(List('a','b','b','a')) === List(('b',2),('a',2)))
      assert(times(List()) === List())
      assert(times(List('a','a','b','a')) === List(('b',1),('a',3)))

    }
  }

  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("combine some leafs"){

    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 2))
    assert(combine(leaflist) === List(Leaf('x',2), Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3)))

  }

  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("combine one leaf list") {
    val leaflist = List(Leaf('e', 1))
    assert(combine(leaflist) === List(Leaf('e', 1)))
  }

  test("testing until"){

    val leaflist = List(Leaf('e', 1), Leaf('t', 2))
    assert(until(singleton,combine)(leaflist) === List(Fork(Leaf('e', 1),Leaf('t', 2),List('e','t'),3)))

  }


  test("testing until deeper"){

    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 5))
    assert(until(singleton,combine)(leaflist) === List(Fork(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3),Leaf('x',5),List('e', 't', 'x'),8)))

  }

  test("testing create"){

    val chars = string2Chars("abababbbbcc")
    assert(createCodeTree(chars) == Fork(Fork(Leaf('c',2),Leaf('a',3),List('c','a'),5),Leaf('b',6),List('c','a','b'),11))
  }

  test("decode"){
    val tree = Fork(Leaf('c',2),Leaf('a',3),List('c','a'),5)
    val code = List(0,1)
    assert(List('c','a') == decode(tree,code))
  }





  test("decode secret"){

    val message = decodedSecret
    assert(message==string2Chars("huffmanestcool"))
  }



  test("encode"){
    val tree = Fork(Leaf('b',2),Leaf('a',3),List('b','a'),5)
    val text = string2Chars("abba")
    assert(encode(tree)(text) == List(0,1,1,0))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }


    test("quickEncode"){
    val tree = Fork(Leaf('b',2),Leaf('a',3),List('b','a'),5)
    val text = string2Chars("abba")
    assert(quickEncode(tree)(text) == List(0,1,1,0))
  }

}
