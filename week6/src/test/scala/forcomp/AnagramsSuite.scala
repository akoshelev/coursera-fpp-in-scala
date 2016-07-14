package forcomp

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Anagrams._

@RunWith(classOf[JUnitRunner])
class AnagramsSuite extends FunSuite {

  test("wordOccurrences: abcd") {
    assert(wordOccurrences("abcd") === List(('a', 1), ('b', 1), ('c', 1), ('d', 1)))
  }

  test("wordOccurrences: Robert") {
    assert(wordOccurrences("Robert") === List(('b', 1), ('e', 1), ('o', 1), ('r', 2), ('t', 1)))
  }

  test("wordOccurrences: Roar") {
    assert(wordOccurrences("roar") === List(('a', 1), ('o', 1), ('r', 2)))
  }

  test("wordOccurrences: empty string") {
    assert(wordOccurrences("") === List())
  }

  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }

  test("sentenceOccurrences: zxa fd") {
    assert(sentenceOccurrences(List("zxa", "fd")) === List(('a', 1), ('d', 1), ('f', 1), ('x', 1), ('z', 1)))
  }

  test("sentenceOccurrences: empty sencences") {
    assert(sentenceOccurrences(List("")) === List())
  }


  test("dictionaryByOccurrences.get: eat") {
    assert(dictionaryByOccurrences.get(List(('a', 1), ('e', 1), ('t', 1))).map(_.toSet) === Some(Set("ate", "eat", "tea")))
  }

  test("word anagrams: married") {
    assert(wordAnagrams("married").toSet === Set("married", "admirer"))
  }

  test("word anagrams: player") {
    assert(wordAnagrams("player").toSet === Set("parley", "pearly", "player", "replay"))
  }

  test("word anagrams: Admirer") {
    assert(wordAnagrams("Admirer").toSet === Set("married", "admirer"))
  }

  test("word anagrams: Ambec") {
    assert(wordAnagrams("Ambec").toSet === Set())
  }



  test("combinations: []") {
    assert(combinations(Nil) === List(Nil))
  }

  test("combinations: abba") {
    val abba = List(('a', 2), ('b', 2))
    val abbacomb = List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
    assert(combinations(abba).toSet === abbacomb.toSet)
  }

  test("combinations: abc") {
    val abc = List(('a', 1), ('b', 1), ('c', 1))
    val abccomb = List(
      List(),
      List(('a', 1)),
      List(('b', 1)),
      List(('c', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 1), ('c', 1)),
      List(('b', 1), ('c', 1)),
      List(('a', 1), ('b', 1), ('c', 1))
    )
    assert(combinations(abc).toSet === abccomb.toSet)
  }

  test("subtract: lard - r") {
    val lard = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    val lad = List(('a', 1), ('d', 1), ('l', 1))
    assert(subtract(lard, r) === lad)
  }

  test("subtract: alla - a") {
    val alla = List(('a', 2), ('l', 2))
    val a = List(('a', 1))
    val all = List(('a', 1), ('l', 2))
    assert(subtract(alla, a) === all)
  }

  test("subtract: lottery - t") {
    val lottery = List(('e', 1), ('l', 1), ('o', 1), ('r', 1), ('t', 2), ('y', 1))
    val t = List(('t', 1))
    val lotery = List(('e', 1), ('l', 1), ('o', 1), ('r', 1), ('t', 1), ('y', 1))
    assert(subtract(lottery, t) === lotery)
  }

  test("subtract: xz - x") {
    val xz = List(('x', 1), ('z', 1))
    val x = List(('x', 1))
    val z = List(('z', 1))

    assert(subtract(xz, x) === z)
  }


  trait TestAnagrams {
    val empty = List()
    val yesMan = List(
      List("en", "as", "my"),
      List("en", "my", "as"),
      List("man", "yes"),
      List("men", "say"),
      List("as", "en", "my"),
      List("as", "my", "en"),
      List("sane", "my"),
      List("Sean", "my"),
      List("my", "en", "as"),
      List("my", "as", "en"),
      List("my", "sane"),
      List("my", "Sean"),
      List("say", "men"),
      List("yes", "man")
    )
    val linuxRulez = List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
  }

  test("sentence anagrams: []") {
    new TestAnagrams {
      assert(sentenceAnagrams(empty) === List(Nil))
      assert(sentenceAnagramsMemo(empty) === List(Nil))
    }
  }

  test("sentence anagrams: Yes Man") {
    val sentence = List("Yes", "man")
    new TestAnagrams {
      assert(sentenceAnagrams(sentence).toSet === yesMan.toSet)
      assert(sentenceAnagramsMemo(sentence).toSet === yesMan.toSet)
    }
  }

  test("sentence anagrams: Linux rulez") {
    val sentence = List("Linux", "rulez")
    new TestAnagrams {
      assert(sentenceAnagrams(sentence).toSet === linuxRulez.toSet)
      assert(sentenceAnagramsMemo(sentence).toSet === linuxRulez.toSet)
    }
  }

}
