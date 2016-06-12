package recfun

import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    * Computes the elements of Pascal’s triangle by means of a recursive process.
    */
    def pascal(c: Int, r: Int): Int = {
    // if we're on the left edge (c == 0)
    // or on the right edge (c == r)
      if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
    * Exercise 2
    * Verifies the balancing of parentheses in a string, which we represent as a List[Char] not a String.
    * For example, the function should return true for the following strings:
    * (if (zero? x) max (/ 1 x))
    * I told him (that it’s not (yet) done). (But he wasn’t listening)
    * The function should return false for the following strings:
      :-)
      ())(
   */
    def balance(chars: List[Char]): Boolean = {

      def matchChar(char: Char): Int = {
        char match {
          case '(' => 1
          case ')' => -1
          case _ => 0
        }
      }

      @tailrec
      def balanceLoop(chars: List[Char], cnt: Int): Boolean = {
        if (chars.isEmpty) cnt == 0
        // as soon as we find a closing parenthesis without an opening pair
        // we can say that string is unbalanced
        else if (cnt < 0) false
        else balanceLoop(chars.tail, cnt + matchChar(chars.head))
      }

      balanceLoop(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {

      def rec(money: Int, mc: List[Int]): Int = {

        if (money == 0) 1
        else if (money < 0 || mc.isEmpty) 0

        // The number of ways to change amount a using n kinds of coins equals
        // the number of ways to change amount a using all but the first kind of coin, plus
        // the number of ways to change amount a - d using all n kinds of coins, where d is the denomination
        // of the first kind of coin.
        else rec(money, mc.tail) + rec(money - mc.head, mc)
      }

      if (money == 0) 0
      else rec(money, coins)
    }
  }
