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
   */
    def pascal(c: Int, r: Int): Int = {
      if (c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }

  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def checkChar(char: Char) = {
        if (char == '(') 1
        else if (char == ')') -1
        else 0
      }
      @tailrec
      def balanceIter(rest: List[Char], parenthesesSum: Int): Boolean = {
        if (rest.isEmpty) parenthesesSum == 0
        else if (parenthesesSum < 0) false
        else balanceIter(rest.tail, parenthesesSum + checkChar(rest.head))
      }
      balanceIter(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if (money < 0) 0
      else if (coins.isEmpty) 0
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
