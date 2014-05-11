package recfun
import common._

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
  def pascal(c: Int, r: Int): Int = if (c == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(open: Int, xs: List[Char]): Boolean = {
      if (open < 0) false
      else if (xs.isEmpty) open == 0
      else if (xs.head == '(') balanceIter(open+1, xs.tail)
      else if (xs.head == ')') balanceIter(open-1, xs.tail)
      else balanceIter(open, xs.tail)
    }
    balanceIter(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count(money: Int, coins: List[Int]): Int = {
        if (money == 0) 1
        else if (money < 0 || coins.isEmpty) 0
        else count(money - coins.head, coins) + count(money, coins.tail)
    }
    count(money, coins.sorted)
  }
}
