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
  def pascal(c: Int, r: Int): Int = if (c == 0 || r == 0 || c == r) 1 else pascal(c-1, r-1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balance_sub(chars: List[Char], num: Int): Boolean = {
      if (num < 0) {
        false
      } else if (chars.isEmpty) {
        num == 0
      } else {
        if (chars.head == '(') {
          balance_sub(chars.tail, num + 1)
        } else if (chars.head == ')') {
          balance_sub(chars.tail, num - 1)
        } else {
          balance_sub(chars.tail, num)
        }
      }
    }
    balance_sub(chars, 0)
  }                                         

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChange_sub(money: Int, coins: List[Int]): Int = {
      if (money == 0) {
        1
      } else if (coins.isEmpty) {
        0
      } else if (money < 0) {
        0
      } else {
        countChange_sub(money - coins.head, coins) + countChange_sub(money, coins.tail)
      }
    }
    countChange_sub(money, coins.sorted)
  }                   
}
