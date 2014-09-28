package recfun

object session {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  def pascal(c: Int, r: Int): Int = if (c == 0 || r == 0 || c == r) 1 else pascal(c-1, r-1) + pascal(c, r - 1)
                                                  //> pascal: (c: Int, r: Int)Int
  pascal(0,2)                                     //> res0: Int = 1
  pascal(1,2)                                     //> res1: Int = 2
  pascal(1,3)                                     //> res2: Int = 3
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
  }                                               //> balance: (chars: List[Char])Boolean
  balance("(if (zero? x) max (/ 1 x))".toList)    //> res3: Boolean = true
  balance(":-)".toList)                           //> res4: Boolean = false
  balance("())(".toList)                          //> res5: Boolean = false
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
  }                                               //> countChange: (money: Int, coins: List[Int])Int
  countChange(4, List(1,2))                       //> res6: Int = 3
}