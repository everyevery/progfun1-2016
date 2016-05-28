package recfun

import sun.font.TrueTypeFont

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
    if (c == 0 && r == 0)
      1
    else if (c < 0 || r < c)
      0
    else
      pascal(c-1, r-1) + pascal(c, r-1)
  }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
    def balanceIter(chars: List[Char], count: Int): Boolean = chars match {
      case Nil => count == 0
      case '(' :: cs => balanceIter(cs, count + 1)
      case ')' :: cs => if (0 < count) balanceIter(cs, count - 1) else false
      case _ :: cs => balanceIter(cs, count)
    }
    balanceIter(chars, 0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeIter(money: Int, coins: List[Int]): Int = coins match {
      case Nil => if (money == 0) 1 else 0
      case (c :: cs) => if (money < c)
        countChangeIter(money, cs)
      else
        countChangeIter(money - c, coins) + countChangeIter(money, cs)

    }
    countChangeIter(money, coins.sortBy(-_))
  }

  }
