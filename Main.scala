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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }
  /**
   * Exercise 2
   */
  def balanceIter(chars: List[Char], count: Int): Boolean =
    if (chars.isEmpty && count == 0) true
    else if (chars.isEmpty && count != 0) false
    else if (count == 0 && chars.head.toString == ")") false
    else if (chars.head.toString == ")") balanceIter(chars.tail, count - 1)
    else if (chars.head.toString == "(") balanceIter(chars.tail, count + 1)
    else balanceIter(chars.tail, count)
  def balance(chars: List[Char]): Boolean = balanceIter(chars, 0)

  /**
   * Exercise 3
   */

  def countChange(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0
    else if (money == 0) 0
    else countChangeRecord(money, 0, coins)
  } 
  def countChangeRecord(money: Int, previous: Int, coins: List[Int]): Int = {
    if (money < previous) 0
    else if (money > coins.head && coins.head >= previous) countChangeRecord(money - coins.head, coins.head, coins) + countChangeIter(money, coins.tail, previous, coins)
    else if (money == coins.head && coins.head >= previous) 1 + countChangeIter(money, coins.tail, previous, coins)
    else countChangeIter(money, coins.tail, previous, coins)
  } 
  def countChangeIter(money: Int, left: List[Int], prev: Int, coins: List[Int]): Int = {
    if (left.isEmpty) 0
    else if (money > left.head && left.head >= prev) countChangeRecord(money - left.head, left.head, coins) + countChangeIter(money, left.tail, prev, coins)
    else if (money == left.head && left.head >= prev) 1 + countChangeIter(money, left.tail, prev, coins)
    else countChangeIter(money, left.tail, prev, coins)
  }
}
