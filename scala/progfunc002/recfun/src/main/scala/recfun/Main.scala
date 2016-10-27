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
     if(c==0 ) 1 else if(r==0) 0 else pascal(c,r-1)+pascal(c-1,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    if (chars.isEmpty) true else countParenthese(chars,0,0)
  }
  def countParenthese(chars: List[Char], open:Int, close:Int): Boolean= {
    if (chars.isEmpty) (open == close) else
    if (open<close) false else
    if (chars.head == '(') countParenthese(chars.tail, open+1, close) else
    if (chars.head == ')') countParenthese(chars.tail, open, close+1)
    else countParenthese(chars.tail, open, close)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(coins.isEmpty || money == 0) 0 else
    drawTree(money, coins.filter(_< money))
  }

  def drawTree(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty) 0 else
    if (money>0) drawTree(money-coins.head, coins) + drawTree(money, coins.tail) else
    if (money == 0)  1 else 0
  }

}
