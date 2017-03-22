package recfun

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

    if (r == c || c == 0) 1
    else
      pascal(c, r - 1) + pascal(c - 1, r - 1)
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {

    def matcher(accu: List[Char], list: List[Char]): Boolean = {

      if (list.isEmpty) false
      else {
        list.head match {
          case ')' => {
            if (accu.tail.isEmpty) balance(list.tail)
            else matcher(accu.tail, list.tail)
          }
          case '(' => matcher('(' :: accu, list.tail)
          case _ => matcher(accu, list.tail)
        }
      }

    }

    if (chars.isEmpty) true
    else {

      chars.head match {
        case ')' => false

        case '(' => matcher(List.apply(chars.head), chars.tail)

        case _ => balance(chars.tail)
      }

    }

  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {

    def total(money: Int, coins: List[Int]): Int = {
      if (money - coins.head == 0) 1
      else {

        if (money - coins.head < 0) 0
        else {
          if (coins.tail.isEmpty && (money - coins.head) > 0)
            total(money - coins.head, coins)
          else
            total(money - coins.head, coins) + total(money, coins.tail)
        }
      }

    }

    total(money, coins.sorted)

  }
}
