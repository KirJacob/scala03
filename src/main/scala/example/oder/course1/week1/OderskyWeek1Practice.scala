package example.oder.course1.week1

object OderskyWeek1Practice {

  /*

    1
   1 1
  1 2 1
 1 3 3 1
1 4 6 4 1

def pascal(c: Int, r: Int): Int
pascal(0,2) = 1
pascal(1,2) = 2
pascal(1,3) = 1

              (0,0)
          (0,1)   (1,1)
      (0,2)   (1,2)   (2,2)
  (0,3)   (1,3)   (2,3)   (3,3)

pascal(0,0) = 1
pascal(x,y) = pascal(x-1,y-1) + pascal(x,y-1)
if (x==0) pascal(x,y) = pascal(x,y-1)
if (x==y) pascal(x,y) = pascal(x-1, y-1)
 */

  def pascal(c: Int, r: Int): Int = {
    var result: Int = 0
    if (c != 0 && r != 0) result = pascal(c - 1, r - 1) + pascal(c, r - 1)
    if (c == 0 && r != 0) result = pascal(c, r - 1)
    if (c == r && c != 0) result = pascal(c - 1, r - 1)
    if (c == 0 && r == 0) result = 1
    result
  }

  def cleanFromNotParenthesis(result: List[Char], chars: List[Char]): List[Char] = {
    if (chars.isEmpty) result.reverse
    else if ((chars.head != '(') && (chars.head != ')')) cleanFromNotParenthesis(result, chars.tail)
    else cleanFromNotParenthesis(chars.head :: result, chars.tail)
  }

  def countParenthesis(acc: Int, chars: List[Char]): Int = {
    var change: Int = 0
    if (chars.nonEmpty) {
      if (chars.head == '(') change = 1
      else if (chars.head == ')') change = -1
      else change = 0
      println(s"acc=$acc change=$change chars=$chars")
    }

    if (chars.isEmpty) acc
    else countParenthesis(acc + change, chars.tail)
  }

  def balance(chars: List[Char]): Boolean = {

    def cleanFromNotParenthesis(result: List[Char], chars: List[Char]): List[Char] = {
      if (chars.isEmpty) result.reverse
      else if ((chars.head != '(') && (chars.head != ')')) cleanFromNotParenthesis(result, chars.tail)
      else cleanFromNotParenthesis(chars.head :: result, chars.tail)
    }

    def iter(flag: Boolean, result: List[Char], list: List[Char]): List[Char] = {
      println(s"flag=$flag list=$list result=$result")
      if (flag) {
        println(s"result=$result head=${list.head} tail=${list.tail}")
        result ::: list.tail
      } else if (list.size < 2) {
        list
      } else if ((list.head == '(') && (list.tail.head == ')')) {
        println(s"FIRST tail=${list.tail} result=$result")
        iter(true, result, list.tail)
      } else {
        println(s"SECOND tail=${list.tail} result=$result")
        iter(false, list.head :: result, list.tail)
      }
    }

    def balanceParenthesis(changes: Int, charsInt: List[Char], size: Int): Boolean = {
      Thread.sleep(1000)
      println(s"CHARS=$charsInt size=$size DIFF=${charsInt.length == size}")
      if ((changes > 0) && (charsInt.length == size)) charsInt.isEmpty
      else balanceParenthesis(changes + 1, iter(false, List(), charsInt), charsInt.length)
    }

    val cleared: List[Char] = cleanFromNotParenthesis(List(), chars)
    balanceParenthesis(0, cleared, cleared.length)
  }

  def countChange(money: Int, coins: List[Int]): Int = {
    def loopIter(count: Int, list: List[Int], listCoins: List[Int], listCoinsConst: List[Int], money: Int,
                 intermResult: List[List[Int]], finalResult: List[List[Int]]): List[List[Int]] = {
      if (count > 0 && count == intermResult.size - 1) finalResult
      else if (listCoins.nonEmpty) {
        val resultSize = finalResult.size - 1
        //        if (resultSize % 100 == 0)
        println(s"SIZE=$resultSize list=$list intermResult=${intermResult.reverse} finalResult=${finalResult.reverse} listCoins=$listCoins money=$money head=${listCoins.head} const=$listCoinsConst count=$count")

        val element: Int = listCoins.head
        val iterList: List[Int] = list ::: List(element)
        val sumRes: Int = list.sum + element
        val incrList: List[List[Int]] = if ((sumRes == money) && (list.isEmpty || (list.nonEmpty && list.last <= element))) iterList :: finalResult
        else finalResult

        if ((list.nonEmpty && list.last > element) || (sumRes > money))
          loopIter(count, list, listCoins.tail, listCoinsConst, money, intermResult, incrList)
        else loopIter(count, list, listCoins.tail, listCoinsConst, money, iterList :: intermResult, incrList)
      } else {
        loopIter(count + 1, intermResult.reverse.tail(count), listCoinsConst, listCoinsConst, money, intermResult, finalResult)
      }
    }

    loopIter(0, List(), coins, coins, money, List(Nil), List()).size
  }


  def main(args: Array[String]): Unit = {
    //    println(pascal(2, 4))
    //    println(balance(List('(', '(', ')', '(', ')', ')')))
    val someList: List[Int] = List(1, 2, 4)
    println(countChange(4, someList))
    //    val start: Long = System.currentTimeMillis()
    //    println(countChange(301, List(5, 10, 20, 50, 100, 200, 500)))
    //    val end: Long = System.currentTimeMillis()
    //    println(s"time=${end - start}")
    //    iterLoop(someList)


  }

  //()->
  //->(2)->(2,2):(2,3)
  //->(3)
  //->(5)

  //(), (1,2,4), (1,2,4), 4, 0
  //1, List(1)

  //  def iterLoop(list: List[Int], listCoins: List[Int], listCoinsConst: List[Int], money: Int, result: Int): Int = {
  //    if (listCoins.nonEmpty) {
  //      println(s"list=$list result=$result listCoins=$listCoins money=$money head=${listCoins.head}")
  //      val iterList: List[Int] = listCoins.head :: list
  //      val element: Int = listCoins.head
  //      val res: Int = (listCoins.head :: list).reverse.sum
  //      val increment: Int = if (res == money) 1 else 0
  //
  //      println(s"iterList=$iterList element=$element res=$res increment=$increment")
  //      //        if (res < money) iterLoop(iterList, listCoinsConst, listCoinsConst, money, result)
  //      iterLoop(list, listCoins.tail, listCoinsConst, money, result + increment)
  //    } else result
  //  }
  //    iterLoop(List(), coins, coins, money, 0)

}
