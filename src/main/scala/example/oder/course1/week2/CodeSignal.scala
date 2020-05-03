package example.oder.course1.week2

object CodeSignal {

  def century(year: Int): Int = {
    ((year - 1) / 100) + 1

  }

  def checkPalindrome(inputString: String): Boolean = {
    inputString.reverse == inputString
  }

  def main(args: Array[String]): Unit = {

    println(century(1905))
    println(century(1900))
    println(century(2000))
    println(checkPalindrome("a2a"))
    println(checkPalindrome("a2a1"))


  }
}
