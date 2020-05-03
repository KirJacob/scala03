package example.oder.course1.week1

object ScalaLecture15 {

  def sqrt(x: Double, precision: Int): Double = {
    var result: Double = 1
    for (i <- 0 until precision) {
      result = calc(x, result)
    }
    result
  }

  def calc(x: Double, y: Double) = {
    (y + x / y) / 2
  }

  def abs(x: Double): Double = {
    if (x < 0) -x else x
  }


  def factorial(n: Int): Long = {
    if (n == 1) 1
    else factorial(n - 1) * n
  }

  def sqrt2(x:Double) = {
    def isGoodEnough(guess: Double, x: Double): Boolean = {
      abs(guess * guess - x) < 0.001
    }

    def improve(guess: Double, x: Double): Double = {
      (guess + x / guess) / 2
    }

    def sqrIter(guess: Double, x: Double): Double = {
      if (isGoodEnough(guess, x)) guess
      else sqrIter(improve(guess, x), x)
    }
    sqrIter(1.0, x)
  }

  def main(args: Array[String]): Unit = {
    println(sqrt(2, 10))
    println(sqrt2(2))
    println(factorial(5))
    //this is two expressions
    def x = 2
    +3
    println(x)
    println("----------")
    println(sqrt2(0.001))
    println(sqrt2(0.1e-20))
    println(sqrt2(1.0e20))
    println(sqrt2(1.0e50))

  }
  //

}
