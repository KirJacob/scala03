package example.oder.course1.week2

object OderskyWeek2TheoryB {


  //TODO - not clear enough alghoritm
  val tolerance = 0.0001

  def isCloseEnough(x: Double, y: Double) =
    Math.abs((x - y) / x) / x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      println(s"iterate=$guess")
      Thread.sleep(1000)
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }

    iterate(firstGuess)
  }

  def sqrt(x: Double) = fixedPoint(y => (y + x / y) / 2)(1)

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x)) / 2

  def sqrtNew2(x: Double) = {
    fixedPoint(averageDamp(y => x / y))(1)
  }

  def main(args: Array[String]): Unit = {
    //Lecture 2.3 - Example: Finding Fixed Points
    //    println(fixedPoint(x => 1 + x / 2)(1))
    println(sqrt(2))
    println(sqrtNew2(2))
  }
}
