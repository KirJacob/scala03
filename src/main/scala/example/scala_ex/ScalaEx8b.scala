package example.scala_ex

object ScalaEx8b {
  def main(args: Array[String]): Unit = {

    //map and filter
    println(List(1, 2, 3).map(x => x + 1))
    println(List(1, 2, 3).map(x => x + 1).filter(x => x % 2 == 0))

    //flatmap
    val xs =
      List(1, 2, 3).flatMap { x =>
        List(x, x * 2, x * 3)
      }
    println(xs)

    //Option
    def sqrt(x: Double): Option[Double] =
      if (x < 0) None else Option(x * x)

    println(sqrt(-2))
    println(sqrt(2).get)

    def foo(x: Double): String =
      sqrt(x) match {
        case None => "no result"
        case Some(y) => y.toString
      }

    println(foo(-1))
    println(foo(1))

    List(1,2).filter(x => x % 2 == 0)
    println(Some(2).flatMap(x => Some(x+1)))

    //TRY and EITHER

  }
}
