package example.scala_ex

object ScalaEx9 {

  def greet(name: String): String =
    s"Hello, $name!"

  def pair(i: Int, s: String): (Int, String) = (i, s)

  trait Function1[A, B] {
    def apply(x: A): B
  }

  def methodWithDefault(start: Int, end: Int, step: Int = 2) = {
    Range(start, end, step)
  }

  case class RangeCustom(start: Int, end: Int, step: Int = 1)

  def average(x: Int, xs: Int*): Double =
    (x :: xs.to(List)).sum.toDouble / (xs.size + 1)

  def main(args: Array[String]): Unit = {
    greet("Kirill")
    greet("Vika")
    println(pair(10, "Pair")._1)
    println(pair(10, "Pair")._2)
    println(pair(1, "One"))

    val is: (Int, String) = (42, "foo")

    val result: String = is match {
      case (i, s) => s"this is $i from $s"
    }
    println(result)

    val f = new Function1[Int, Int] {
      def apply(x: Int) = x * x
    }
    println(f.apply(7))

    //FOR EXPRESSIONS
    val lst01: List[Int] = List(1, 2, 4, 11)
    lst01.map(x => x + 1)
    println(lst01.map(x => x + 1))
    println(for (x <- lst01) yield x + 1)
    println(lst01.filter(x => x % 2 == 0))
    println(for (x <- lst01 if x % 2 == 0) yield x)
    val lst02: List[List[Int]] = List(List(1, 2, 4, 11), List(-1, 1, 3, 10))
    println(lst02.flatten(x => x))
    val lst0x: List[Int] = List(10, 9, 8, 1)
    val lst0y: List[Int] = List(110, 109, 108, 101)
    println(lst0x.flatMap(x => lst0y.map(y => (x, y))))
    println(for (x <- lst0x; y <- lst0y) yield (x, y))

    val range01: RangeCustom = RangeCustom(1, 2)
    val range02: RangeCustom = RangeCustom(1, 2, 3)
    println(methodWithDefault(10, 20).sum)

    println(average(1, 2))
    println(average(1, 2, 3, 4))
    val xs: List[Int] = List(10, 13, 31, 42)
    println(average(1, xs: _*)) //pass list as a parameter

    //TYPE ALIASES
    type Result = Either[String, (Int, Int)]

    def divide(dividend: Int, divisor: Int): Result =
      if (divisor == 0) Left("Division by zero")
      else Right((dividend / divisor), dividend % divisor)

    println(divide(6,4))
  }
}
