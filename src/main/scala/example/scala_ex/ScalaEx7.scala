package example.scala_ex

import scala.annotation.tailrec

object ScalaEx7 {
  def sumInts(a: Int, b: Int): Int =
    if (a > b) 0 else a + sumInts(a + 1, b)

  def cube(x: Int): Int =
    x * x * x

  def factorialTR(n: Int): Int = {
    @tailrec
    def iter(x: Int, result: Int): Int = {
      if (x == 1) result
      else iter(x - 1, result * x)
    }

    iter(n, 1)
  }

  def factorialMO(n: Int): Int = {
    def loop(acc: Int, n: Int):Int =
      if (n == 0) acc
      else loop(n - 1, acc * n)
    loop(n, 1)
  }

  def sumCubes(a: Int, b: Int): Int =
    if (a > b) 0 else cube(a) + sumInts(a + 1, b)

  def sumFactorialsTR(a: Int, b: Int): Int =
    if (a > b) 0 else factorialTR(a) + sumInts(a + 1, b)

  def sum(f: Int => Int, a: Int, b: Int) =
    if (a > b) 0 else f(a) + sumInts(a + 1, b)

  def sumEx(f: Int => Int, a: Int, b: Int): Int = {
    def loop(x: Int, acc: Int): Int = {
      println(s"x=$x acc=$acc")
      if (x > b) acc
      else loop(x + 1, acc + f(x))
    }

    loop(a, 0)
  }

  def main(args: Array[String]): Unit = {
    def id(x: Int): Int = x

    def sumOfInts(a: Int, b: Int) = sum(id, a, b)

    def sumOfCubes(a: Int, b: Int) = sum(cube, a, b)

    def sumOfFactorials(a: Int, b: Int) = sum(factorialTR, a, b)

    println(sumCubes(1, 3))

    def sumOfIntsA(a: Int, b: Int) = sum(x => x, a, b)

    //other way to write down the function as a parameter
    def sumOfCubesA(a: Int, b: Int) = sum(x => x * x * x, a, b)

    def sumOfCubesB(a: Int, b: Int) = sum({
      def f(x: Int): Int = x * x * x; f
    }, a, b)

    sumEx(x => x, 1, 10)

  }

}
