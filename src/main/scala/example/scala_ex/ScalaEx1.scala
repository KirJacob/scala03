package example.scala_ex

import scala.annotation.tailrec

object ScalaEx1 {

  def gcd(a: Int, b: Int): Int = {
    println(s"a=${a} b=${b}")
    if (b == 0) a else gcd(b, a % b)
  }

  def factorial(n: Int): Int = {
    println(s"n=${n}")
    if (n == 1) 1 else factorial(n - 1) * n
  }

  def factorialTR(n: Int): Int = {
    @tailrec
    def iter(x: Int, result: Int): Int = {
      println(s"x=${x} result=${result}")
      if (x == 1) result
      else iter(x - 1, result * x)
    }
    iter(n, 1)
  }

  def gcdTR(a: Int, b: Int): Int = {
    @tailrec
    def iter(x: Int, result: Int): Int = {
      println(s"x=${x} result=${result}")
      if (result == 0) x
      else iter(result, result % x)
    }

    iter(a, b)
  }

  def main(args: Array[String]): Unit = {
    gcd(14, 21)
    println("----------")
    factorial(10)
    println("----------")
    factorialTR(5)
    println("----------")
    println(gcdTR(14, 21))
  }
}
