package example.oder.course1.week3

import example.oder.course1.week2.Rational
// THIS IS AN EXAMPLE OF wild-card import
import example.oder.course1.week2._
import example.oder.course1.week2.{Rational, RationalCase}

object OderskyWeek3B {
  def error(msg: String) = throw new Error(msg)

  def main(args: Array[String]): Unit = {
    //access class from other package
    val r1: Rational = new Rational(1, 1)
    new example.oder.course1.week2.Rational(1, 1)
    val sc1: SampleClass = new SampleClass(1)
    println(sc1.someValue)
    println(s"${sc1.methodForAll(2)}")
    println(sc1.traitOneMethod(5))

    val x = null
    val y: String = null
    // not a valid expression
    // val z: Int = null

    //Throw ERROR
    //error("Not good")

    //EXCEPTION THROW
    //throw new Exception

    println(sc1.traitTwoMethod(6))

    val variable: Any = "Hey"
    val variable2: AnyVal = 2
    val variable3: AnyRef = "2"

  }
}
