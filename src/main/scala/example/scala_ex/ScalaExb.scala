package example.scala_ex

object ScalaExb {
  def main(args: Array[String]): Unit = {
    val base01: Sub02 = new Sub02()
    base01.bar
    val nums = List(1,2,3,4)
    println(Product.reduce(nums))
    println(Sum.reduce(nums))
  }
}

abstract class IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean
}


class Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
}

abstract class Base {
  def foo = 1

  def bar: Int
}

//OVERRIDING
class Sub extends Base {
  override def foo = 2

  def bar = 3
}

class Sub02 extends Base {
  def bar = 3
}

object Empty02 extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NonEmpty(x, Empty02, Empty02)
}

trait Planar {
  def height: Int

  def width: Int

  def surface = height * width
}

abstract class Reducer(init: Int) {
  def combine(x: Int, y: Int): Int

  def reduce(xs: List[Int]): Int ={
    println(s"xs=$xs")
    xs match {
      case Nil => init
      case y :: ys => combine(y, reduce(ys))
    }
  }

}

object Product extends Reducer(1) {
  def combine(x: Int, y: Int): Int = {
    println(s"x=$x y=$y")
    x * y
  }
}

object Sum extends Reducer(0) {
  def combine(x: Int, y: Int): Int = {
    println(s"x=$x y=$y")
    x + y
  }
}