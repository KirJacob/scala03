package example.oder.course1.week3

import scala.annotation.tailrec

object OderskyWeek3C {

  def main(args: Array[String]): Unit = {

    val cons01: Cons[Long] = new Cons[Long](1, new MyNil[Long])
    val cons02: Cons[Long] = new Cons[Long](1, new Cons[Long](2, new Cons[Long](4, new MyNil[Long])))
    singlethon[Int](1)
    singlethon[Boolean](true)
    singlethon(11)
    println(nth(-1, cons02))

  }

  def singlethon[T](elem: T) = new Cons[T](elem, new MyNil[T])

  def nth[T](n: Int, myList: MyList[T]): T = {
    @tailrec
    def iter(index: Int, list: MyList[T]): T = {
      if (list.isEmpty) throw new IndexOutOfBoundsException("index is bigger than list size")
      else if (index == n) list.head
      else iter(index + 1, list.tail)
    }

    iter(0, myList)
  }
}

trait MyList[T] {
  def isEmpty: Boolean

  def head: T

  def tail: MyList[T]
}

class Cons[T](val head: T, val tail: MyList[T]) extends MyList[T] {

  def isEmpty: Boolean = false

}

class MyNil[T] extends MyList[T] {

  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.tail")

}