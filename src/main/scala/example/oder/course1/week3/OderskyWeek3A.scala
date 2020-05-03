package example.oder.course1.week3

object OderskyWeek3A {

  class MySuperClass(varStr: String, varInt: Int) {
    def foo: Int = 1

    def someMethod(x: Int): String = s"x=$x"
  }

  def main(args: Array[String]): Unit = {

    val res01: MySuperClass = new MySuperClass("Sylvan Path", 39)
    println(res01.foo)
    println(res01.someMethod(10))

    val tree: IntSet = new NonEmpty(3,
      new NonEmpty(1, Empty, Empty),
      new NonEmpty(2, Empty, Empty)
    )

    val t1: NonEmpty = new NonEmpty(6, Empty, Empty)
    val t2 = t1 incl 4 incl 10 incl 2 incl 5 incl 11
    val t3: NonEmpty = new NonEmpty(7, new NonEmpty(1, Empty, Empty), new NonEmpty(20, Empty, Empty))
    println(t2)
    println(t2.union(t3))

    //CLASS HIERARCHIES

  }
}

//ABSTRACT CLASS may not have implementation in methods, for usual class its not possible
//absract class in scala cannot be instantiated
abstract class IntSet {
  def incl(x: Int): IntSet

  def contains(x: Int): Boolean

  def union(other: IntSet): IntSet
}

//Binary trees example
object Empty extends IntSet {
  def contains(x: Int): Boolean = false

  def incl(x: Int): IntSet = new NonEmpty(x, Empty, Empty)

  def union(other: IntSet): IntSet = other

  override def toString: String = "."
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def incl(x: Int): IntSet = {
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
  }

  def union(other: IntSet): IntSet = {
    println(s"union=> this=$this other=$other elem=$elem")
    ((left union right) union other) incl elem
  }

  override def toString: String = "{" + left + elem + right + "}"
}

