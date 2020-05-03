import example.oder.course1.week3.IntSet

println("Hello World from Scala Worksheet")

def abs(x: Double) = if (x < 0) -x else x

abstract class IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
}

//Binary trees example
class Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmpty(x, new Empty, new Empty)
}

class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }

  def incl(x: Int): IntSet =
    if (x < elem) new NonEmpty(elem, left incl x, right)
    else if (x > elem) new NonEmpty(elem, left, right incl x)
    else this
}