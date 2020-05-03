package example.oder.course1.week2

object OderskyWeek2C {
  def main(args: Array[String]): Unit = {

    def r1: Rational = new Rational(10, 20)

    def rx: Rational = new Rational(1, 2)

    def ry: Rational = new Rational(1, 3)

    def rz: Rational = new Rational(1, 4)

    def r2: Rational = new Rational(3)


    def rc1: RationalCase = new RationalCase(1, 2)

    println(r2)
    println(s"${r1.denum} ${r1.numer}")
    println(s"${r1.toString}")
    println(s"${rc1}")
    println(r1)
    println(r1.makeString())
    println(r1.add(new Rational(3, 4)))
    println(rx.add(ry).neg().substrat(rz))
    println(rx + ry - rz)

    //Infix notation
    println(r1 add r2)

  }
}

class Rational(x: Int, y: Int) {

  def this(x: Int) = this(x, 1) //if we need many constructors

  assert(y != 0, "assert: denumenator should not be a zero")
  require(y != 0, "denumenator should not be a zero")

  def numer = x

  def denum = y


  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)


  def less(that: Rational) = numer * that.denum < that.numer * denum

  def <(that: Rational) = numer * that.denum < that.numer * denum

  def max(that: Rational) = if (this < that) that else this

  // 1/2 - 3/4
  def add(that: Rational): Rational = {
    new Rational(x * that.denum + that.numer * y, that.denum * y)
  }

  def +(that: Rational): Rational = {
    new Rational(x * that.denum + that.numer * y, that.denum * y)
  }

  def neg(): Rational = new Rational(-x, y)

  def substrat(that: Rational): Rational = {
    new Rational(x * that.denum - y * that.numer, that.denum * y)
  }

  def -(that: Rational): Rational = {
    new Rational(x * that.denum - y * that.numer, that.denum * y)
  }

  def substrat2(that: Rational) = add(that.neg())

  def mul(that: Rational): Rational = {
    new Rational(x * that.numer, y * that.denum)
  }

  def makeString(): String = {
    s"x=$x y=$y"
  }

  override def toString: String = {
    val g = gcd(x, y)

    val visibleNum = if (y < 0) -x / g else x / g

    val visibleDenum = if (y < 0) -y / g else y / g

    s"$visibleNum/$visibleDenum"
  }

}

case class RationalCase(x: Int, y: Int)
