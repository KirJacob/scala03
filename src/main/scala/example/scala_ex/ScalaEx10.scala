package example.scala_ex

object ScalaEx10 {

  //these are static methods
  def addRationalSt(r: Rational, s: Rational): Rational =
    new Rational(
      r.numer * s.denom + s.numer * r.denom,
      r.denom * s.denom
    )

  //these are static methods
  def makeStringSt(r: Rational) =
    r.numer + "/" + r.denom


  def main(args: Array[String]): Unit = {
    val r01: Rational = new Rational(1, 2)
    val r02: Rational = new Rational(2, 3)
    val r03: Rational = new Rational(2, 33)

    println(makeStringSt(addRationalSt(r01, r02)))
    println(r01.add(r02))
    println(r01.mul(r02))

    println(r01 + r02 * r03)
  }
}

class Rational(x: Int, y: Int) {
  //  def numer = x
  //  def denom = y

  //parameters validation
  require(y > 0, "denominator must be positive")

  private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

  def numer = x / gcd(x, y)

  def denom = y / gcd(x, y)

  def printRational(): String = {
    s"x=$x y=$y"
  }

  //AUXILIARY CONSTRUCTOR
  def this(x: Int) = this(x, 1)

  def add(r: Rational): Rational =
    new Rational(
      numer * r.denom + r.numer * denom,
      r.denom * denom
    )

  def +(r: Rational) =
    new Rational(
      numer * r.denom + r.numer * denom,
      denom * r.denom
    )

  def mul(r: Rational): Rational =
    new Rational(
      numer * r.denom,
      r.denom * denom
    )

  def *(r: Rational): Rational =
    new Rational(
      numer * r.denom,
      r.denom * denom
    )

  override def toString = numer + "/" + denom

  def less(that: Rational) =
    this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) =
    if (this.less(that)) that else this

}
