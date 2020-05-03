package example.oder.course1.week2

object OderskyWeek2Theory {

  def sum(f: Int => Int, a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      println(s"a=$a acc=$acc")
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }

    loop(a, 0)
  }

  def cube(x: Int): Int = x * x * x

  def factorial(x: Int): Int = {
    def loop(x: Int, acc: Int): Int = {
      println(s"loop => x=$x acc=$acc")
      if (x == 0) acc
      else loop(x - 1, acc * x)
    }

    loop(x, 1)
  }

  //Functions as a parameters and as a return type
  def funcWithParam(f: Int => Int, x: Int, n: Int): Int = {
    Math.pow(x, n).toInt
  }

  def distFunc2(x: Int, y: Int): Double = {
    Math.sqrt(x * x + y * y)
  }

  def distance(f: (Int, Int) => Double, x: Int, y: Int): Int = {
    0
  }

  //passing two arguments function as a parameter
  distance(distFunc2, 1, 2)

  //Anonymous functions
  //passing anonymous function as a parameter below
  sum((x: Int) => x * x * x, 1, 3)

  def sumDouble(f: (Int, Int) => Double, x: Int, y: Int): Double = {
    0
  }

  sumDouble((x: Int, y: Int) => Math.sqrt(x * x + y * y), 1, 3)

  def sumCubes = sumNew(x => x * x * x)


  //returning function as a result
  def sumNew(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int = {
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    }

    sumF
  }

  //shorter option of the function above
  def sumNew2(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0
    else f(a) + sumNew2(f)(a + 1, b)

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    println(s"product=$f a=$a b=$b")
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)
  }

  def generalize(f: Int => Int, agr: (Int, Int) => Int, init: Int)(a: Int, b: Int): Int = {
    if (a > b) init
    else agr(f(a), generalize(f, agr, init)(a + 1, b))
  }

  def productNew3(f: Int => Int)(a: Int, b: Int): Int = generalize(f, (y, z) => y * z, 1)(a, b)

  def sumNew3(f: Int => Int)(a: Int, b: Int): Int = generalize(f, (y, z) => y + z, 0)(a, b)

  def factorialNew(x: Int): Int = product(n => n)(1, x)

  def main(args: Array[String]): Unit = {
    //Hardening recursion
    println(">>>>>>> Hardening recursion")
    println(factorial(4))
    println(sum(cube, 1, 3))
    println(sum(factorial, 1, 3))

    //CURRYING
    println(">>>>>>> CURRYING")
    println(sumCubes(1, 5))

    println(sumNew2(x => x * x)(1, 3))
    println(sumNew2(x => x)(1, 3))
    println(product(x => x * x)(1, 2))
    println(factorialNew(5))

    println(generalize(x => x * x, (y, z) => y * z, 1)(1, 2))
    println(generalize(x => x * x, (y, z) => y + z, 0)(1, 2))
    println(">>>>>>>")
    println(productNew3(x => x * x)(1, 2))
    println(sumNew3(x => x * x)(1, 2))

  }
}
