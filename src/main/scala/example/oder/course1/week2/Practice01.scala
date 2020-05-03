package example.oder.course1.week2

object Practice01 {
  def main(args: Array[String]): Unit = {
    type FunSet = Int => Boolean

    def contains(s: FunSet, elem: Int): Boolean = s(elem)

    def func3(x: Int): Boolean = x % 3 == 0

    def func2(x: Int): Boolean = x % 2 == 0

    def func5(x: Int): Boolean = x % 5 == 0

    def singletonSet(elem: Int): FunSet = x => x == elem

    def union(s: FunSet, t: FunSet): FunSet = z => s(z) || t(z)

    def intersect(s: FunSet, t: FunSet): FunSet = z => s(z) && t(z)

    def diff(s: FunSet, t: FunSet): FunSet = z => s(z) && !t(z)

    def filter(s: FunSet, p: Int => Boolean): FunSet = z => s(z) && p(z)

    println(s"func3 contains 9 ${contains(func3, 9)}")
    println(s"func3 contains 9 ${contains(func3, 10)}")
    println(s"singletonSet=${contains(singletonSet(3), 3)}")
    println(s"singletonSet=${contains(singletonSet(3), 4)}")

    def uni(x: Int): Boolean = union(func3, func2)(x)

    def inter(x: Int): Boolean = intersect(func2, func3)(x)

    def df(x: Int): Boolean = diff(func2, func3)(x)

    def fi(x: Int, p: Int => Boolean): Boolean = filter(func2, p)(x)

    println(s"union 1=>${uni(1)}")
    println(s"union 2=>${uni(2)}")
    println(s"union 9=>${uni(9)}")
    println(s"union 8=>${uni(8)}")
    println(s"union 6=>${uni(6)}")
    println(s"union 13=>${uni(13)}")

    println(s"intersect 1=>${inter(1)}")
    println(s"intersect 6=>${inter(6)}")
    println(s"intersect 9=>${inter(9)}")
    println(s"intersect 4=>${inter(4)}")
    println(s"intersect 13=>${inter(13)}")

    println(s"diff 4 true=>${df(4)}")
    println(s"diff 6 false=>${df(6)}")
    println(s"diff 10 true=>${df(10)}")
    println(s"diff 9 false=>${df(9)}")

    filter(func2, func3)
    println(s"filter 1 false=>${fi(1, func3)}")
    println(s"filter 6 true =>${fi(6, func3)}")
    println(s"filter 9 false =>${fi(9, func3)}")
    println(s"filter 4 =>${fi(4, func3)}")
    println(s"filter 13=>${fi(13, func3)}")

    // -1000 to 1000
    def forall(s: FunSet, p: Int => Boolean): Boolean = {
      def iter(a: Int): Boolean = {
        //        println(s"iter=$a s(a)=${s(a)} p(a)=${p(a)} res=${s(a) && !p(a)}")
        if (a == 1000) true
        else if (s(a) && !p(a)) false
        else iter(a + 1)
      }

      iter(-1000)
    }

    def exists(s: FunSet, p: Int => Boolean): Boolean = {
      def iter(a: Int): Boolean = {
        if (a == 1000) false
        else if (s(a) && p(a)) true
        else iter(a + 1)
      }

      iter(-1000)
    }

    def generalize(f: Int => Int, agr: (Int, Int) => Int, init: Int)(a: Int, b: Int): Int = {
      if (a > b) init
      else agr(f(a), generalize(f, agr, init)(a + 1, b))
    }

    def productNew3(f: Int => Int)(a: Int, b: Int): Int = generalize(f, (y, z) => y * z, 1)(a, b)

    val s1 = singletonSet(2)
    val s2 = singletonSet(4)
    val s3 = singletonSet(6)

    def forAllTest: FunSet = union(union(s1, s2), s3)

    println("--------------------------")
    println(s"forall positive = ${forall(forAllTest, x => x % 2 == 0)}")
    println(s"forall negative = ${forall(forAllTest, x => x % 3 == 0)}")
    println("--------------------------")
    println(s"exists positive = ${exists(forAllTest, x => x % 6 == 0)}")
    println(s"exists negative = ${exists(forAllTest, x => x % 5 == 0)}")

    def map(s: FunSet, f: Int => Int): FunSet = {
      def funcReverse(x: Int, f: Int => Int): FunSet = y => x == f(y)
      x => forall(funcReverse(x, f), s)
    }

    def func55(x: Int): Boolean = x % 5 == 0

    println(map(func55, x => x + 1)(16))
    println(map(func55, x => x + 1)(15))
    println(map(func55, x => x + 1)(11))
    println(map(func55, x => x + 1)(10))
    println(map(func55, x => x + 1)(9))
    println(map(func55, x => x + 1)(8))

    //5,10,15,20,25,30 ... x => x%5 == 0
    //6,11,16,21 ... x => x + 1
    //s(frev(x))
    //y=f(x), x=fr(y)
    //exists

    //    def someFunc():Int =>Int = z => z -1

  }
}
