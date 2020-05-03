package example.scala_ex

object ScalaEx8 {
  def main(args: Array[String]): Unit = {


    val fruit = List("apples", "oranges", "pears")
    println(fruit)
    val nums = List(1, 2, 3, 4)
    println(nums)
    val diag3 = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
    println(diag3)
    val empty = List()
    println(empty)
    val listAny = List(1, "apples", true)
    println(listAny)
    val cities: List[String] = List("Kyiv", "Kharkiv", "Lviv")
    val listOfLists: List[List[Int]] = List(List(1, 0, 0), List(0, 1, 0), List(0, 0, 1))
    val fruitsRec = "apples" :: ("oranges" :: ("pears" :: Nil))
    println(fruitsRec)
    val fruitsRec2 = "apples" :: "oranges" :: "pears" :: Nil
    println(fruitsRec2)
    val fruitsRec3 = Nil.::("pears").::("oranges").::("apples")
    println(fruitsRec3)


    val nums2 = List(1, 2, 3, 4)
    val result = nums2 match {
      case 1 :: 2 :: xs => println("Lists of `Int` that starts with `1` and then `2`"); 11
      case x :: Nil => println("Lists of length 1"); 12
      case List(x) => println("Same as `x :: Nil`"); 13
      case List() => println("The empty list, same as `Nil`"); 14
      case 3 :: xs => println("A list that starts with 3"); 15
    }
    println(result)


    val cond: (Int, Int) => Boolean = { (x, y) => x < y }

    def insert(x: Int, xs: List[Int]): List[Int] = {
//      println(s"insert x=$x xs=$xs")
      xs match {
        case List() => x :: List()
        case y :: ys =>{
//          println(s"compare x=$x y=$y")
          if (cond(x, y)) x :: y :: ys
          else y :: insert(x, ys)
        }
      }
    }

    def insertionSort(xs: List[Int]): List[Int] = xs match {
      case List() => List()
      case y :: ys => {
//        println(s"insertionSort y=$y ys=$ys")
        insert(y, insertionSort(ys))
      }
    }

    def checkDots2(xs: List[Int]):Unit = {
      xs match {
        case y :: ys => println(s"y=$y ys=$ys")
      }
    }

    println(List(2, 31, 6, 11))
    //checkDots2(List(2, 31, 6, 11))
    println(insertionSort(List(2, 31, 6, 11)))
    //insert
    println(insert(2, 1 :: 3 :: Nil))
    println(insert(1, 2 :: 3 :: Nil))
    println(insert(3, 1 :: 2 :: Nil))


  }
}
