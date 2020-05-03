package example.oder.course1.week1

object ScClass {

  def printHello(message:String, from:String):Unit = {
    println(s"$message from $from")
  }

  def square(x: Double):Double = {
    x * x
  }

  def and(x: Boolean, y:Boolean):Boolean = {
    if (x) y else x
  }

  def or(x: Boolean, y:Boolean):Boolean = {
    if (!x) y else true
  }

  def main(args: Array[String]): Unit = {
    println("Hello world from Scala Object main method")
    println("What is the difference between Object and Class here")
    printHello("Hello everyone", "method in main method")
    val x = 5
    var y = 10
    val z = square(y)
    y = 15
    println(x, y, z)
    println(and(true,true))
    println(and(false,true))
    println(and(true,false))
    println(and(false,false))
  }

}
