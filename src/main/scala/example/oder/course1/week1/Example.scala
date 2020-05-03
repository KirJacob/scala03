package example.oder.course1.week1

object Example extends App {

  def printHello(message: String, from: String): Unit = {
    println(s"$message from $from")
  }

  println("Hello world from Scala Object!")
  printHello("Hello everyone", "method in main method")
}
