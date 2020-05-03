package example.scala_ex

object ScalaEx11 {

  case class Note(name: String, duration: String, octave: Int)

  def main(args: Array[String]): Unit = {
    val c3 = Note("C", "Quarter", 3)
    println(c3.toString)

  }
}
