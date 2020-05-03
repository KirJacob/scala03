package example.scala_ex

object ScalaEx2 {
  def main(args: Array[String]): Unit = {
    val c3 = Note("C", "Quarter", 3)
    println(c3.name, c3.duration, c3.octave)

    val symbol1: Symbol = Note("D", "Quarter", 4)
    val symbol2: Symbol = Rest("Whole")
    val symbol3: Symbol = Note("D", "Quarter", 4)

    println(symbolDuration(symbol1))
    println(symbolDuration(symbol2))
    println(symbol1 == symbol2)
    println(symbol1 == symbol3)

    val e = "a"
    val result = e match {
      case "a" => "letter A"
      case "b" => "letter B"
    }
    println(result)
    println(nonExhaustiveDuration(symbol2))

    println("------ fractionOfWhole -----")
    println(fractionOfWhole(Quarter))
    println(fractionOfWhole(Half))

  }

  def symbolDuration(symbol: Symbol): String =
    symbol match {
      case Note(name, duration, octave) => duration
      case Rest(duration) => duration
    }

  def nonExhaustiveDuration(symbol: Symbol): String =
    symbol match {
      case Rest(duration) => duration
    }

  sealed trait Symbol

  case class Note(name: String, duration: String, octave: Int) extends Symbol

  case class Rest(duration: String) extends Symbol


  sealed trait NoteName
  case object A extends NoteName
  case object B extends NoteName
  case object C extends NoteName
  case object D extends NoteName

  sealed trait Duration
  case object Whole extends Duration
  case object Half extends Duration
  case object Quarter extends Duration

  def fractionOfWhole(duration: Duration):Double =
    duration match {
      case Whole => 1.0
      case Half => 0.5
      case Quarter => 0.25
    }

}
