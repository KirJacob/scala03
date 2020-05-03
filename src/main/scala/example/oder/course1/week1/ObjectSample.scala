package example.oder.course1.week1

object ObjectSample {
  def x:Int = 10
}

object ObjectSample2 {
  import ObjectSample.x
  def y:Int = x

  def main(args: Array[String]): Unit = {
    println(y)
  }
}
