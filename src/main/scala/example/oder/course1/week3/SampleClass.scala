package example.oder.course1.week3

class SampleClass (x:Int) extends SampleParentClass with SampleTrait1 with SampleTrait2 {
  override def implementMe(x: Int): String = s"Hello from ${x}"

}
