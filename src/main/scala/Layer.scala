//네트워크 상의 위치와, 내부에 들어있는 노드의 개수
class Layer(val id: Int, val length: Int) {
  val output = new Array(length)
  val delta = new Array(length)
  val labels = new Array(length)



  //calculating Error Sum of Squares
  def sse(labels: Array[Double]): Double = {
    var sse = 0.0

    output.drop(1).zipWithIndex.foreach{ outputNode =>
      val err = labels(outputNode._2) - outputNode._1
      delta.update(outputNode._2 + 1, outputNode._1 * (1.0 - outputNode._1) * err)
      sse += err * err
    }
    sse * 0.5
  }




}
