import scala.util.Random

class Main {

  def main(args: Array[String]): Unit = {

    val topology = Array[Int](n)

    val layers = top


  }

}

class BaseConfig(val hiddenLayerCount: Array[Int])

class MultiLayerPerceptron(config: BaseConfig, inputCount: Int, outputCount: Int) {

  //계층당 노드 개수를 담는 배열
  val topology: Array[Int] = Array[Int](inputCount) ++ config.hiddenLayerCount ++ Array[Int](outputCount);
  val layers: Array[Layer] = topology.zipWithIndex.map(t => Layer(t._2, t._1 + 1))
  val connections: Array[Connection] = Range(0, layers.size - 1).map{n => new Connection(config, layers(n), layers(n+1))}.toArray

}

//네트워크 상의 위치와, 내부에 들어있는 노드의 개수
case class Layer(val id: Int, val length: Int) {
  val output = new Array(length)
  val delta = new Array(length)
}

//가중치와 가중치 조정값.
case class Synapse(weight: Double, delta: Double)

//
class Connection(config: BaseConfig, from: Layer, to: Layer) {
  val beta = 0.1
  val synapses = Array.tabulate(to.length){ n =>
    if (n > 0) Array.fill(from.length){ (beta * Random.nextDouble(), 0.0) }
  }

  def connectionForwardPropagation(): Unit = {
    val s = synapses.drop(1)
    val _output = s.map { x =>

      val dot = x.zip(from.output).foldLeft(0.0){ (s, xy) => s + xy._1._1 * xy._2 }
      if (!isOutLayer) config.activation(sum) else sum }

    val out = if(isOutLayer) mlpObjective(_output) else _output
    out.copyToArray(to.output, 1)

  }

}



