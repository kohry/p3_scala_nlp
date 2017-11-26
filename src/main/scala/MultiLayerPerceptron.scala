class MultiLayerPerceptron(config: BaseConfig, inputCount: Int, outputCount: Int) {

  //계층당 노드 개수를 담는 배열
  val topology: Array[Int] = Array[Int](inputCount) ++ config.hiddenLayerCount ++ Array[Int](outputCount);
  val layers: Array[Layer] = topology.zipWithIndex.map(t => Layer(t._2, t._1 + 1))
  val connections: Array[Connection] = Range(0, layers.size - 1).map{n => new Connection(config, layers(n), layers(n+1))}.toArray

}