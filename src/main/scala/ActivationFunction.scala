object ActivationFunction {
  def softmax(input: Array[Double]) : Array[Double] = {
    val numerator = input.map(Math.exp(_))
    val denominator = numerator.sum
    return numerator.map( _ / denominator).drop(1) //편향은 필요없음.
  }
}
