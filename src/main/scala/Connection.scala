import scala.util.Random



class Connection(config: BaseConfig, from: Layer, to: Layer) {
  val beta = 0.1
  val synapses = Array.tabulate(to.length) { n =>
    if (n > 0) Array.fill(from.length) {
      (beta * Random.nextDouble(), 0.0)
    }
  }

  def connectionForwardPropagation(): Unit = {
    val s = synapses.drop(1)
    val _output = s.map { x =>

      val dot = x.zip(from.output).foldLeft(0.0) { (s, xy) => s + xy._1._1 * xy._2 }
      if (!isOutLayer) config.activation(sum) else sum
    }

    val out = if (isOutLayer) mlpObjective(_output) else _output
    out.copyToArray(to.output, 1)

  }

  def backpropUpdate(): Unit = {
    Range(1, to.length).foreach { i =>
      val delta = to.delta(i)
      Range(0, from.length).foreach { j =>
        val _output = from.output(j)
        val oldSynapse = synapses(i)(j)
        val grad = config.eta * delta * _output
        val deltaWeight = grad + config.alpha * oldSynapse._2
        synapses(i)(j) = (oldSynapse._1 + deltaWeight, grad)
      }
    }
  }


  def backprop() = {
    Range(1, from.length).foreach { i =>
      val dot = Range(1, to.length).foldLeft(0.0) { (s, j) =>
        s + synapses(j)(i)._1 * to.delta(j)
      }
      from.delta(i) = from.output(i) * (1.0 - from.output(i)) * dot
    }
  }
}
