class BaseConfig (
  val alpha: Double,
  val eta: Double,
  val hiddenLayers: Array[Int],
  val epochCount: Int,
  val eps: Double,
  val activation: Double => Double
)
