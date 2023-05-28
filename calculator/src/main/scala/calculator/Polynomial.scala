package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal[Double] {
      Math.pow(b(), 2) - 4 * a() * c()
    }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal[Set[Double]] {
      val deltaValue = delta()
      val aValue = a()
      val bValue = b()
      if deltaValue > 0 then
        Set((-bValue + Math.sqrt(deltaValue)) / (2 * aValue),
          (-bValue - Math.sqrt(deltaValue)) / (2 * aValue))
      else if deltaValue == 0 then
        Set(-bValue / (2 * aValue))
      else
        Set()
    }
