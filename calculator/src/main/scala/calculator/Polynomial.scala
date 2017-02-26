package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] =
    Signal(Math.pow(b(), 2.0) - (4 * a() * c()))

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal {
      val (aa, bb, dd) = (a(), b(), delta())
      if (dd < 0) Set()
      else Set( (-bb + math.sqrt(dd)) / (2 * aa),
                (-bb - math.sqrt(dd)) / (2 * aa))}
}
