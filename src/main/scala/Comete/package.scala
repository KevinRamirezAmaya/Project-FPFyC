package object Comete {
  type DistributionValues= Vector[Double]
  type Frequency = Vector[Double]
  type MedidaPol = Distribution => Double
  type PolMeasure = Distribution => Double
  type Distribution = (Frequency, DistributionValues)

  def listaPuntos(punto: Double, min: Double, max: Double): List[Double] = {
    if (min > max) Nil
    else min :: listaPuntos(punto, min + punto, max)
  }

  def min_p(f: Double => Double, min: Double, max: Double, prec: Double): Double = {
    if (max - min < prec) {
      (min + max) / 2
    } else {
      val punto = (max - min) / 10
      val puntos = listaPuntos(punto, min, max)
      val minPoint1 = puntos.map(x => (f(x), x))
      val minPoint = minPoint1.minBy(_._1)
      if (minPoint._2 + punto > max || minPoint._2 - punto < min) {
        minPoint._2
      }
      else {
        min_p(f, minPoint._2 - punto, minPoint._2 + punto, prec)
      }
    }
  }

  def rhoCMTGen(alpha: Double, beta: Double): MedidaPol = {
    distribution =>
      val (frequencies, values) = distribution

      // Función auxiliar rhoAux que calcula ρΠ_aux(p)
      def rhoAux(p: Double): Double = {
        frequencies.zip(values).map {
          case (pi, yi) => Math.pow(pi, alpha) * Math.pow(Math.abs(yi - p), beta)
        }.sum
      }
      // Usa min_p para encontrar el valor mínimo de rhoAux en el rango [0, 1]
      min_p(rhoAux, 0.0, 1.0, 0.01)
  }

}
