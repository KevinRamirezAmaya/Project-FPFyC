package object Comete {
  type DistributionValues = Vector[Double]
  type Frequency = Vector[Double]
  type Distribution = (Frequency, DistributionValues)
  type MedidaPol = Distribution => Double


  // Encuentra el mínimo de una función f en el intervalo [min, max] con precisión prec
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
      val nuevoMin = math.max(min, minPoint._2 - punto)
      val nuevoMax = math.min(max, minPoint._2 + punto)
      min_p(f, nuevoMin, nuevoMax, prec)
    }
  }

  // Genera una medida de polarización parametrizada con alpha y beta
  def rhoCMT_Gen(alpha: Double, beta: Double): MedidaPol = {
    distribution =>
      val (frequencies, values) = distribution

      // Función auxiliar para calcular rho en un punto dado
      def rhoAux(p: Double): Double = {
        frequencies.zip(values).map {
          case (pi, yi) => Math.pow(pi, alpha) * Math.pow(Math.abs(yi - p), beta)
        }.sum
      }

      // Usamos min_p para encontrar el mínimo punto de polarización
      val min = min_p(rhoAux, 0.0, 1.0, 0.01)
      val resultado = rhoAux(min)

      // Si el valor es muy pequeño, lo redondeamos a 0; de lo contrario, redondeamos el resultado a 3 decimales
      if (Math.abs(resultado) < 1e-3) 0.0 else Math.round(resultado * 1000) / 1000.0
  }

  // Normaliza una medida de polarización ajustándola al intervalo [0,1]
  def normalizar(m: MedidaPol): MedidaPol = {
    // Calcula la polarización en el peor caso (50% en extremos izquierdo y derecho)
    val worstCasePolarization = m((Vector(0.5, 0.0, 0.0, 0.0, 0.5), Vector(0.0, 0.25, 0.5, 0.75, 1.0)))
    (distribution: Distribution) => {
      val polarization = m(distribution)
      polarization / worstCasePolarization
    }
  }
}