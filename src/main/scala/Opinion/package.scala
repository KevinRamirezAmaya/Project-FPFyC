import Comete._

import scala.collection.parallel.CollectionConverters._

package object Opinion {
  type SpecificBelief = Vector[Double]
  //type GenericBeliefConf = Int => SpecificBelief// no se esta empleando
  //type GenericBelief =  Int => SpecificBeliefConf// este type lo sugiere el proyecto en la paguina 5 pero en la estructura del paquete opinion ya no aparese
  type AgentsPolMeasure = (SpecificBelief, DistributionValues) => Double


  // Función de creencia uniforme
  def uniformBelief(nags: Int): SpecificBelief = {
    Vector.tabulate(nags)((i : Int) => (i + 1).toDouble / nags.toDouble)
  }

  // Función de creencia levemente polarizada
  def midlyBelief(nags: Int): SpecificBelief = {
    val middle = nags / 2
    Vector.tabulate(nags)((i : Int) =>
      if (i < middle) math.max(0.25 - 0.01 * (middle - i - 1), 0)
      else math.min(0.75 - 0.01 * (middle - i), 1))
  }

  // Función de creencia extremadamente polarizada
  def allExtremeBelief(nags: Int): SpecificBelief = {
    val middle = nags / 2
    Vector.tabulate(nags)((i : Int) =>
      if (i < middle) 0.0 else 1.0)
  }

  // Función de creencia de tres polos
  def allTripleBelief(nags: Int): SpecificBelief = {
    val oneThird = nags / 3
    val twoThird = (nags / 3) * 2
    Vector.tabulate(nags)((i : Int) =>
      if (i < oneThird) 0.0
      else if (i >= twoThird) 1.0
      else 0.5)
  }

  // Función de creencia de consenso
  def consensusBelief(b: Double)(nags: Int): SpecificBelief = {
    Vector.tabulate(nags)((i : Int) => b)
  }

  // Función rho que representa la medida de polarización entre agentes en proceso de creaciòn dado que no pasa las pruebas
  def rho(alpha: Double, beta: Double): AgentsPolMeasure = {
    (agentes: SpecificBelief, distribucion: DistributionValues) => {
      val k = distribucion.length - 1

      val intervalos = distribucion.zipWithIndex.map { case (_, i) =>
        if (i == 0) List(distribucion.head, distribucion.tail.head / 2)
        else if (0 < i && i < k) List(
          (distribucion.drop(i).head + distribucion.drop(i - 1).head) / 2,
          (distribucion.drop(i).head + distribucion.drop(i + 1).head) / 2
        )
        else List(
          (distribucion.drop(k - 1).head + distribucion.drop(k).head) / 2,
          distribucion.drop(k).head
        )
      }

      val agrupados = intervalos.map { interval =>
        val min = interval.head
        val max = interval.last
        if (intervalos.indexOf(interval) < k)
          agentes.filter(x => min <= x && x < max)
        else
          agentes.filter(x => min <= x && x <= max)
      }

      val pi_b = agrupados.map { grupo =>
        if (grupo.isEmpty) 0.0
        else grupo.length.toDouble / agentes.length.toDouble
      }.toVector

      val distribution: Distribution = (pi_b, distribucion)
      val rho_b = rhoCMT_Gen(alpha, beta)
      val norm: MedidaPol = normalizar(rho_b)

      norm(distribution)
    }
  }

  // Tipo de datos de grafo ponderado para evaluar la evolución de la opinión en una red
  type WeightedGraph = (Int, Int) => Double
  type SpecificWeightedGraph = (WeightedGraph, Int)
  //type GenericWeightedGraph = Int => SpecificWeightedGraph// no se esta empleando

  // Función de influencia tipo 1 pagina 7
  // def i1(nags: Int): SpecificWeightedGraph = {
  // ((i: Int, j: Int) => if (i == j) 1.0 else if (i < j) 1.0 / (j - i).toDouble else 0.0, nags)
  // }

  // Función de influencia tipo 2
  //def i2(nags: Int): SpecificWeightedGraph = {
  //((i: Int, j: Int) => if (i == j) 1.0 else if (i < j) (j - i).toDouble / nags.toDouble else (nags - (i - j)).toDouble / nags.toDouble, nags)
  //}

  // Tipo de función para actualizar creencias basada en un grafo ponderado específico
  type FunctionUpdate = (SpecificBelief, SpecificWeightedGraph) => SpecificBelief//el proyecto sugiere SpecificBeliefConf

  // Función para actualizar creencias con sesgo de confirmación
  def confBiasUpdate(b: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {//el proyecto sugiere SpecificBeliefConf
    val (influencias, agentes) = swg

    (0 until agentes).map { i =>
      val a = (0 until agentes).filter(j => influencias(j, i) > 0)

      if (a.isEmpty) b(i)
      else {
        val actualizacion = a.map { j =>
          val beta = 1 - math.abs(b(j) - b(i))
          beta * influencias(j, i) * (b(j) - b(i))
        }.sum / a.length
        b(i) + actualizacion
      }
    }.toVector
  }
  // Función para mostrar el grafo ponderado
  def showWeightedGraph(swg: SpecificWeightedGraph): IndexedSeq[IndexedSeq[Double]] = {
    val (graph, n) = swg
    IndexedSeq.tabulate(n, n)((i, j) => graph(i, j))
  }

  // Función para simular la evolución de la opinión
  //type FunctionUpdate = (SpecifieBeliefConf, SpecificWeightedGraph ) => SpecifieBeliefConf // este type lo sugiere el proyecto en la paguina 8 para crear la funciòn simulate
  def simulate(fu: FunctionUpdate, swg: SpecificWeightedGraph, b0: SpecificBelief, t: Int): IndexedSeq[SpecificBelief] = {
    def iterar(tiempo: Int, belief: SpecificBelief, acc: IndexedSeq[SpecificBelief]): IndexedSeq[SpecificBelief] = {
      if (tiempo <= 0) acc
      else {
        val nuevoBelief = fu(belief, swg)
        iterar(tiempo - 1, nuevoBelief, acc :+ nuevoBelief)
      }
    }
    iterar(t, b0, IndexedSeq(b0))
  }

  // Versión paralela de rho que representa la medida de polarización entre agentes
  def rhoPar(alpha: Double, beta: Double): AgentsPolMeasure = {
    (specificBelief: SpecificBelief, distributionValues: DistributionValues) => {
      val numAgents = specificBelief.length
      val k = distributionValues.length

      val firstInterval = (0.0, distributionValues(1)/2)
      val middleIntervals = (1 to k-2).par.map(i =>
        ((distributionValues(i)+distributionValues(i-1))/2, (distributionValues(i)+distributionValues(i+1))/2)).toVector
      val lastInterval = ((distributionValues(k-2)+1)/2, 1.0)

      val intervals = firstInterval +: middleIntervals :+ lastInterval

      val emptyClassification = (0 until k).map(i => i -> Vector.empty[Double]).toMap
      val classification = specificBelief.par.groupBy(a => intervals.zipWithIndex.indexWhere {
        case ((start, end), i) =>
          if(i == k-1) (start <= a && a <= end)
          else (start <= a && a < end)
      })
      val finalClassification = (emptyClassification ++ classification).toSeq.sortBy(_._1)

      val frequency = finalClassification.map{ case (i, values) => values.knownSize.toDouble/numAgents}.toVector

      val rhoAux = rhoCMT_Gen(alpha, beta)
      val normalizarAux = normalizar(rhoAux)
      normalizarAux((frequency, distributionValues))
    }
  }
  def confBiasUpdate(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {
    val NoAgente = sb.size // Número de agentes
    val InfluenciaAgentes = swg._1  // Función de influencia entre agentes

    // Calcula la nueva creencia para un agente
    def actualizarCreencia(i: Int): Double = {
      val influencias = (0 until NoAgente).map { j =>
        val diferencia = sb(j) - sb(i) // Diferencia en creencias
        val sesgo = 1 - math.abs(diferencia) // Sesgo de confirmación basado en proximidad
        sesgo * InfluenciaAgentes(j, i) * diferencia // Contribución del agente j a i
      }
      val sumaInfluencias = influencias.sum
      sb(i) + sumaInfluencias / (i + 1) // Ajusta la creencia normalizando por el índice
    }
    // Aplica la actualización a cada agente
    sb.indices.map(actualizarCreencia).toVector
  }



  // Versión paralela de confBiasUpdate

}