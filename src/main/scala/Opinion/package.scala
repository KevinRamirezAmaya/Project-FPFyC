import Comete._
import scala.collection.parallel.CollectionConverters._
import scala.math._
import common._

package object Opinion {
  type SpecificBelief = Vector[Double]
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

      // Paso 1: Generar intervalos de la distribución
      val intervalos = distribucion.zipWithIndex.map { case (_, i) =>
        if (i == 0)
          List(distribucion.head, distribucion.tail.head / 2) // Intervalo para el primer punto
        else if (0 < i && i < k)
          List(
            (distribucion.drop(i).head + distribucion.drop(i - 1).head) / 2, // Límite inferior
            (distribucion.drop(i).head + distribucion.drop(i + 1).head) / 2  // Límite superior
          )
        else
          List(
            (distribucion.drop(k - 1).head + distribucion.drop(k).head) / 2, // Límite inferior del último intervalo
            distribucion.drop(k).head // Último punto
          )
      }

      // Paso 2: Agrupar agentes según los intervalos calculados
      val agrupados = intervalos.map { interval =>
        val min = interval.head
        val max = interval.last
        if (intervalos.indexOf(interval) < k)
          agentes.filter(x => min <= x && x < max) // Excluye el límite superior para intervalos intermedios
        else
          agentes.filter(x => min <= x && x <= max) // Incluye el límite superior en el último intervalo
      }

      // Paso 3: Calcular las frecuencias pi_b
      val pi_b = agrupados.map { grupo =>
        if (grupo.isEmpty) 0.0 // Si no hay agentes en el intervalo, frecuencia es 0
        else grupo.length.toDouble / agentes.length.toDouble // Proporción de agentes en el intervalo
      }.toVector

      // Paso 4: Construir la distribución de frecuencias y valores
      val distribution: Distribution = (pi_b, distribucion)

      // Paso 5: Calcular la medida de polarización con rhoCMT_Gen y normalizarla
      val rho_b = rhoCMT_Gen(alpha, beta) // Generar función de polarización con parámetros alpha y beta
      val norm: MedidaPol = normalizar(rho_b) // Normalizar la medida de polarización

      // Paso 6: Retornar el valor de la medida normalizada para la distribución dada
      norm(distribution)
    }
  }


  // Tipo de datos de grafo ponderado para evaluar la evolución de la opinión en una red
  type WeightedGraph = (Int, Int) => Double
  type SpecificWeightedGraph = (WeightedGraph, Int)

  // Tipo de función para actualizar creencias basada en un grafo ponderado específico
  type FunctionUpdate = (SpecificBelief, SpecificWeightedGraph) => SpecificBelief//el proyecto sugiere SpecificBeliefConf

  // Función para actualizar creencias con sesgo de confirmación
  def confBiasUpdate(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {
    val NoAgente = sb.size // Número de agentes
    val InfluenciaAgentes = swg._1 // Matriz de influencias entre agentes

    // Calcula la nueva creencia optimizando el cálculo y aplicando truncamiento
    def actualizarCreencia(i: Int): Double = {
      // Filtra y calcula influencias en una sola pasada para agentes relevantes
      val sumaInfluencias = (0 until NoAgente).iterator.filter(j => InfluenciaAgentes(j, i) > 0).map { j =>
        val diferencia = sb(j) - sb(i)
        val sesgo = 1 - math.abs(diferencia)
        sesgo * InfluenciaAgentes(j, i) * diferencia
      }.sum

      // Calcula la nueva creencia normalizando por el número de agentes influyentes
      val nuevaCreencia = if (sumaInfluencias != 0) sb(i) + sumaInfluencias / NoAgente else sb(i)

      // Truncar a 4 decimales
      BigDecimal(nuevaCreencia).setScale(4, BigDecimal.RoundingMode.DOWN).toDouble
    }

    // Aplica la actualización a cada agente utilizando índices para evitar búsquedas repetidas
    sb.indices.map(actualizarCreencia).toVector
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
    (agentes: SpecificBelief, distribucion: DistributionValues) => {
      val k = distribucion.length - 1
      val intervalos = distribucion.zipWithIndex.par.map { case (_, i) =>
        if (i == 0) {
          List(distribucion.head, distribucion.tail.head / 2)  // Primer intervalo
        } else if (0 < i && i < k) {
          List(
            (distribucion(i) + distribucion(i - 1)) / 2,
            (distribucion(i) + distribucion(i + 1)) / 2
          )
        } else {
          List(
            (distribucion(k - 1) + distribucion(k)) / 2,
            distribucion(k)
          )
        }
      }.toList

      val agrupados = intervalos.par.map { interval =>
        val min = interval.head
        val max = interval.last
        if (intervalos.indexOf(interval) < k) {
          agentes.filter(x => min <= x && x < max)
        } else {
          agentes.filter(x => min <= x && x <= max)
        }
      }.toList

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



  def confBiasUpdatePar(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {
    val NoAgente = sb.size // Número de agentes
    val InfluenciaAgentes = swg._1 // Matriz de influencias entre agentes

    // Calcula la nueva creencia optimizando el cálculo y aplicando truncamiento
    def actualizarCreencia(i: Int): Double = {
      val sumaInfluencias = (0 until NoAgente).iterator.filter(j => InfluenciaAgentes(j, i) > 0).map { j =>
        val diferencia = sb(j) - sb(i)
        val sesgo = 1 - math.abs(diferencia)
        sesgo * InfluenciaAgentes(j, i) * diferencia
      }.sum

      val nuevaCreencia = if (sumaInfluencias != 0) sb(i) + sumaInfluencias / NoAgente else sb(i)

      // Truncar a 4 decimales
      BigDecimal(nuevaCreencia).setScale(4, BigDecimal.RoundingMode.DOWN).toDouble
    }

    // Divide el trabajo en dos mitades y las ejecuta en paralelo
    val (left, right) = sb.splitAt(sb.length / 2)

    val (resLeft, resRight) = parallel(
      left.indices.par.map(actualizarCreencia).toVector,
      right.indices.par.map(i => actualizarCreencia(i + left.length)).toVector
    )

    resLeft ++ resRight
  }
}