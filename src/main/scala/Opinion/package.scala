import Comete._
import common._
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
    (agentes: SpecificBelief, distribucion: DistributionValues) => {
      val k = distribucion.length - 1

      // Paralelización para construir los intervalos
      val intervalos = distribucion.zipWithIndex.par.map { case (_, i) =>
        if (i == 0) List(distribucion.head, distribucion.tail.head / 2)
        else if (0 < i && i < k) List(
          (distribucion.drop(i).head + distribucion.drop(i - 1).head) / 2,
          (distribucion.drop(i).head + distribucion.drop(i + 1).head) / 2
        )
        else List(
          (distribucion.drop(k - 1).head + distribucion.drop(k).head) / 2,
          distribucion.drop(k).head
        )
      }.toList // Convertir de nuevo a secuencia estándar

      // Paralelización para agrupar agentes en intervalos
      val agrupados = intervalos.par.map { interval =>
        val min = interval.head
        val max = interval.last
        if (intervalos.indexOf(interval) < k)
          agentes.filter(x => min <= x && x < max)
        else
          agentes.filter(x => min <= x && x <= max)
      }.toList

      // Paralelización para calcular las frecuencias pi_b
      val pi_b = agrupados.par.map { grupo =>
        if (grupo.isEmpty) 0.0
        else grupo.length.toDouble / agentes.length.toDouble
      }.toVector

      // Creación de la distribución y cálculo de polarización
      val distribution: Distribution = (pi_b, distribucion)
      val rho_b = rhoCMT_Gen(alpha, beta)
      val norm: MedidaPol = normalizar(rho_b)

      norm(distribution)
    }
  }

  // Versión paralela de confBiasUpdate
  def confBiasUpdatePar(sb: SpecificBelief, swg: SpecificWeightedGraph): SpecificBelief = {
    val (influencias, agentes) = swg

    (0 until agentes).par.map { i =>
      val a = (0 until agentes).par.filter(j => influencias(j, i) > 0)

      if (a.isEmpty) sb(i)
      else {
        val actualizacion = a.map { j =>
          val beta = 1 - math.abs(sb(j) - sb(i))
          beta * influencias(j, i) * (sb(j) - sb(i))
        }.sum / a.size
        sb(i) + actualizacion
      }
    }.toVector
  }

}
