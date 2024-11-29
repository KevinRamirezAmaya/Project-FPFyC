import Comete._
import Opinion._
import common._
import Benchmark._

// Definición de vectores de frecuencias y distribución con una escala de Likert5
val pi_max = Vector(0.5, 0.0, 0.0, 0.0, 0.5)
val pi_min = Vector(0.0, 0.0, 1.0, 0.0, 0.0)
val pi_der = Vector(0.4, 0.0, 0.0, 0.0, 0.6)
val pi_izq = Vector(0.6, 0.0, 0.0, 0.0, 0.4)
val pi_int1 = Vector(0.0, 0.5, 0.0, 0.5, 0.0)
val pi_int2 = Vector(0.25, 0.0, 0.5, 0.0, 0.25)
val pi_int3 = Vector(0.25, 0.25, 0.0, 0.25, 0.25)
val pi_cons_centro = pi_min
val pi_cons_der = Vector(0.0, 0.0, 0.0, 0.0, 1.0)
val pi_cons_izq = Vector(1.0, 0.0, 0.0, 0.0, 0.0)

val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)

// Generación de la medida de polarización
val cmt1 = rhoCMT_Gen(1.2, 1.2)

cmt1(pi_max,likert5)
cmt1(pi_min,likert5)
cmt1(pi_der,likert5)
cmt1(pi_izq,likert5)
cmt1(pi_int1,likert5)
cmt1(pi_int2,likert5)
cmt1(pi_int3,likert5)
cmt1(pi_cons_centro,likert5)
cmt1(pi_cons_der,likert5)
cmt1(pi_cons_izq,likert5)

// Generación de la medida de polarización normalizada
val cmt1_norm = normalizar(cmt1)

cmt1_norm(pi_max, likert5)
cmt1_norm(pi_min, likert5)
cmt1_norm(pi_der, likert5)
cmt1_norm(pi_izq, likert5)
cmt1_norm(pi_int1, likert5)
cmt1_norm(pi_int2, likert5)
cmt1_norm(pi_int3, likert5)
cmt1_norm(pi_cons_centro, likert5)
cmt1_norm(pi_cons_der, likert5)
cmt1_norm(pi_cons_izq, likert5)

// Estados de creencias específicas; prueba de 100 agentes pagina 5
val sb_ext = allExtremeBelief(100)
val sb_cons = consensusBelief(0.2)(100)
val sb_unif = uniformBelief(100)
val sb_triple = allTripleBelief(100)
val sb_midly = midlyBelief(100)

// Pruebas de la función rho del paquete Opinion de acuerdo a las creencias definidas pagina 6

val rho1 = rho(1.2, 1.2)
val rho2 = rho(2.0, 1.0)

// Vectores de distribución
val dist1: Vector[Double] = Vector(0.0, 0.25, 0.50, 0.75, 1.0)
val dist2: Vector[Double] = Vector(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)

// Pruebas con redondeo explícito
val rho1 = rho(1.2, 1.2)
val rho2 = rho(2.0, 1.0)

val dist1 = Vector(0.0, 0.25, 0.50, 0.75, 1.0)
val dist2 = Vector(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)

rho1(sb_ext, dist1)
rho2(sb_ext, dist1)
rho1(sb_ext, dist2)
rho2(sb_ext, dist2)

rho1(sb_cons, dist1)
rho2(sb_cons, dist1)
rho1(sb_cons, dist2)
rho2(sb_cons, dist2)

rho1(sb_unif, dist1)
rho2(sb_unif, dist1)
rho1(sb_unif, dist2)
rho2(sb_unif, dist2)

rho1(sb_triple, dist1)
rho2(sb_triple, dist1)
rho1(sb_triple, dist2)
rho2(sb_triple, dist2)

rho1(sb_midly, dist1)
rho2(sb_midly, dist1)
rho1(sb_midly, dist2)
rho2(sb_midly, dist2)

// Función de influencia tipo 1 pagina 7, las ubique a aca pero toca definir donde quedaran aca o en opinion
def i1(nags: Int): SpecificWeightedGraph = {
  ((i: Int, j: Int) => if (i == j) 1.0 else if (i < j) 1.0 / (j - i).toDouble else 0.0, nags)
}

// Función de influencia tipo 2
def i2(nags: Int): SpecificWeightedGraph = {
  ((i: Int, j: Int) => if (i == j) 1.0 else if (i < j) (j - i).toDouble / nags.toDouble else (nags - (i - j)).toDouble / nags.toDouble, nags)
}

// Creencias definidas mediante la (función showWeightedGraph) que crea grafos de influencia

val i1_10 = i1(10)
val i2_10 = i2(10)
val i1_20 = i1(20)
val i2_20 = i2(20)
showWeightedGraph(i1_10)
showWeightedGraph(i2_10)
//showWeightedGraph(i1_20)
//showWeightedGraph(i2_20)

// Pruebas para la función confBiasUpdate
val sbu_10 = uniformBelief(10)
confBiasUpdate(sbu_10 , i1_10)
rho1(sbu_10 , dist1)
rho1(confBiasUpdate(sbu_10 , i1_10),dist1)

val sbm_10 = midlyBelief(10)
confBiasUpdate(sbm_10, i1_10)
rho1(sbm_10, dist1)
rho1(confBiasUpdate(sbm_10, i1_10),dist1)

// Ejemplo para la función paralela confupdatePar

val sbu_10 = uniformBelief(10)
confBiasUpdatePar(sbu_10, i1_10)
rho1(sbu_10, dist1)
rho1(confBiasUpdatePar(sbu_10, i1_10), dist1)

val sbm_10 = midlyBelief(10)
confBiasUpdatePar(sbm_10, i1_10)
rho1(sbm_10, dist1)
rho1(confBiasUpdatePar(sbm_10, i1_10), dist1)

// Pruebas para la función simulate
// Ejecución de la simulación con la función de actualización confBiasUpdate y 2 unidades de tiempo para Sbu_10
val simulacionSbu10 = for {
  b <- simulate(confBiasUpdate, i1_10, sbu_10, 2)
} yield (b, rho1(b, dist1))

// Ejecución de la simulación con la función de actualización confBiasUpdate y 2 unidades de tiempo para sbm_10
val simulacionSbm10 = for {
  b <- simulate(confBiasUpdate, i1_10, sbm_10, 2)
} yield (b, rho1(b, dist1))


// Pruebas versiòn paralela de la función rhoPar del paquete Opinion
val rho3 = rhoPar(1.2, 1.2)
val rho4 = rhoPar(2.0, 1.0)
rho3(sb_ext, dist1)
rho4(sb_ext, dist1)
rho3(sb_ext, dist2)
rho4(sb_ext, dist2)

rho3(sb_cons, dist1)
rho4(sb_cons, dist1)
rho3(sb_cons, dist2)
rho4(sb_cons, dist2)

rho3(sb_unif, dist1)
rho4(sb_unif, dist1)
rho3(sb_unif, dist2)
rho4(sb_unif, dist2)

rho3(sb_triple, dist1)
rho4(sb_triple, dist1)
rho3(sb_triple, dist2)
rho4(sb_triple, dist2)

rho3(sb_midly, dist1)
rho4(sb_midly, dist1)
rho3(sb_midly, dist2)
rho4(sb_midly, dist2)

// Evaluacion comparativa de la vercion secuencial y concurrente pagina de rho y rhoPar
//Caso suministrado por el profesor
val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)
val sbms = for {
  n <- 2 until 18
  nags = math.pow(2, n).toInt
} yield midlyBelief(nags)

val polSec = rho(1.2, 1.2)
val polPar = rhoPar(1.2, 1.2)

val cmp1 = compararMedidasPol(sbms, likert5, polSec, polPar)
val tiempos = cmp1.map { case (_, _, _, t1, t2, _) => (t1, t2) }
val resultados = cmp1 .map( t => t ._6)

//Caso con medidas de polarizacion Uniforme
val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)

val sbu = for {
  n <- 2 until 18
  nags = math.pow(2, n).toInt
} yield uniformBelief(nags)

val polSec = rho(1.5, 1.5)
val polPar = rhoPar(1.5, 1.5)
val cmp1 = compararMedidasPol(sbu, likert5, polSec, polPar)
//esta funcion crea una tupla con los tiempos de polSeq y polPar
val tiempos = cmp1.map { case (_, _, _, t1, t2, _) => (t1, t2) }
val resultados = cmp1 .map( t => t ._6)

//caso con medidas de polarizacion extremas
val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)
val sbae = for {
  n <- 2 until 18
  nags = math.pow(2, n).toInt
} yield allExtremeBelief(nags)
val polSec = rho(1.6, 1.4)
val polPar = rhoPar(1.6, 1.4)
val cmp1 = compararMedidasPol(sbae, likert5, polSec, polPar)
//esta funcion crea una tupla con los tiempos de polSeq y polPar
val tiempos = cmp1.map { case (_, _, _, t1, t2, _) => (t1, t2) }
val resultados = cmp1 .map( t => t ._6)

//Caso con las medidas de polarizacion Triple
val likert5 = Vector(0.0, 0.25, 0.5, 0.75, 1.0)
val sbat = for {
  n <- 2 until 18
  nags = math.pow(2, n).toInt
} yield allTripleBelief(nags)
val polSec = rho(1.75, 1.21)
val polPar = rhoPar(1.75, 1.21)
val cmp1 = compararMedidasPol(sbat, likert5, polSec, polPar)
//esta funcion crea una tupla con los tiempos de polSeq y polPar
val tiempos = cmp1.map { case (_, _, _, t1, t2, _) => (t1, t2) }
val resultados = cmp1 .map( t => t ._6)



//Desde aqui encontramos las pruebas de rendimiento de confbiasUpdate vs confbiasUpdatePar
// el valor de i2 representa la cantidad de agentes
// Ejemplo confUpdatePar pagina 10
val i1_16384 = i1(16384)
val i2_16384 = i2(16384)
compararFuncionesAct(sbms.take(sbms.length/2),
  i2_16384, confBiasUpdate, confBiasUpdatePar)

val i1_32768 = i1(32768)
val i2_32768 = i2(32768)

compararFuncionesAct(sbms.take(sbms.length/2),
  i2_32768, confBiasUpdate, confBiasUpdatePar)

val i1_65536 = i1(65536)
val i2_65536 = i2(65536)

compararFuncionesAct(sbms.take(sbms.length/2),
  i2_65536, confBiasUpdate, confBiasUpdatePar)

val i1_131072 = i1(131072)
val i2_131072 = i2(131072)

compararFuncionesAct(sbms.take(sbms.length/2),
  i2_131072, confBiasUpdate, confBiasUpdatePar)

val i1_262144 = i1(262144)
val i2_262144 = i2(262144)

compararFuncionesAct(sbms.take(sbms.length/2),
  i2_262144, confBiasUpdate, confBiasUpdatePar)

val i1_524288 = i1(524288)
val i2_524288 = i2(524288)

compararFuncionesAct(sbms.take(sbms.length/2),
  i2_524288, confBiasUpdate, confBiasUpdatePar)

val i1_1048576 = i1(1048576)
val i2_1048576 = i2(1048576)

compararFuncionesAct(sbms.take(sbms.length/2),
  i2_1048576, confBiasUpdate, confBiasUpdatePar)
//
val sbms = for{
  n <- 2 until 16
  nags = math.pow(2, n).toInt
} yield midlyBelief(nags)

val sbes = for {
  n <-2 until 16
  nags = math.pow(2,n).toInt
} yield allExtremeBelief(nags)

val sbts = for {
  n <-2 until 16
  nags = math.pow(2,n).toInt
} yield allTripleBelief(nags)


//Ejemplo graficar y generar los archivos html
val evolsSec = for {
  i <- 0 until sbms.length
} yield simEvolucion(Seq(sbms(i),sbes(i),sbts(i)),
  i2_32768, 10, polSec, confBiasUpdate, likert5,
  "Simulacion_Secuencial_" ++ i.toString ++ "_"
    ++ sbms(i).length.toString)