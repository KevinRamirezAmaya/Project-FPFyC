import scala.collection.immutable.Vector
import Comete._
import Opinion._
import common._


// Definición de vectores de frecuencias y distribución con una escala de Likert5
val pi_max =Vector(0.5, 0.0, 0.0, 0.0, 0.5)
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

// Pruebas de la función rho del paquete Opinion de acuerdo a las creencias definidas mediante el tipo GenericBelief pagina 6

// Función de redondeo a 3 decimales
def roundToThreeDecimals(value: Double): Double =
  BigDecimal(value).setScale(3, BigDecimal.RoundingMode.HALF_UP).toDouble

// Definir rho1 y rho2
val rho1 = rho(1.2, 1.2)
val rho2 = rho(2.0, 1.0)

// Vectores de distribución
val dist1: Vector[Double] = Vector(0.0, 0.25, 0.50, 0.75, 1.0)
val dist2: Vector[Double] = Vector(0.0, 0.2, 0.4, 0.6, 0.8, 1.0)

// Pruebas con redondeo explícito
val res20 = roundToThreeDecimals(rho1(sb_ext, dist1))//Double =  1.0 segun pagina 6
val res21 = roundToThreeDecimals(rho2(sb_ext, dist1))// =  1.0
val res22 = roundToThreeDecimals(rho1(sb_ext, dist2))// =  1.0
val res23 = roundToThreeDecimals(rho2(sb_ext, dist2))// =  1.0

val res24 = roundToThreeDecimals(rho1(sb_cons, dist1)) //Double =  0.0
val res25 = roundToThreeDecimals(rho2(sb_cons, dist1))
val res26 = roundToThreeDecimals(rho1(sb_cons, dist2))
val res27 = roundToThreeDecimals(rho2(sb_cons, dist2))

val res28 = roundToThreeDecimals(rho1(sb_unif, dist1))//Double =  0.38
val res29 = roundToThreeDecimals(rho2(sb_unif, dist1))// =  0.188
val res30 = roundToThreeDecimals(rho1(sb_unif, dist2))// =  0.377
val res31 = roundToThreeDecimals(rho2(sb_unif, dist2))// =  0.172

val res32 = roundToThreeDecimals(rho1(sb_triple, dist1))// = 0.677
val res33 = roundToThreeDecimals(rho2(sb_triple, dist1))// = 0.448
val res34 = roundToThreeDecimals(rho1(sb_triple, dist2))// = 0.617
val res35 = roundToThreeDecimals(rho2(sb_triple, dist2))// = 0.448

val res36 = roundToThreeDecimals(rho1(sb_midly, dist1))// = 0.784
val res37 = roundToThreeDecimals(rho2(sb_midly, dist1))// = 0.58
val res38 = roundToThreeDecimals(rho1(sb_midly, dist2))// = 0.773
val res39 = roundToThreeDecimals(rho2(sb_midly, dist2))// = 0.528

// Creencias definidas mediante la (función showWeightedGraph) que crea grafos de influencia

val i1_10 = i1(10)
val i2_10 = i2(10)
val i1_20 = i1(20)
val i2_20 = i2(20)
//showWeightedGraph(i1_10)
//showWeightedGraph(i2_10)
//showWeightedGraph(i1_20)
//showWeightedGraph(i2_20)

// Pruebas para la función confBiasUpdate
val sbu_10 = uniformBelief(10)
val sbm_10 = midlyBelief(10)

// Aplicación de la función de sesgo de confirmación en uniformBelief  los resultados no son los esperados
val updatedSbu10 = confBiasUpdate(sbu_10, i1_10)
val updatedSbm10 = confBiasUpdate(sbm_10, i1_10)

// Evaluación de polarización usando rho1 funciona correctamente
val rho1Result1 = rho1(sbu_10, dist1) // = 0.383
//val rho1Result2 = rho1(updatedSbu10, dist1) // = 0.38
val rho1Result3 = rho1(sbm_10, dist1) // = 0.435
//val rho1Result4 = rho1(updatedSbm10, dist1) // = 0.435

// Pruebas para la función simulate
// Ejecución de la simulación con la función de actualización confBiasUpdate y 2 unidades de tiempo resultados parcialmente buenos
val simulacionSbu10 = for {
  b <- simulate(confBiasUpdate, i1_10, sbu_10, 2)
} yield (b, rho1(b, dist1))

// Ejecución de la simulación con la función de actualización confBiasUpdate y 2 unidades de tiempo para sbm_10 resultados parcialmente buenos
val simulacionSbm10 = for {
  b <- simulate(confBiasUpdate, i1_10, sbm_10, 2)
} yield (b, rho1(b, dist1))

