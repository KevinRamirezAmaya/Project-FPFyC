import scala.collection.immutable.Vector
import Comete._

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

val cmt1 = rhoCMT_Gen(1.2, 1.2)

cmt1(pi_max,likert5(5))
cmt1(pi_min,likert5(5))
cmt1(pi_der,likert5(5))
cmt1(pi_izq,likert5(5))
cmt1(pi_int1,likert5)
cmt1(pi_int2,likert5)
cmt1(pi_int3,likert5)
cmt1(pi_cons_centro,likert5)
cmt1(pi_cons_der,likert5)
cmt1(pi_cons_izq,likert5)
