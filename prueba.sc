import Datos._
import Operaciones._

val itsAireCurso = itinerariosAire(vuelosCurso, aeropuertosCurso)
val itsAire1 = itsAireCurso("MID", "SVCS")
val itsAire2 = itsAireCurso("CLO", "SVCS")

val itsAire3 = itsAireCurso("CLO", "SVO")
val itsAire4 = itsAireCurso("CLO", "MEX")
val itsAire5 = itsAireCurso("CTG", "PTY")

