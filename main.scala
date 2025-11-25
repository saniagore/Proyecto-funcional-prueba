import Datos._
import Operaciones._

// Envuelve todo el código en un objeto
object Main extends App {

  // --- BLOQUE DE EJECUCIÓN ---

  println("--- Iniciando Pruebas de Itinerarios ---")

  val vuelosPrueba = vuelosCurso
  val aeropuertosPrueba = aeropuertosCurso

  // Definición de funciones a probar
  val itsEscalasPar = itinerariosEscalasPar(vuelosPrueba, aeropuertosPrueba)
  val itsEscalasSec = itinerariosEscalas(vuelosPrueba, aeropuertosPrueba)
  
  val itsAirePar = itinerariosAirePar(vuelosPrueba, aeropuertosPrueba)
  val itsAireSec = itinerariosAire(vuelosPrueba, aeropuertosPrueba)

  val itsTiempoPar = itinerariosTiempoPar(vuelosPrueba, aeropuertosPrueba)
  val itsTiempoSec = itinerariosTiempo(vuelosPrueba, aeropuertosPrueba)

  val pruebas = List(
    ("1", "MID", "SVCS"),
    ("2", "CLO", "SVCS"),
    ("3", "CLO", "SVO"),
    ("4", "CLO", "MEX"),
    ("5", "CTG", "PTY"),
    ("6", "MDE", "MEX"),
    ("7", "BOG", "SVO"),
    ("8", "CLO", "IST"),
    ("9", "IST", "SVO"),
    ("10", "HND", "BAQ"), // Long chain vs Shortcut
    ("11", "HND", "MAD"), // Multiple paths
    ("12", "DXB", "MIA")  // Sub-chain
  )

  def measureTime[T](block: => T): (T, Long) = {
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    (result, (end - start) / 1000000) // Convert to ms
  }

  var resultados = List[(String, String, String, String, String, String, String, String, String)]()

  for ((id, org, dst) <- pruebas) {
    println(s"\n--- PRUEBA $id: $org -> $dst ---")

    // Escalas
    val (escPar, tEscPar) = measureTime { itsEscalasPar(org, dst) }
    val (escSec, tEscSec) = measureTime { itsEscalasSec(org, dst) }
    println(s"Escalas (Par): $escPar ($tEscPar ms)")
    println(s"Escalas (Sec): $escSec ($tEscSec ms)")
    resultados = resultados :+ (id, org, dst, "Escalas", escPar.length.toString, escSec.length.toString, if (escPar == escSec) "SI" else "NO", tEscPar.toString, tEscSec.toString)

    // Aire
    val (airePar, tAirePar) = measureTime { itsAirePar(org, dst) }
    val (aireSec, tAireSec) = measureTime { itsAireSec(org, dst) }
    println(s"Aire (Par):    $airePar ($tAirePar ms)")
    println(s"Aire (Sec):    $aireSec ($tAireSec ms)")
    resultados = resultados :+ (id, org, dst, "Aire", airePar.length.toString, aireSec.length.toString, if (airePar == aireSec) "SI" else "NO", tAirePar.toString, tAireSec.toString)

    // Tiempo
    val (tiempoPar, tTiempoPar) = measureTime { itsTiempoPar(org, dst) }
    val (tiempoSec, tTiempoSec) = measureTime { itsTiempoSec(org, dst) }
    println(s"Tiempo (Par):  $tiempoPar ($tTiempoPar ms)")
    println(s"Tiempo (Sec):  $tiempoSec ($tTiempoSec ms)")
    resultados = resultados :+ (id, org, dst, "Tiempo", tiempoPar.length.toString, tiempoSec.length.toString, if (tiempoPar == tiempoSec) "SI" else "NO", tTiempoPar.toString, tTiempoSec.toString)

    // Salida (Cita a las 23:59)
    val citaH = 23
    val citaM = 59
    val (salidaPar, tSalidaPar) = measureTime { itinerarioSalidaPar(vuelosPrueba, aeropuertosPrueba)(org, dst, citaH, citaM) }
    val (salidaSec, tSalidaSec) = measureTime { itinerarioSalida(vuelosPrueba, aeropuertosPrueba)(org, dst, citaH, citaM) }
    println(s"Salida (Par):  $salidaPar ($tSalidaPar ms)")
    println(s"Salida (Sec):  $salidaSec ($tSalidaSec ms)")
    
    // Convertimos el resultado único a lista para contar (0 o 1)
    val countPar = if (salidaPar.isEmpty) 0 else 1
    val countSec = if (salidaSec.isEmpty) 0 else 1
    
    resultados = resultados :+ (id, org, dst, "Salida", countPar.toString, countSec.toString, if (salidaPar == salidaSec) "SI" else "NO", tSalidaPar.toString, tSalidaSec.toString)

  }

  Helper.imprimirTablaResultados(resultados)

}