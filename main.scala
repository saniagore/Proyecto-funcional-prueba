import Datos._
import Operaciones._

object Main extends App {

  // --- BLOQUE DE EJECUCIÓN ---

  // println("--- Iniciando Pruebas de Itinerarios ---")

  val vuelosPrueba = vuelosCurso
  val aeropuertosPrueba = aeropuertosCurso

  // Definición de funciones a probar
  val itsEscalasPar = itinerariosEscalasPar(vuelosPrueba, aeropuertosPrueba)
  val itsEscalasSec = itinerariosEscalas(vuelosPrueba, aeropuertosPrueba)
  
  val itsAirePar = itinerariosAirePar(vuelosPrueba, aeropuertosPrueba)
  val itsAireSec = itinerariosAire(vuelosPrueba, aeropuertosPrueba)

  val itsTiempoPar = itinerariosTiempoPar(vuelosPrueba, aeropuertosPrueba)
  val itsTiempoSec = itinerariosTiempo(vuelosPrueba, aeropuertosPrueba)

  val itsPar = itinerariosPar(vuelosPrueba, aeropuertosPrueba)
  val itsSec = itinerarios(vuelosPrueba, aeropuertosPrueba)

  val pruebas = List(
    ("1", "MID", "SVCS"), ("2", "CLO", "SVCS"), ("3", "CLO", "SVO"), ("4", "CLO", "MEX"), ("5", "CTG", "PTY"),
    ("6", "MDE", "MEX"), ("7", "BOG", "SVO"), ("8", "CLO", "IST"), ("9", "IST", "SVO"), ("10", "HND", "BAQ"),
    ("11", "HND", "MAD"), ("12", "DXB", "MIA"), ("13", "BOG", "LIM"), ("14", "LIM", "CUZ"), ("15", "CUZ", "LPB"),
    ("16", "LPB", "VVI"), ("17", "VVI", "GRU"), ("18", "GRU", "GIG"), ("19", "GIG", "MIA"), ("20", "MIA", "DFW"),
    ("21", "DFW", "LAX"), ("22", "ATL", "CDG"), ("23", "CDG", "DXB"), ("24", "DXB", "HND"), ("25", "HND", "SYD"),
    ("26", "SYD", "LAX"), ("27", "SFO", "HNL"), ("28", "HNL", "AKL"), ("29", "AKL", "SYD"), ("30", "SIN", "LHR"),
    ("31", "LHR", "JFK"), ("32", "FRA", "BOG"), ("33", "MUC", "DEN"), ("34", "ZRH", "MIA"), ("35", "AMS", "PTY"),
    ("36", "PTY", "SJO"), ("37", "SJO", "GUA"), ("38", "GUA", "MEX"), ("39", "MEX", "CUN"), ("40", "CUN", "HAV"),
    ("41", "BOG", "MIA"), ("42", "MDE", "PTY"), ("43", "CTG", "JFK"), ("44", "CLO", "MAD"), ("45", "BAQ", "MIA"),
    ("46", "SMR", "BOG"), ("47", "PTY", "LAX"), ("48", "JFK", "LHR"), ("49", "MIA", "EZE"), ("50", "MEX", "LIM"),
    ("51", "MAD", "DXB"), ("52", "SVCS", "MIA"), ("53", "MID", "MEX"), ("54", "AUA", "AMS"), ("55", "IST", "HND"),
    ("56", "HND", "SFO"), ("57", "DXB", "SYD"), ("58", "SVO", "PEK"), ("59", "ABQ", "DFW"), ("60", "ATL", "MCO"),
    ("61", "BNA", "ORD"), ("62", "BOS", "SFO"), ("63", "DCA", "MIA"), ("64", "DEN", "SEA"), ("65", "DFW", "JFK"),
    ("66", "DTW", "LAX"), ("67", "HOU", "ATL"), ("68", "JFK", "LHR"), ("69", "LAX", "HND"), ("70", "MIA", "GIG"),
    ("71", "MSP", "ORD"), ("72", "MSY", "IAH"), ("73", "ORD", "LHR"), ("74", "PHL", "CDG"), ("75", "PHX", "DEN"),
    ("76", "PVD", "DCA"), ("77", "RDU", "ATL"), ("78", "SEA", "HNL"), ("79", "SFO", "NRT"), ("80", "STL", "ORD"),
    ("81", "TPA", "JFK"), ("82", "LIM", "MAD"), ("83", "SCL", "MIA"), ("84", "EZE", "FCO"), ("85", "CUZ", "LIM"),
    ("86", "LPB", "BOG"), ("87", "VVI", "MIA"), ("88", "GRU", "JFK"), ("89", "GIG", "LIS"), ("90", "CDG", "JFK"),
    ("91", "SYD", "SFO"), ("92", "HNL", "LAX"), ("93", "AKL", "LAX"), ("94", "SIN", "FRA"), ("95", "LHR", "DXB"),
    ("96", "FRA", "JFK"), ("97", "MUC", "ORD"), ("98", "ZRH", "JFK"), ("99", "AMS", "MSP"), ("100", "SJO", "MIA"),
    ("101", "MAD", "BOG"), ("102", "CDG", "ATL"), ("103", "SYD", "DXB"), ("104", "HND", "SFO"), ("105", "JFK", "LHR"),
    ("106", "MIA", "EZE"), ("107", "MEX", "LIM"), ("108", "GRU", "JFK"), ("109", "GIG", "MIA"), ("110", "CDG", "JFK"),
    ("111", "SYD", "SFO"), ("112", "HNL", "LAX"), ("113", "AKL", "LAX"), ("114", "SIN", "FRA"), ("115", "LHR", "DXB"),
    ("116", "FRA", "JFK"), ("117", "MUC", "ORD"), ("118", "ZRH", "JFK"), ("119", "AMS", "MSP"), ("120", "SJO", "MIA"),
    ("121", "BOG", "MAD"), ("122", "ATL", "CDG"), ("123", "DXB", "SYD"), ("124", "SFO", "HND"), ("125", "LHR", "JFK"),
    ("126", "EZE", "MIA"), ("127", "LIM", "MEX"), ("128", "JFK", "GRU"), ("129", "MIA", "GIG"), ("130", "JFK", "CDG"),
    ("131", "SFO", "SYD"), ("132", "LAX", "HNL"), ("133", "LAX", "AKL"), ("134", "FRA", "SIN"), ("135", "DXB", "LHR"),
    ("136", "JFK", "FRA"), ("137", "ORD", "MUC"), ("138", "JFK", "ZRH"), ("139", "MSP", "AMS"), ("140", "MIA", "SJO"),
    ("141", "CLO", "MIA"), ("142", "MDE", "JFK"), ("143", "CTG", "MIA"), ("144", "BAQ", "JFK"), ("145", "SMR", "MIA"),
    ("146", "PTY", "MIA"), ("147", "JFK", "MIA"), ("148", "MIA", "JFK"), ("149", "MEX", "JFK"), ("150", "CUN", "JFK"),
    ("151", "HAV", "MIA"), ("152", "SJO", "JFK"), ("153", "GUA", "MIA"), ("154", "LIM", "MIA"), ("155", "SCL", "JFK"),
    ("156", "EZE", "JFK"), ("157", "CUZ", "MIA"), ("158", "LPB", "MIA"), ("159", "VVI", "MIA"), ("160", "GRU", "MIA"),
    ("161", "GIG", "MIA"), ("162", "CDG", "MIA"), ("163", "SYD", "MIA"), ("164", "HNL", "MIA"), ("165", "AKL", "MIA"),
    ("166", "SIN", "MIA"), ("167", "LHR", "MIA"), ("168", "FRA", "MIA"), ("169", "MUC", "MIA"), ("170", "ZRH", "MIA"),
    ("171", "AMS", "MIA"), ("172", "ABQ", "MIA"), ("173", "ATL", "MIA"), ("174", "BNA", "MIA"), ("175", "BOS", "MIA"),
    ("176", "DCA", "MIA"), ("177", "DEN", "MIA"), ("178", "DFW", "MIA"), ("179", "DTW", "MIA"), ("180", "HOU", "MIA"),
    ("181", "LAX", "MIA"), ("182", "MSP", "MIA"), ("183", "MSY", "MIA"), ("184", "ORD", "MIA"), ("185", "PHL", "MIA"),
    ("186", "PHX", "MIA"), ("187", "PVD", "MIA"), ("188", "RDU", "MIA"), ("189", "SEA", "MIA"), ("190", "SFO", "MIA"),
    ("191", "STL", "MIA"), ("192", "TPA", "MIA"), ("193", "MID", "MIA"), ("194", "SVCS", "MIA"), ("195", "AUA", "MIA"),
    ("196", "IST", "MIA"), ("197", "HND", "MIA"), ("198", "DXB", "MIA"), ("199", "SVO", "MIA"), ("200", "MAD", "MIA")
  )

  def measureTime[T](block: => T): (T, Long) = {
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    (result, (end - start) / 1000000) // Convert to ms
  }

  val headers = List("Prueba", "Origen", "Destino", "Criterio", "N° Itin (Par)", "N° Itin (Sec)", "Coinciden?", "T. Par (ms)", "T. Sec (ms)")
  Helper.imprimirCabecera(headers)

  var totalEscalasPar = 0L
  var totalEscalasSec = 0L
  var totalAirePar = 0L
  var totalAireSec = 0L
  var totalTiempoPar = 0L
  var totalTiempoSec = 0L
  var totalSalidaPar = 0L
  var totalSalidaSec = 0L
  var totalItinPar = 0L
  var totalItinSec = 0L

  for ((id, org, dst) <- pruebas) {
    // Escalas
    val (escPar, tEscPar) = measureTime { itsEscalasPar(org, dst) }
    val (escSec, tEscSec) = measureTime { itsEscalasSec(org, dst) }
    totalEscalasPar += tEscPar
    totalEscalasSec += tEscSec
    Helper.imprimirResultado(List(id, org, dst, "Escalas", escPar.length.toString, escSec.length.toString, if (escPar == escSec) "SI" else "NO", tEscPar.toString, tEscSec.toString))

    // Aire
    val (airePar, tAirePar) = measureTime { itsAirePar(org, dst) }
    val (aireSec, tAireSec) = measureTime { itsAireSec(org, dst) }
    totalAirePar += tAirePar
    totalAireSec += tAireSec
    Helper.imprimirResultado(List(id, org, dst, "Aire", airePar.length.toString, aireSec.length.toString, if (airePar == aireSec) "SI" else "NO", tAirePar.toString, tAireSec.toString))

    // Tiempo
    val (tiempoPar, tTiempoPar) = measureTime { itsTiempoPar(org, dst) }
    val (tiempoSec, tTiempoSec) = measureTime { itsTiempoSec(org, dst) }
    totalTiempoPar += tTiempoPar
    totalTiempoSec += tTiempoSec
    Helper.imprimirResultado(List(id, org, dst, "Tiempo", tiempoPar.length.toString, tiempoSec.length.toString, if (tiempoPar == tiempoSec) "SI" else "NO", tTiempoPar.toString, tTiempoSec.toString))

    // Salida (Cita a las 23:59)
    val citaH = 23
    val citaM = 59
    val (salidaPar, tSalidaPar) = measureTime { itinerarioSalidaPar(vuelosPrueba, aeropuertosPrueba)(org, dst, citaH, citaM) }
    val (salidaSec, tSalidaSec) = measureTime { itinerarioSalida(vuelosPrueba, aeropuertosPrueba)(org, dst, citaH, citaM) }
    totalSalidaPar += tSalidaPar
    totalSalidaSec += tSalidaSec
    
    // Convertimos el resultado único a lista para contar (0 o 1)
    val countPar = if (salidaPar.isEmpty) 0 else 1
    val countSec = if (salidaSec.isEmpty) 0 else 1
    
    Helper.imprimirResultado(List(id, org, dst, "Salida", countPar.toString, countSec.toString, if (salidaPar == salidaSec) "SI" else "NO", tSalidaPar.toString, tSalidaSec.toString))

    // Itinerarios (General)
    val (itinPar, tItinPar) = measureTime { itsPar(org, dst) }
    val (itinSec, tItinSec) = measureTime { itsSec(org, dst) }
    totalItinPar += tItinPar
    totalItinSec += tItinSec
    Helper.imprimirResultado(List(id, org, dst, "Itinerarios", itinPar.length.toString, itinSec.length.toString, if (itinPar == itinSec) "SI" else "NO", tItinPar.toString, tItinSec.toString))
  }

  Helper.imprimirCierre()

  // Calcular y mostrar resumen
  val stats = List(
    ("Escalas", totalEscalasPar, totalEscalasSec, if (totalEscalasPar > 0) totalEscalasSec.toDouble / totalEscalasPar else 0.0),
    ("Aire", totalAirePar, totalAireSec, if (totalAirePar > 0) totalAireSec.toDouble / totalAirePar else 0.0),
    ("Tiempo", totalTiempoPar, totalTiempoSec, if (totalTiempoPar > 0) totalTiempoSec.toDouble / totalTiempoPar else 0.0),
    ("Salida", totalSalidaPar, totalSalidaSec, if (totalSalidaPar > 0) totalSalidaSec.toDouble / totalSalidaPar else 0.0),
    ("Itinerarios", totalItinPar, totalItinSec, if (totalItinPar > 0) totalItinSec.toDouble / totalItinPar else 0.0)
  )
  Helper.imprimirTablaResumen(stats)

}