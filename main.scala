// Envuelve todo el código en un objeto
object Main extends App {

  // --- DEFINICIONES DE DATOS (DEL PDF) ---
  case class Aeropuerto(Cod: String, X: Int, Y: Int, GMT: Int)
  case class Vuelo(Aln: String, Num: Int, Org: String, HS: Int, MS: Int, Dst: String, HL: Int, ML: Int, Esc: Int)
  type Itinerario = List[Vuelo]

  // --- DATOS DE PRUEBA (DEL PDF) ---
  val aeropuertosCurso: List[Aeropuerto] = List(
    Aeropuerto("CLO", 100, 200, -500), // Cali
    Aeropuerto("BOG", 300, 500, -500), // Bogota
    Aeropuerto("MDE", 200, 600, -500), // Medellin
    Aeropuerto("BAQ", 350, 850, -500), // Barranquilla
    Aeropuerto("SMR", 400, 950, -500), // Santa Marta
    Aeropuerto("CTG", 300, 800, -500), // Cartagena (Corregí Y de 500 a 800 para que SMR-PTY no sea más corto)
    Aeropuerto("PTY", 400, 1000, -500), // Ciudad de Panama
    Aeropuerto("JFK", 2000, 2000, -400), // Nueva York
    Aeropuerto("MIA", 1000, 2000, -500), // Miami
    Aeropuerto("MEX", 1000, 1000, -600), // Ciudad de Mexico
    Aeropuerto("MAD", 5000, 5000, 100), // Madrid
    Aeropuerto("SVCS", 400, 1000, -600), // Caracas
    Aeropuerto("MID", 500, 1000, -600), // Merida
    Aeropuerto("AUA", 500, 2000, -400), // Aruba
    Aeropuerto("IST", 9000, 9000, 300), // Estambul
    Aeropuerto("HND", 10000, 12000, 900), // Tokio
    Aeropuerto("DXB", 9500, 11500, 400), // Dubai
    Aeropuerto("SVO", 12500, 12500, 300) // Moscu
  )

  val vuelosCurso: List[Vuelo] = List(
    Vuelo("AIRVZLA", 601, "MID", 5, 0, "SVCS", 6, 0, 0),
    Vuelo("AIRVZLA", 602, "SVCS", 6, 30, "MID", 7, 30, 0),
    Vuelo("AVA", 9432, "CLO", 7, 0, "SVO", 2, 20, 4),
    Vuelo("AVA", 9433, "CLO", 7, 0, "BOG", 8, 0, 0),
    Vuelo("IBERIA", 505, "BOG", 18, 0, "MAD", 12, 0, 0),
    Vuelo("IBERIA", 506, "MAD", 14, 0, "SVO", 23, 20, 0),
    Vuelo("IBERIA", 507, "MAD", 16, 0, "SVO", 1, 20, 0),
    Vuelo("LATAM", 787, "BOG", 17, 0, "MEX", 19, 0, 0),
    Vuelo("VIVA", 756, "BOG", 9, 0, "MDE", 10, 0, 0),
    Vuelo("VIVA", 769, "MDE", 11, 0, "BAQ", 12, 0, 0),
    Vuelo("AVA", 5643, "BAQ", 14, 0, "MEX", 16, 0, 0),
    Vuelo("COPA", 1234, "CTG", 10, 0, "PTY", 11, 30, 0),
    Vuelo("AVA", 4321, "CTG", 9, 30, "SMR", 10, 0, 0),
    Vuelo("COPA", 7631, "SMR", 10, 50, "PTY", 11, 50, 0),
    Vuelo("TURKISH", 7799, "CLO", 7, 0, "IST", 14, 0, 3),
    Vuelo("QATAR", 5566, "IST", 23, 0, "SVO", 2, 0, 0)
  )


  // FUNCIONES BASICAS DE ITINERARIOS // 

  def itinerarios(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    val vuelosPorOrigen = vuelos.groupBy(_.Org)
    val codsValidos = aeropuertos.map(_.Cod).toSet

    // Función recursiva que busca todos los itinerarios desde un aeropuerto de origen (cod1) hasta un destino (cod2)
    def buscarItinerarios(cod1: String, cod2: String, visitados: Set[String]): List[Itinerario] = {
      if (cod1 == cod2) {
        List(Nil)
      } else {
        // Obtenemos los vuelos que salen del aeropuerto actual
        val vuelosSalientes = vuelosPorOrigen.getOrElse(cod1, Nil)
        // Y filtramos los que conducen a aeropuertos aún no visitados
        val vuelosValidos = vuelosSalientes.filter(v => !visitados.contains(v.Dst))

        // Exploramos recursivamente los destinos posibles, concatenando el vuelo actual con el resto del itinerario
        for {
          vuelo <- vuelosValidos
          resto <- buscarItinerarios(vuelo.Dst, cod2, visitados + cod1)
        } yield vuelo :: resto
      }
    }

    // Función que devolvemos
    (cod1: String, cod2: String) => {
      // Verificamos que ambos códigos correspondan a aeropuertos válidos
      if (!codsValidos.contains(cod1) || !codsValidos.contains(cod2)) {
        List()
      } else {
        // Si es válido, tons iniciamos la búsqueda recursiva desde el aeropuerto de origen hacia el destino
        buscarItinerarios(cod1, cod2, Set())
      }
    }
  }



  def itinerariosPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    val vuelosPorOrigen = vuelos.groupBy(_.Org)
    val codsValidos = aeropuertos.map(_.Cod).toSet

    def buscarItinerariosPar(cod1: String, cod2: String, visitados: Set[String]): List[Itinerario] = {
      // Caso base: llegó al destino
      if (cod1 == cod2) {
        List(Nil)
      } else {
        val vuelosSalientes = vuelosPorOrigen.getOrElse(cod1, Nil)
        val vuelosValidos = vuelosSalientes.filter(v => !visitados.contains(v.Dst))

        // Umbral: Si hay 1 o 0 vuelos, es más rápido hacerlo secuencial.
        if (vuelosValidos.length <= 1) {
          for {
            vuelo <- vuelosValidos
            resto <- buscarItinerariosPar(vuelo.Dst, cod2, visitados + cod1)
          } yield vuelo :: resto
        } else {
          // Procesamiento paralelo usando par collection
          vuelosValidos.par.flatMap { vuelo =>
            val subItinerarios = buscarItinerariosPar(vuelo.Dst, cod2, visitados + cod1)
            subItinerarios.map(resto => vuelo :: resto)
          }.toList
        }
      }
    }

    (cod1: String, cod2: String) => {
      if (!codsValidos.contains(cod1) || !codsValidos.contains(cod2)) {
        List()
      } else {
        buscarItinerariosPar(cod1, cod2, Set())
      }
    }
  }






  // --- FUNCIÓN SECUENCIAL A PROBAR ---

  def itinerariosEscalas(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {
    (cod1: String, cod2: String) => {

      def calcularEscalas(itinerario: Itinerario): Int = {
        val escalasTecnicas = itinerario.map(_.Esc).sum
        val escalasPorConexion = if (itinerario.isEmpty) 0 else itinerario.length - 1
        escalasTecnicas + escalasPorConexion
      }

      val todosLosItinerarios = itinerarios(vuelos, aeropuertos)(cod1, cod2)
      val itinerariosOrdenados = todosLosItinerarios.sortBy(calcularEscalas)
      itinerariosOrdenados.take(3)
    }
  }

  def itinerariosEscalasPar(vuelos: List[Vuelo], aeropuertos: List[Aeropuerto]): (String, String) => List[Itinerario] = {

    // Función principal que usamos para buscar rutas entre dos códigos.
      (codOrigen, codDestino) => {

        // Helper para sumar las escalas de todos los vuelos de una ruta
        def calcularEscalas(itinerario: List[Vuelo]): Int = {
          val escalasTecnicas = itinerario.map(_.Esc).sum
          val escalasPorConexion = if (itinerario.isEmpty) 0 else itinerario.length - 1
          escalasTecnicas + escalasPorConexion
        }

        /**
         * Busca todos los caminos (DFS).
         * El 'visitados' es para no dar vueltas.
         */
        def buscarItinerario(origenActual: String, destinoFinal: String, visitados: Set[String], itinerarioActual: List[Vuelo]): List[Itinerario] = {

          // Caso base: Llegamos al destino
          if (origenActual == destinoFinal) List(itinerarioActual)

          else {
            // Vuelos que salen de aquí y van a un sitio nuevo
            val vuelosDisponibles = vuelos.filter(vuelo => vuelo.Org == origenActual && !visitados.contains(vuelo.Dst))

            //  Busca rutas desde cada vuelo disponible
            vuelosDisponibles.par.flatMap(vuelo => 
              buscarItinerario(vuelo.Dst, destinoFinal, visitados + origenActual, itinerarioActual :+ vuelo)
            ).toList // Volvemos a List normal.
          }
        }

        // Encontramos todas las rutas posibles.
        val todosLosItinerarios = buscarItinerario(codOrigen, codDestino, Set(), List())

        // Ordenamos: el que tenga menos escalas va primero.
        val itinerariosOrdenados = todosLosItinerarios.sortBy(calcularEscalas)

        // Devolvemos el TOP 3 de rutas.
        itinerariosOrdenados.take(3)
      }
    }


  // --- BLOQUE DE EJECUCIÓN ---

  println("--- ✈️ Iniciando Pruebas de Itinerarios ---")

  val vuelosPrueba = vuelosCurso
  val aeropuertosPrueba = aeropuertosCurso
  val itsEscalasPar = itinerariosEscalasPar(vuelosPrueba, aeropuertosPrueba)

  val itsEscalasSec = itinerariosEscalas(vuelosPrueba, aeropuertosPrueba)
  // --- PRUEBA 1: MID -> SVCS (Vuelo único) ---
  println("\n--- PRUEBA 1: MID -> SVCS ---")
  val itsel_par = itsEscalasPar("MID", "SVCS")
  println(s"Paralelo:   $itsel_par")
  val itsel_sec = itsEscalasSec("MID", "SVCS")
  println(s"Secuencial: $itsel_sec")

  // --- PRUEBA 2: CLO -> SVCS (No conectado) ---
  println("\n--- PRUEBA 2: CLO -> SVCS ---")
  val itsel2_par = itsEscalasPar("CLO", "SVCS")
  println(s"Paralelo:   $itsel2_par")
    val itsel2_sec = itsEscalasSec("CLO", "SVCS")
  println(s"Secuencial: $itsel2_sec")

  // --- PRUEBA 3: CLO -> SVO (Caso clave) ---
  println("\n--- PRUEBA 3: CLO -> SVO ---")
  val itsel3_par = itsEscalasPar("CLO", "SVO")
  println(s"Paralelo:   $itsel3_par")
    val itsel3_sec = itsEscalasSec("CLO", "SVO")
  println(s"Secuencial: $itsel3_sec")

  // --- PRUEBA 4: CLO -> MEX ---
  println("\n--- PRUEBA 4: CLO -> MEX ---")
  val itsc4_par = itsEscalasPar("CLO", "MEX")
  println(s"Paralelo:   $itsc4_par")
    val itsc4_sec = itsEscalasSec("CLO", "MEX")
  println(s"Secuencial: $itsc4_sec")

  // --- PRUEBA 5: CTG -> PTY ---
  println("\n--- PRUEBA 5: CTG -> PTY ---")
  val itsc5_par = itsEscalasPar("CTG", "PTY")
  println(s"Paralelo:   $itsc5_par")
    val itsc5_sec = itsEscalasSec("CTG", "PTY")
  println(s"Secuencial: $itsc5_sec")

} // Cierre del 'object Main'