object Datos {
  case class Aeropuerto(Cod: String, X: Int, Y: Int, GMT: Int)
  case class Vuelo(Aln: String, Num: Int, Org: String, HS: Int, MS: Int, Dst: String, HL: Int, ML: Int, Esc: Int)
  type Itinerario = List[Vuelo]

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
    Vuelo("QATAR", 5566, "IST", 23, 0, "SVO", 2, 0, 0),
    // World Tour
    Vuelo("JAL", 1001, "HND", 10, 0, "DXB", 17, 0, 0),
    Vuelo("EMIRATES", 1002, "DXB", 19, 0, "IST", 23, 0, 0),
    Vuelo("TURKISH", 1003, "IST", 1, 0, "MAD", 5, 0, 0),
    Vuelo("IBERIA", 1004, "MAD", 7, 0, "JFK", 10, 0, 0),
    Vuelo("AA", 1005, "JFK", 12, 0, "MIA", 15, 0, 0),
    Vuelo("AA", 1006, "MIA", 17, 0, "BAQ", 20, 0, 0),
    // Shortcut
    Vuelo("AEROFLOT", 2001, "HND", 9, 0, "SVO", 14, 0, 0),
    Vuelo("AEROFLOT", 2002, "SVO", 16, 0, "MAD", 19, 0, 0)
  )
}
