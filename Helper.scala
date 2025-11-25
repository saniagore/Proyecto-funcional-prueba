import Datos._

object Helper {
  def imprimirTablaResultados(resultados: List[(String, String, String, String, String, String, String, String, String)]): Unit = {
    val headers = List("Prueba", "Origen", "Destino", "Criterio", "N° Itin (Par)", "N° Itin (Sec)", "Coinciden?", "T. Par (ms)", "T. Sec (ms)")
    val rows = resultados.map { case (p, o, d, c, np, ns, co, tp, ts) =>
      List(p, o, d, c, np, ns, co, tp, ts)
    }

    val colWidths = (headers :: rows).transpose.map(_.map(_.length).max + 2)
    val formatRow = (row: List[String]) =>
      row.zip(colWidths).map { case (cell, width) => cell.padTo(width, ' ') }.mkString("|", "|", "|")

    val separator = colWidths.map("-" * _).mkString("+", "+", "+")

    println(separator)
    println(formatRow(headers))
    println(separator)
    rows.foreach { row =>
      println(formatRow(row))
    }
    println(separator)
  }
}
