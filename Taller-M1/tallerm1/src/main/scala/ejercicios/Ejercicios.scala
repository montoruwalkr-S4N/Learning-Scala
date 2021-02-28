package ejercicios

/** Funcion anonima que recibe los dos lados rectos de un triangulo rectangulo y calcula su area */
object Ejercicio1 {
 val areaTrianguloRectangulo = (a: Int, b: Int) => { b * a / 2 }
}

/** Funcion anonima que recibe el radio de un circulo y calcula su area */
object Ejercicio2 {
 import scala.math.Pi
 import scala.math.pow

 val areaDeUnCirculo = new Function1[Double, Double] {
  def apply(r: Double): Double = { Pi * pow(r,2) }
 }
}