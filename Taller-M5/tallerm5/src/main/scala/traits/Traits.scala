package traits

/** Ejercicios Taller-M5 - Traits  */


/** Ejercicio 1 */

sealed trait Felino {
 def color(): String = "Blanco"
 def sonido(): String
}

final class Leon(val melena: Int = 0) extends Felino {
 override def color(): String = "Blanco"
 def sonido(): String = "chrrr"
}

final class Tigre() extends Felino {
 override def color(): String = "Negro"
 def sonido():  String = "plop"
}

final class Jaguar() extends Felino {
 override def color(): String = "Salmon"
 def sonido(): String = "blublublu"
}

final class Gato(val comida: String = "Lasagna") extends Felino {
 override def color(): String = "Azulito"
 def sonido(): String = "Miau"
}


/** Ejercicio 2 */

sealed trait Forma {
 def tamaño(): Int
 def perimetro(): Double
 def area(): Double
}

final case class Circulo(val radio: Double = 1) extends Forma {
 def tamaño(): Int = 0
 def perimetro(): Double = 2 * math.Pi * radio
 def area(): Double = math.Pi * radio * radio
}

final case class Cuadrado(val lado: Double = 1) extends Forma {
 def tamaño(): Int = 4
 def perimetro(): Double = lado * tamaño()
 def area(): Double = lado * lado
}

final case class Rectangulo(val longitud: Double = 1, val altura: Double = 2) extends Forma {
 def tamaño(): Int = 4
 def perimetro(): Double = longitud * 2 + altura * 2
 def area(): Double = longitud * altura
}


/** Ejercicio 3 */

trait Forma2 {
 def tamaño(): Int
 def perimetro(): Double
 def area(): Double
}

sealed trait Rectangular extends Forma2 {
 def tamaño(): Int = 4
}

final case class Circulo2(val radio: Double = 1) extends Forma2 {
 def tamaño(): Int = 0
 def perimetro(): Double = 2 * math.Pi * radio
 def area(): Double = math.Pi * radio * radio
}

final case class Cuadrado2(val lado: Double = 1) extends Rectangular {
 def perimetro(): Double = lado * tamaño()
 def area(): Double = lado * lado
}

final case class Rectangulo2(val longitud: Double = 1, val altura: Double = 2) extends Rectangular {
 def perimetro(): Double = longitud * 2 + altura * 2
 def area(): Double = longitud * altura
}


/** Ejercicio 4 */

sealed trait Forma3 {
 def tamaño(): Int
 def perimetro(): Double
 def area(): Double
}

final case class Circulo3(val radio: Double = 1) extends Forma3 {
 def tamaño(): Int = 0
 def perimetro(): Double = 2 * math.Pi * radio
 def area(): Double = math.Pi * radio * radio
}

final case class Rectangulo3(val longitud: Double = 1, val altura: Double = 2) extends Forma3 {
 def tamaño(): Int = 4
 def perimetro(): Double = longitud * 2 + altura * 2
 def area(): Double = longitud * altura
}

object Draw {
 def apply(form: Forma3): String = form match {
  case Circulo3(radio)                 => s"Un circulo de radio $radio" + "cm"
  case Rectangulo3(longitud, altura)   => s"Un rectangulo de ancho $longitud cm y largo $altura cm"
 }
}


/** Ejercicio 5 */

class Color(val red: Int, val green: Int, val blue: Int)

object Rojo extends Color(255,0,0)
object Amarillo extends Color(255,233,0)
object Rosa extends Color(255,0,255)

object Color {

 def apply(red: Int, green: Int, blue: Int): Color = new Color(red, green, blue)

}