package co.s4n.traits

/** 
  * Taller de Traits en Scala
  * @author Alex Montoya Franco
  */

/**
  * Ejercicio 1
  * Trait Felino: Comportamiento comun a gatos
  */
sealed trait Felino {
    def color(): String = "Blanco"
    def sonido(): String
}

/**
  * Ejercicio 1
  * Leon: Sub-especie de la super clase Felino
  * 
  * @constructor    Construye objetos de tipo Leon
  * @param melena   Tamaño de melena
  * @note           Final class no puede ser heredada
  * @example        val leon = new Leon("4")
  * 
  */
final class Leon(val melena: Int = 0) extends Felino {
    override def color(): String = "Blanco"
    def sonido(): String = "chrrr"
}

/**
  * Ejercicio 1
  * Tigre: Sub-especie de la super clase Felino
  * 
  * @constructor    Construye objetos de tipo Tigre
  * @note           Final class no puede ser heredada
  * @example        val tigre = new Tigre()
  * 
  */
final class Tigre() extends Felino {
    override def color(): String = "Negro"
    def sonido():  String = "plop"
}

/**
  * Ejercicio 1
  * Jaguar: Sub-especie de la super clase Felino
  * 
  * @constructor    Construye objetos de tipo Jaguar
  * @note           Final class no puede ser heredada
  * @example        val jaguar = new Jaguar()
  *
  */
final class Jaguar() extends Felino {
    override def color(): String = "Salmon"
    def sonido(): String = "blublublu"
}

/**
  * Ejercicio 1
  * Gato: Sub-especie de la super clase Felino
  * 
  * @constructor    Construye objetos de tipo Gato
  * @param comida   String que representa la comida favorita del gato
  * @note           Final class no puede ser heredada
  * @example        val gato = new Gato()
  * 
  */
final class Gato(val comida: String = "Lasagna") extends Felino {
    override def color(): String = "Azulito"
    def sonido(): String = "Miau"
}


/**
  * Ejercicio 2
  * Trait Forma: Caracteristicas comunes de las formas 
  */
sealed trait Forma {
    def tamaño(): Int              // numero de lados
    def perimetro(): Double        // longitud total de los lados
    def area(): Double             // area de la forma en cuestion
}

/**
  * Ejercicio 2
  * Circulo: clase de la super clase Forma
  *
  * @constructor     Construye objetos de tipo Circulo
  * @param radio     Radio del circulo
  * @note            Final class no puede ser heredada
  * @note            Case class genera object companion y permite obviar el uso de new
  * @example         val circulo = Circulo(2.0)
  * 
  */
final case class Circulo(val radio: Double = 1.0) extends Forma {
    def tamaño(): Int = 0
    def perimetro(): Double = 2 * math.Pi * radio
    def area(): Double = math.Pi * radio * radio
}

/**
  * Ejercicio 2
  * Cuadrado: clase de la super clase Forma
  *
  * @constructor     Construye objetos de tipo Cuadrado
  * @param lado      Longitud del lado del cuadrado
  * @note            Final class no puede ser heredada
  * @note            Case class genera object companion y permite obviar el uso de new
  * @example         val cuadrado = Cuadrado(3.0)
  * 
  */
final case class Cuadrado(val lado: Double = 1.0) extends Forma {
    def tamaño(): Int = 4
    def perimetro(): Double = lado * tamaño()
    def area(): Double = lado * lado
}

/**
  * Ejercicio 2
  * Rectangulo: clase de la super clase Forma
  *
  * @constructor       Construye objetos de tipo Rectangulo
  * @param longitud    Longitud del rectangulo
  * @param altura      Altura del rectangulo
  * @note              Final class no puede ser heredada
  * @note              Case class genera object companion y permite obviar el uso de new
  * @example           val rectangulo = Rectangulo(2.0,5.0)
  * 
  */
final case class Rectangulo(val longitud: Double = 1.0, val altura: Double = 2.0) extends Forma {
    def tamaño(): Int = 4
    def perimetro(): Double = longitud * 2 + altura * 2
    def area(): Double = longitud * altura
}


/**
  * Ejercicio 3
  * Trait Forma2: Caracteristicas comunes de las formas 
  */
trait Forma2 {
    def tamaño(): Int
    def perimetro(): Double
    def area(): Double
}

/**
  * Ejercicio 3
  * Trait Rectangular: Tipos de rectangulos - Extiende de la super clase Forma2
  */
sealed trait Rectangular extends Forma2 {
    def tamaño(): Int = 4
}

final case class Circulo2(val radio: Double = 1.0) extends Forma2 {
    def tamaño(): Int = 0
    def perimetro(): Double = 2 * math.Pi * radio
    def area(): Double = math.Pi * radio * radio
}

final case class Cuadrado2(val lado: Double = 1.0) extends Rectangular {
    def perimetro(): Double = lado * tamaño()
    def area(): Double = lado * lado
}

final case class Rectangulo2(val longitud: Double = 1.0, val altura: Double = 2.0) extends Rectangular {
    def perimetro(): Double = longitud * 2 + altura * 2
    def area(): Double = longitud * altura
}


/**
  * Ejercicio 4
  * Trait Forma3: Caracteristicas comunes de las formas
  */
sealed trait Forma3 {
    def tamaño(): Int
    def perimetro(): Double
    def area(): Double
}

final case class Circulo3(val radio: Double = 1.0) extends Forma3 {
    def tamaño(): Int = 0
    def perimetro(): Double = 2 * math.Pi * radio
    def area(): Double = math.Pi * radio * radio
}

final case class Rectangulo3(val longitud: Double = 1.0, val altura: Double = 2.0) extends Forma3 {
    def tamaño(): Int = 4
    def perimetro(): Double = longitud * 2 + altura * 2
    def area(): Double = longitud * altura
}

/**
  * Singleton object Draw
  */
object Draw {
    /**
      * Funcion apply que toma una forma y retorna una descripcion de ella
      *
      * @param form     Circulo3 o Rectangulo3 de tipo Forma3
      * @return         Descripcion de la forma form
      * @example        Draw(Circulo3(10))  -->  "Un circulo de radio 10.0cm"
      * 
      */
    def apply(form: Forma3): String = form match {
        case Circulo3(radio)                   => s"Un circulo de radio $radio" + "cm"
        case Rectangulo3(longitud, altura)     => s"Un rectangulo de ancho $longitud cm y largo $altura cm"
    }
}


/**
  * Ejercicio 5
  * Definicion de la clase Color
  *
  * @constructor     Construye objetos de tipo Color
  * @param red       Componente rojo del color
  * @param green     Componente verde del color
  * @param blue      Componente azul del color
  * @example         val color = new Color(233,255,0)
  * 
  */
class Color(val red: Int, val green: Int, val blue: Int)

/**
  * Ejercicio 5
  * Objetos de tipo color
  */
object Rojo extends Color(255,0,0)
object Amarillo extends Color(255,233,0)
object Rosa extends Color(255,0,255)

/**
  * Ejercicio 5
  * Objeto de compañia para la clase Color
  * Permite crear objetos de tipo Color sin usar new
  * Este es un medio para que se produzcan colores personalizados con valores RGB
  * @example      val myNewColor = Color(0,0,0)
  */
object Color {
    def apply(red: Int, green: Int, blue: Int): Color = new Color(red, green, blue)
}