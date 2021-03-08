package co.s4n.traits

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** 
  * Tests del taller de Traits en Scala
  * @author Alex Montoya Franco
  */

class traitsSpec extends AnyFlatSpec with Matchers {
  "El perimetro de un Cuadrado(3.0)" should "debe ser 12" in {
    val cuadrado = Cuadrado(3.0)
    cuadrado.perimetro() shouldEqual 12.0
  }

  "El area de un Rectangulo2(2.0, 5.0)" should "debe ser 10.0" in {
    val rectangulo = Rectangulo2(2.0,5.0)
    rectangulo.area() shouldEqual 10.0
  }

  "La descripcion de Circulo3(10)" should "debe ser Un circulo de radio 10.0cm" in {
    Draw(Circulo3(10)) shouldEqual "Un circulo de radio 10.0cm"
  }

  "La descripcion de Rectangulo3(1, 2)" should "debe ser Un rectangulo de ancho 1.0 cm y largo 2.0 cm" in {
    Draw(Rectangulo3(1, 2)) shouldEqual "Un rectangulo de ancho 1.0 cm y largo 2.0 cm"
  }
}