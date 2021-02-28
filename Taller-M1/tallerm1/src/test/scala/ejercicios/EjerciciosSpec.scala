package ejercicios

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Ejercicio1Spec extends AnyFlatSpec with Matchers {
  "El area de un triangulo rectangulo con lados 2 y 5" should "debe dar 5" in {
    Ejercicio1.areaTrianguloRectangulo(2,5) shouldEqual 5
  }
}

class Ejercicio2Spec extends AnyFlatSpec with Matchers {
  "El area de un circulo con radio 2" should "debe dar 12.56" in {
  Ejercicio2.areaDeUnCirculo(2).toInt shouldEqual 12
  }
}