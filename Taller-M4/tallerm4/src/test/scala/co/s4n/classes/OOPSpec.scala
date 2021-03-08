package co.s4n.classes

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** 
  * Tests del taller de Programacion orientada a objetos en Scala
  * @author Alex Montoya Franco
  */

class classesSpec extends AnyFlatSpec with Matchers {
  "El cubo de 3" should "debe dar 27" in {
    comp.cubo(3) shouldEqual 27
  }

  "El cubo de 2" should "debe dar 8" in {
    comp2.cubo(2L) shouldEqual 8
  }

  "La funcion despachar para el gato1" should "debe dar true" in {
    VentaDeChurrus.despachar(Gatos.gato1) shouldEqual true
  }

  "La funcion despachar para el gato2" should "debe dar false" in {
    VentaDeChurrus.despachar(Gatos.gato2) shouldEqual false
  }

  "Las carreras no terminadas del conductor1" should "deben dar 3" in {
    val conductor1 = new Conductor("David", "Rios", 5, 2)
    conductor1.getCarrerasNoTerminadas() shouldEqual 3
  }

  "Un nuevo contador con valor de 10 e incr" should "debe dar 11" in {
    val contador1 = new Contador(10)
    contador1.incr().contador shouldEqual 11
  }

  "Un nuevo contador2 con valor de 10 e incr(5)" should "debe dar 15" in {
    val contador1 = new Contador2(10)
    contador1.incr(5).contador shouldEqual 15
  }
}