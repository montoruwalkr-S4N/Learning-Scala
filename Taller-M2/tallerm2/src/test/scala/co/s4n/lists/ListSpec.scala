package co.s4n.lists

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Tests del taller de tipos de datos inmutables - Listas
  * @author Alex Montoya Franco
  */

class LengthSpec extends AnyFlatSpec with Matchers {
  "De una lista con true, false, true, true la funcion length" should "debe dar 4" in {
    val lst = List(true, false, true, true)
    List.length(lst) shouldEqual 4
  }
}

class SumSpec extends AnyFlatSpec with Matchers {
  "De una lista con 2,3,5 la funcion sum" should "debe dar 10" in {
    val lst = List(2, 3, 5)
    List.sum(lst) shouldEqual 10
  }
}

class ProductSpec extends AnyFlatSpec with Matchers {
  "De una lista con 5,5 la funcion product" should "debe dar 25" in {
    val lst = List(5.0, 5.0)
    List.product(lst) shouldEqual 25
  }
}

class Ejercicio1Spec extends AnyFlatSpec with Matchers {
  "x" should "debe dar 9" in {
    List.x shouldEqual 9
  }
}

class Ejercicio2TailSpec extends AnyFlatSpec with Matchers {
  "De una lista con 1,2,3 la funcion tail" should "debe dar 2,3" in {
  val lst = List(1,2,3)
  List.tail(lst) shouldEqual List(2,3)
  }
  "De una lista con Hello,World,Again la funcion tail" should "debe dar World,Again" in {
  val lst = List("Hello","World","Again")
  List.tail(lst) shouldEqual List("World","Again")
  }
}

class Ejercicio3HeadSpec extends AnyFlatSpec with Matchers {
  "De una lista con 1,2,3 la funcion head" should "debe dar 1" in {
  val lst = List(1,2,3)
  List.head(lst) shouldEqual 1
  }
  "De una lista con Hello,World,Again la funcion head" should "debe dar Hello" in {
  val lst = List("Hello","World","Again")
  List.head(lst) shouldEqual "Hello"
  }
}

class Ejercicio4AndSpec extends AnyFlatSpec with Matchers {
  "De una lista con true, true, true la funcion and" should "debe dar true" in {
  val lst = List(true, true, true)
  List.and(lst) shouldEqual true
  }
  "De una lista con true, false, true la funcion and" should "debe dar false" in {
  val lst = List(true, false, true)
  List.and(lst) shouldEqual false
  }
}

class Ejercicio5OrSpec extends AnyFlatSpec with Matchers {
  "De una lista con false, false, false la funcion or" should "debe dar false" in {
  val lst = List(false, false, false)
  List.or(lst) shouldEqual false
  }
  "De una lista con true, false, true la funcion or" should "debe dar true" in {
  val lst = List(true, false, true)
  List.or(lst) shouldEqual true
  }
}

class Ejercicio6MaxSpec extends AnyFlatSpec with Matchers {
  "De una lista con 3,8,1 la funcion max" should "debe dar 8" in {
  val lst = List(3,8,1)
  List.max(lst) shouldEqual 8
  }
  "De una lista con 5,-2,9  la funcion max" should "debe dar 9" in {
  val lst = List(5,-2,9)
  List.max(lst) shouldEqual 9
  }
 "De una lista con -8,-19,-1  la funcion max" should "debe dar -1" in {
  val lst = List(-8,-19,-1)
  List.max(lst) shouldEqual -1
  }
}

class Ejercicio7MinSpec extends AnyFlatSpec with Matchers {
  "De una lista con 3,8,1 la funcion min" should "debe dar 1" in {
  val lst = List(3L,8L,1L)
  List.min(lst) shouldEqual 1
  }
  "De una lista con 5,-2,9  la funcion min" should "debe dar -2" in {
  val lst = List(5L,-2L,9L)
  List.min(lst) shouldEqual -2
  }
 "De una lista con -8,-19,-1  la funcion min" should "debe dar -19" in {
  val lst = List(-8L,-19L,-1L)
  List.min(lst) shouldEqual -19
  }
}

class Ejercicio8MinMaxSpec extends AnyFlatSpec with Matchers {
  "De una lista con 3,8,1 la funcion minMax" should "debe dar (1,8)" in {
  val lst = List(3.0,8.0,1.0)
  List.minMax(lst) shouldEqual (1,8)
  }
  "De una lista con 5,-2,9  la funcion minMax" should "debe dar (-2,9)" in {
  val lst = List(5.0,-2.0,9.0)
  List.minMax(lst) shouldEqual (-2,9)
  }
 "De una lista con -8,-19,-1  la funcion minMax" should "debe dar (-19,-1)" in {
  val lst = List(-8.0,-19.0,-1.0)
  List.minMax(lst) shouldEqual (-19,-1)
  }
  "De una lista con 3,0,7,-3,5,18,1  la funcion minMax" should "debe dar (-3,18)" in {
  val lst = List(3.0, 0.0, 7.0, -3.0, 5.0, 18.0, 1.0)
  List.minMax(lst) shouldEqual (-3,18)
  }
}