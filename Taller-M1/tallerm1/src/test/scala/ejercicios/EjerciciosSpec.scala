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

class Ejercicio3Spec extends AnyFlatSpec with Matchers {
  "El salario de una persona que devenga 5000 y se le deduce 300" should "debe ser 4700" in {
  Ejercicio3.calSalario(5000,300) shouldEqual 4700
  }
}

class Ejercicio4Spec extends AnyFlatSpec with Matchers {
  "El salario de una persona con bono incluido que devenga 5000 y se le deduce 300" should "debe ser 5200" in {
  Ejercicio4.calSalarioBono(5000,300) shouldEqual 5200
  }
}

class Ejercicio5ASpec extends AnyFlatSpec with Matchers {
  "El salario de una persona aplicando la funcion calSalario que devenga 5000 y se le deduce 300" should "debe ser 4700" in {
  Ejercicio5.compSalario(Ejercicio3.calSalario, 5000, 300) shouldEqual 4700
  }
}

class Ejercicio5BSpec extends AnyFlatSpec with Matchers {
  "El salario de una persona aplicando la funcion calSalarioBono que devenga 5000 y se le deduce 300" should "debe ser 5200" in {
  Ejercicio5.compSalario(Ejercicio4.calSalarioBono, 5000, 300) shouldEqual 5200
  }
}

class Ejercicio7Spec extends AnyFlatSpec with Matchers {
  "El salario de una persona con bono del 5% que devenga 5000 y se le deduce 300" should "debe ser 2200" in {
  Ejercicio7.calSalario5(5000,300) shouldEqual 2200
  }
}

class Ejercicio8Spec extends AnyFlatSpec with Matchers {
  "El salario de una persona con bono del 20% que devenga 5000 y se le deduce 300" should "debe ser 700" in {
  Ejercicio8.calSalario20(5000,300) shouldEqual 700
  }
}

class Ejercicio9Spec extends AnyFlatSpec with Matchers {
  "El salario de una persona con bono del 20% externo que devenga 5000 y se le deduce 300" should "debe ser 700" in {
  Ejercicio9.calSalarioBonoClausura(5000,300) shouldEqual 700
  }
}

class Ejercicio10Spec extends AnyFlatSpec with Matchers {
  "El salario de una persona con bono del 20% externo que devenga 5000 y se le deduce 300" should "debe ser 700" in {
  Ejercicio5.compSalario(Ejercicio9.calSalarioBonoClausura, 5000, 300) shouldEqual 700
  }
}

class Ejercicio11Spec extends AnyFlatSpec with Matchers {
  "El salario de una persona con bono del 15% que devenga 5000 y se le deduce 300" should "debe ser 5450" in {
  Ejercicio11.calSalario15(5000,300) shouldEqual 5450
  }
}

class Ejercicio12Spec extends AnyFlatSpec with Matchers {
  "El salario de una persona con bono del 20% que devenga 5000 y se le deduce 300" should "debe ser 9700" in {
  Ejercicio12.calSalario20(5000,300) shouldEqual 9700
  }
}

class Ejercicio15Spec extends AnyFlatSpec with Matchers {
  "El factorial de 7" should "debe dar 5040 " in {
  Ejercicio15.factorial(7) shouldEqual 5040
  }
}

class Ejercicio16Spec extends AnyFlatSpec with Matchers {
  "El fibonacci de 9" should "debe dar 34 " in {
  Ejercicio16.fibonacci(9) shouldEqual 34
  }
}

class Ejercicio17Spec extends AnyFlatSpec with Matchers {
  "El factorial de 3" should "debe dar 6 " in {
  Ejercicio17.factorial(3) shouldEqual 6
  }
}