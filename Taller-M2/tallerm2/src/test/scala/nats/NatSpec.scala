package	nats

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NatSpec extends AnyFlatSpec with Matchers {

 "Nat se puede construir" should "como cero" in {
   val cero = Cero
   cero shouldEqual Cero
  }

  "Nat se puede construir" should "como uno" in {
    val uno = Suc(Cero)
    uno shouldEqual Suc(Cero)
  }

  "Nat se puede construir" should "como dos" in {
    val	dos = Suc(Suc(Cero))
    dos	shouldEqual Suc(Suc(Cero))
  }

}

class FromNatToIntSpec extends AnyFlatSpec with Matchers {

 "La transformacion de Nat a Int" should "debe dar 0 si se ingresa Cero" in {
   val cero = Cero
   Nat.fromNatToInt(cero) shouldEqual 0
  }

 "La transformacion de Nat a Int" should "debe dar 1 si se ingresa Suc(Cero)" in {
   val uno = Suc(Cero)
   Nat.fromNatToInt(uno) shouldEqual 1
  }

 "La transformacion de Nat a Int" should "debe dar 4 si se ingresa Suc(Suc(Suc(Suc(Cero))))" in {
   val cuatro = Suc(Suc(Suc(Suc(Cero))))
   Nat.fromNatToInt(cuatro) shouldEqual 4
  }

}

class FromIntToNatSpec extends AnyFlatSpec with Matchers {

 "La transformacion de Int a Nat" should "debe dar Cero si se ingresa 0" in {
   Nat.fromIntToNat(0) shouldEqual Cero
  }

 "La transformacion de Int a Nat" should "debe dar Suc(Cero) si se ingresa 1" in {
   Nat.fromIntToNat(1) shouldEqual Suc(Cero)
  }

 "La transformacion de Int a Nat" should "debe dar Suc(Suc(Suc(Cero))) si se ingresa 3" in {
   Nat.fromIntToNat(3) shouldEqual Suc(Suc(Suc(Cero)))
  }

}