package co.s4n.nats

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Tests del taller de construccion de listas - Naturales
  * @author Alex Montoya Franco
  */

class addNatSpec extends AnyFlatSpec with Matchers {
 "addNat de Cero, Suc(Cero)" should "debe dar Suc(Cero)" in {
   Nat.addNat(Cero, Suc(Cero)) shouldEqual Suc(Cero)
  }
 "addNat de Suc(Suc(Cero)), Suc(Cero)" should "debe dar Suc(Suc(Suc(Cero)))" in {
   Nat.addNat(Suc(Suc(Cero)), Suc(Cero)) shouldEqual Suc(Suc(Suc(Cero)))
  }
 "addNat de Suc(Suc(Suc(Cero))), Suc(Suc(Cero))" should "debe dar Suc(Suc(Suc(Suc(Suc(Cero)))))" in {
   Nat.addNat(Suc(Suc(Suc(Cero))), Suc(Suc(Cero))) shouldEqual Suc(Suc(Suc(Suc(Suc(Cero)))))
  }
}

class prodNatSpec extends AnyFlatSpec with Matchers {
 "prodNat de Cero, Suc(Cero)" should "debe dar Cero" in {
   Nat.prodNat(Cero, Suc(Cero)) shouldEqual Cero
  }
 "prodNat de Suc(Suc(Cero)), Suc(Cero)" should "debe dar Suc(Suc(Cero))" in {
   Nat.prodNat(Suc(Suc(Cero)), Suc(Cero)) shouldEqual Suc(Suc(Cero))
  }
 "prodNat de Suc(Suc(Suc(Cero))), Suc(Suc(Cero))" should "debe dar Suc(Suc(Suc(Suc(Suc(Suc(Cero))))))" in {
   Nat.prodNat(Suc(Suc(Suc(Cero))), Suc(Suc(Cero))) shouldEqual Suc(Suc(Suc(Suc(Suc(Suc(Cero))))))
  }
}