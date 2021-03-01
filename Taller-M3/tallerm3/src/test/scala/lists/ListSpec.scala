package lists

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class TailSpec extends AnyFlatSpec with Matchers {
  "De una lista con 1,2,3 la funcion tail" should "debe dar 2,3" in {
  val lst = List(1,2,3)
  List.tail(lst) shouldEqual List(2,3)
  }

  "De una lista con Hello,World,Again la funcion tail" should "debe dar World,Again" in {
  val lst = List("Hello","World","Again")
  List.tail(lst) shouldEqual List("World","Again")
  }

}

class HeadSpec extends AnyFlatSpec with Matchers {
  "De una lista con 1,2,3 la funcion head" should "debe dar 1" in {
  val lst = List(1,2,3)
  List.head(lst) shouldEqual 1
  }

  "De una lista con Hello,World,Again la funcion head" should "debe dar Hello" in {
  val lst = List("Hello","World","Again")
  List.head(lst) shouldEqual "Hello"
  }

  "De una lista vacia la funcion head" should "debe dar Empty List" in {
  val lst = List()
  List.head(lst) shouldEqual "Empty List"
  }
}

class constSpec extends AnyFlatSpec with Matchers {
  "De un 0 y una lista con 1,2,3 la funcion const" should "debe dar 0,1,2,3" in {
  val lst = List(1,2,3)
  List.const(0,lst) shouldEqual List(0,1,2,3)
  }
}

class addEndSpec extends AnyFlatSpec with Matchers {
  "De una lista con 1,3,5 y un 8 la funcion addEnd" should "debe dar 1,3,5,8" in {
  val lst = List(1,3,5)
  List.addEnd(lst,8) shouldEqual List(1,3,5,8)
  }
}

class appendSpec extends AnyFlatSpec with Matchers {
  "De una lista con 1,2 y otra lista con 3,4 la funcion append" should "debe dar 1,2,3,4" in {
  val lst1 = List(1,2)
  val lst2 = List(3,4)
  List.append(lst1,lst2) shouldEqual List(1,2,3,4)
  }
}

class dropSpec extends AnyFlatSpec with Matchers {
  "De un n de 3 y una lista con 1,2,3,4,5,6,7 la funcion drop" should "debe dar 4,5,6,7" in {
  val lst = List(1,2,3,4,5,6,7)
  List.drop(3, lst) shouldEqual List(4,5,6,7)
  }
}

class takeSpec extends AnyFlatSpec with Matchers {

 "De un n de 3 y una lista con a,b,c,d,e la funcion take" should "debe dar a,b,c" in {
  val lst = List("a","b","c","d","e")
  List.take(3, lst) shouldEqual List("a","b","c")
  }

 "De un n de 0 y una lista con 1,2,3,4 la funcion take" should "debe dar Nil" in {
  val lst = List(1,2,3,4)
  List.take(0, lst) shouldEqual Nil
  }

  "De un n de 6 y una lista con 1.0,2.0,3.0 la funcion take" should "debe dar 1.0,2.0,3.0" in {
  val lst = List(1.0,2.0,3.0)
  List.take(6, lst) shouldEqual lst
  }
}

class initSpec extends AnyFlatSpec with Matchers {

 "De una lista con 1 la funcion init" should "debe dar Nil" in {
  val lst = List(1)
  List.init(lst) shouldEqual Nil
  }

 "De una lista con 1,2,3,4,5,6 la funcion init" should "debe dar 1,2,3,4,5" in {
  val lst = List(1,2,3,4,5,6)
  List.init(lst) shouldEqual List(1,2,3,4,5)
  }
}

class splitSpec extends AnyFlatSpec with Matchers {

 "De un n de 3 y una lista con 1,2,3,4,5,6,7 la funcion split" should "debe dar (List(1,2,3), List(4,5,6,7))" in {
  val lst = List(1,2,3,4,5,6,7)
  List.split(3, lst) shouldEqual (List(1,2,3),List(4,5,6,7))
  }

 "De un n de 1 y una lista con 1,2,3,4,5,6,7 la funcion split" should "debe dar (List(1), List(2,3,4,5,6,7))" in {
  val lst = List(1,2,3,4,5,6,7)
  List.split(1, lst) shouldEqual (List(1),List(2,3,4,5,6,7))
  }

 "De un n de 8 y una lista con 1,2,3,4,5,6,7 la funcion split" should "debe dar (List(2,3,4,5,6,7), Nil)" in {
  val lst = List(1,2,3,4,5,6,7)
  List.split(8, lst) shouldEqual (List(1,2,3,4,5,6,7),Nil)
  }

 "De un n de 0 y una lista con 1,2,3,4,5,6,7 la funcion split" should "debe dar (Nil, List(1,2,3,4,5,6,7))" in {
  val lst = List(1,2,3,4,5,6,7)
  List.split(0, lst) shouldEqual (Nil, List(1,2,3,4,5,6,7))
  }
}

class zipSpec extends AnyFlatSpec with Matchers {

 "De una lista con 1,2,3 y otra lista con true, false, true la funcion zip" should "debe dar List((1,true),(2,false),(3,true))" in {
  val lst1 = List(1,2,3)
  val lst2 = List(true,false,true)
  List.zip(lst1,lst2) shouldEqual List((1,true),(2,false),(3,true))
  }

}