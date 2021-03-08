package co.s4n.lists

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/**
  * Tests del taller de construccion de listas y Funciones de alto orden
  * @author Alex Montoya Franco
  */

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
  List.append(lst1, lst2) shouldEqual List(1,2,3,4)
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
"De una lista con 1,2,3 y otra lista con true, false, true, true la funcion zip" should "debe dar List((1,true),(2,false),(3,true))" in {
  val lst1 = List(1,2,3)
  val lst2 = List(true,false,true,true)
  List.zip(lst1,lst2) shouldEqual List((1,true),(2,false),(3,true))
  }
 "De una lista con 1,2,3,4 y otra lista con false, true, false la funcion zip" should "debe dar List((1,false),(2,true),(3,false))" in {
  val lst1 = List(1,2,3,4)
  val lst2 = List(false,true,false)
  List.zip(lst1,lst2) shouldEqual List((1,false),(2,true),(3,false))
  }
}

class unzipSpec extends AnyFlatSpec with Matchers {
 "De una lista con (1,a),(2,b),(3,c) la funcion unzip" should "debe dar (List(1,2,3), List(a,b,c))" in {
  val lst = List((1,"a"),(2,"b"),(3,"c"))
  List.unzip(lst) shouldEqual (List(1,2,3), List("a","b","c"))
  }
}

class reverseSpec extends AnyFlatSpec with Matchers {
 "De una lista con a,b,c la funcion reverse" should "debe dar c,b,a" in {
  val lst = List("a","b","c")
  List.reverse(lst) shouldEqual List("c","b","a")
  }
}

class intersperseSpec extends AnyFlatSpec with Matchers {
 "De un elem de 1 y una lista con 2,3,4,5 la funcion intersperse" should "debe dar 2,1,3,1,4,1,5" in {
  val lst = List(2,3,4,5)
  List.intersperse(1, lst) shouldEqual List(2,1,3,1,4,1,5)
  }
 "De un elem de a y una lista con b,c,d la funcion intersperse" should "debe dar b,a,c,a,d" in {
  val lst = List("b","c","d")
  List.intersperse("a", lst) shouldEqual List("b","a","c","a","d")
  }
}

class concatSpec extends AnyFlatSpec with Matchers {
 "De una lista con List(1,2,3),List(4,5,6) la funcion concat" should "debe dar List(1,2,3,4,5,6)" in {
  val lst = List(List(1,2,3),List(4,5,6))
  List.concat(lst) shouldEqual List(1,2,3,4,5,6)
  }
 "De una lista con List(a,b),List(c,d,e) la funcion concat" should "debe dar List(a,b,c,d,e)" in {
  val lst = List(List("a","b"),List("c","d","e"))
  List.concat(lst) shouldEqual List("a","b","c","d","e")
 }
 "De una lista con List(1.0,2.0),Nil,List(3.0,4.0) la funcion concat" should "debe dar List(1.0,2.0,3.0,4.0)" in {
  val lst = List(List(1.0,2.0),Nil,List(3.0,4.0))
  List.concat(lst) shouldEqual List(1.0,2.0,3.0,4.0) 
  }
}

class lengthSpec extends AnyFlatSpec with Matchers {
 "De una lista con List(1,2,3) la funcion length" should "debe dar 3" in {
  val lst = List(1,2,3)
  List.length(lst) shouldEqual 3
  }
}

class andSpec extends AnyFlatSpec with Matchers {
 "De una lista con true,true,true la funcion and" should "debe dar true" in {
  val lst = List(true,true,true)
  List.and(lst) shouldEqual true
  }
  "De una lista con true,false,true la funcion and" should "debe dar false" in {
  val lst = List(true,false,true)
  List.and2(lst) shouldEqual false
  }
}

class takeWhileSpec extends AnyFlatSpec with Matchers {
 "De una lista con 1,4,9 y el predicado <5 la funcion takeWhile" should "debe dar 1,4" in {
  val lst = List(1,4,9)
  List.takeWhile(lst)(_<5) shouldEqual List(1,4)
  }
}

class filterSpec extends AnyFlatSpec with Matchers {
 "De una lista con 1,4,1 y el predicado <3 la funcion filter" should "debe dar 1,1" in {
  val lst = List(1,4,1)
  List.filter(lst)(_<3) shouldEqual List(1,1)
  }
}

class unzipRSpec extends AnyFlatSpec with Matchers {
 "De una lista con (1,a),(2,b),(3,c) la funcion unzipR" should "debe dar (List(1,2,3), List(a,b,c))" in {
  val lst = List((1,"a"),(2,"b"),(3,"c"))
  List.unzipR(lst) shouldEqual (List(1,2,3), List("a","b","c"))
  }
}

class lengthLSpec extends AnyFlatSpec with Matchers {
 "De una lista con List(1,2,3) la funcion lengthL" should "debe dar 3" in {
  val lst = List(1,2,3)
  List.lengthL(lst) shouldEqual 3
  }
}

class andLSpec extends AnyFlatSpec with Matchers {
 "De una lista con true,true,true la funcion andL" should "debe dar true" in {
  val lst = List(true,true,true)
  List.andL(lst) shouldEqual true
  }
}

class takeWhileLSpec extends AnyFlatSpec with Matchers {
 "De una lista con 1,4,9 y el predicado <5 la funcion takeWhileL" should "debe dar 1,4" in {
  val lst = List(1,4,9)
  List.takeWhileL(lst)(_<5) shouldEqual List(1,4)
  }
}

class filterLSpec extends AnyFlatSpec with Matchers {
 "De una lista con 1,4,1 y el predicado <3 la funcion filterL" should "debe dar 1,1" in {
  val lst = List(1,4,1)
  List.filterL(lst)(_<3) shouldEqual List(1,1)
  }
}

class unzipLSpec extends AnyFlatSpec with Matchers {
 "De una lista con (1,a),(2,b),(3,c) la funcion unzipL" should "debe dar (List(1,2,3), List(a,b,c))" in {
  val lst = List((1,"a"),(2,"b"),(3,"c"))
  List.unzipL(lst) shouldEqual (List(1,2,3), List("a","b","c"))
  }
}