package co.s4n.lists

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** 
  * Tests del taller de listas basado en los 99 problemas de Haskell
  * @see https://wiki.haskell.org/99_questions/1_to_10
  * @author Alex Montoya Franco
  */

class listsProblemsSpec extends AnyFlatSpec with Matchers {

 "La función myLast aplicada a la lista de 1,2,3,4,5" should " debe dar 5" in {
   val lst = List(1, 2, 3, 4, 5)
   ListsProblems.myLast(lst) shouldEqual 5
 }

}