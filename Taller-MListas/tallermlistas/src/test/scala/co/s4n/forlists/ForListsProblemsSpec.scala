package co.s4n.forlists

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** 
  * Test del taller de listas usando listas de comprension(for) basado en los 99 problemas de Haskell
  * @see https://wiki.haskell.org/99_questions/1_to_10
  * @author Alex Montoya Franco
  */

class forListsProblemsSpec extends AnyFlatSpec with Matchers {

  "La función myLongitud aplicada a la lista de 1,2,3,4,5" should " debe dar 5" in {
   val lst = List(1, 2, 3, 4, 5)
   ForListsProblems.myLongitud(lst) shouldEqual 5
 }

 "La función myLongitud aplicada a la lista vacia ()" should " debe dar 0" in {
   val lst = List()
   ForListsProblems.myLongitud(lst) shouldEqual 0
 }

 "La función myLastFor aplicada a la lista de 1,2,3,4,5" should " debe dar 5" in {
   val lst = List(1, 2, 3, 4, 5)
   ForListsProblems.myLastFor(lst) shouldEqual 5
 }

 "La función myLastFor aplicada a la lista de true,true,false" should " debe dar false" in {
   val lst = List(true,true,false)
   ForListsProblems.myLastFor(lst) shouldEqual false
 }

 "La función myHeadFor aplicada a la lista de false,true,true" should " debe dar false" in {
   val lst = List(false,true,true)
   ForListsProblems.myHeadFor(lst) shouldEqual false
 }

 "La función myHeadFor aplicada a la lista de 1,2,3,4,5" should " debe dar 1" in {
   val lst = List(1,2,3,4,5)
   ForListsProblems.myHeadFor(lst) shouldEqual 1
 }

 "La función myKthElem aplicada a la lista de 3,6,9 con k de 0" should " debe dar 3" in {
   val lst = List(3,6,9)
   ForListsProblems.myKthElem(0, lst) shouldEqual 3
 }

 "La función myKthElem aplicada a la lista de 3,6,9 con k de 4" should " debe dar -1" in {
   val lst = List(3,6,9)
   ForListsProblems.myKthElem(4, lst) shouldEqual -1
 }

 "La función copyFor aplicada a la lista de 1,2,3,4,5" should " debe dar la lista de 1,2,3,4,5" in {
   val lst = List(1,2,3,4,5)
   ForListsProblems.copyFor(lst) shouldEqual List(1,2,3,4,5)
 }

 "La función reverseFor aplicada a la lista de 1,2,3" should " debe dar la lista de 3,2,1" in {
   val lst = List(1,2,3)
   ForListsProblems.reverseFor(lst) shouldEqual List(3,2,1)
 }

 "La función reverseFor aplicada a la lista de a,b,c" should " debe dar la lista de c,b,a" in {
   val lst = List("a","b","c")
   ForListsProblems.reverseFor(lst) shouldEqual List("c","b","a")
 }

 "La función reverseFor aplicada a la lista de false,false,true" should " debe dar la lista de true,false,false" in {
   val lst = List(false,false,true)
   ForListsProblems.reverseFor(lst) shouldEqual List(true,false,false)
 }


}