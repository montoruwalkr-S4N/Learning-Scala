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

 "La función myLast aplicada a la lista de true,true,false" should " debe dar false" in {
   val lst = List(true, true, false)
   ListsProblems.myLast(lst) shouldEqual false
 }

 "La función myButLast aplicada a la lista de 1,2,3,4" should " debe dar 3" in {
   val lst = List(1,2,3,4)
   ListsProblems.myButLast(lst) shouldEqual 3
 }

 "La función elementAt aplicada a la lista de a,b,c,d con n de 3" should " debe dar c" in {
   val lst = List("a","b","c","d")
   ListsProblems.elementAt(lst,3) shouldEqual "c"
 }

 "La función myLength aplicada a la lista de a,b,c,d" should " debe dar 4" in {
   val lst = List("a","b","c","d")
   ListsProblems.myLength(lst) shouldEqual 4
 }

 "La función myReverse aplicada a la lista de a,b,c,d" should " debe dar d,c,b,a" in {
   val lst = List("a","b","c","d")
   ListsProblems.myReverse(lst) shouldEqual List("d","c","b","a")
 }

 "La función isPalindrome aplicada a la lista de 1,2,1" should " debe dar true" in {
   val lst = List(1,2,1)
   ListsProblems.isPalindrome(lst) shouldEqual true
 }

 "La función isPalindrome aplicada a la lista de 1,2,3" should " debe dar false" in {
   val lst = List(1,2,3)
   ListsProblems.isPalindrome(lst) shouldEqual false
 }

}