/** Ejercicio 1 */
def myLast[A](lst: List[A]): A = lst match {
 case x :: Nil   => x
 case x :: xs    => myLast(xs)
}


/** Ejercicio 2 */
def myButLast[A](lst: List[A]): A = lst match {
 case x	:: y ::	Nil   => x
 case x :: xs         => myButLast(xs)
}


/** Ejercicio 3 */
def elementAt[A](lst: List[A], n: Int): A = (lst, n) match {
 case (x :: _, 1)    	=> x
 case (_ :: xs, n)      => elementAt(xs, n-1)
}


/** Ejercicio 4 */
def myLength[A](lst: List[A]): Int = lst match {
 case x :: Nil   => 1
 case x :: xs    => 1 + myLength(xs)
}

def myLengthR[A](lst: List[A]): Int = {
 @annotation.tailrec
 def aux[A](lst: List[A], n: Int): Int = lst match {
  case Nil      => n
  case x :: xs  => aux(xs,n+1)
 }
 aux(lst,0)
}


/** Ejercicio 5 */

def myReverse[A](lst: List[A]): List[A] = lst match {
 case x :: xs      => myReverse(xs) ::: List(x)
 case Nil          => Nil
}


/** Ejercicio 6 */
def isPalindrome[A](lst: List[A]): Boolean = lst match {
 case Nil                    => true
 case x :: Nil               => true
 case x :: xs                => (x == xs.last && isPalindrome(xs.init))
}


/** Ejercicio 7 */

sealed trait NestedList[+A]

case class Elem[A](elem: A) extends NestedList[A]

case class Lista[A](lista: List[NestedList[A]]) extends NestedList[A]


