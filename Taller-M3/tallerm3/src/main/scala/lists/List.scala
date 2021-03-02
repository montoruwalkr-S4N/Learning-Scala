package lists

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Const[+A](h: A, t: List[A]) extends List[A]

object List {

 def apply[A](as: A*) : List[A] ={
    if (as.isEmpty) Nil
    else Const(as.head, apply(as.tail: _*))
 }

 /** Funcion que añade un elemento al inicio de la lista */
 def const[A](h:A, t:List[A]):List[A] = Const(h,t)

 /** Funcion que añade un elemento al final de la lista  */
 def addEnd[A](lst:List[A], elem:A):List[A] = lst match {
  case Nil => Const(elem, Nil)
  case Const(h, t) => Const(h, addEnd(t, elem))
 }

 /** Funcion que añade un elemento al final de la lista  */
 def addStart[A](elem:A, lst: List[A]): List[A] = lst match {
  case Nil => Const(elem, Nil)
  case Const(h, t) => Const(h, addStart(elem, t))
 }

 /** Funcion que concatena dos listas */
 def append[A](lst1:List[A], lst2:List[A]):List[A] =
 (lst1,lst2) match {
  case (Nil,Nil)          => Nil
  case (lst1,Nil)         => lst1
  case (Nil,lst2)         => lst2
  case (Const(h,t),lst2)  => Const(h, append(t, lst2))
 }


 /** Ejercicios Taller M3 - Construccion de listas  */


 /** Ejercicio 1 - Version 1: Funcion que toma los n primeros elementos de una lista */
 def take[A](n: Int, lst: List[A]): List[A] =  {
  def aux[A](n: Int, lst1: List[A], lst2: List[A]): List[A] = (n, lst1, lst2) match {
   case (0, lst1, Nil)            => Nil
   case (n, Nil, Nil)             => Nil
   case (0, lst1, lst2)           => lst2
   case (n, Nil, lst2)            => lst2
   case (n, Const(h, t), Nil)     => aux(n-1, t, addEnd(lst2, h))
   case (n, Const(h, t), lst2)    => aux(n-1, t, addEnd(lst2, h))
   }
  aux(n, lst, Nil)
 }

 /** Ejercicio 1 - Version 2 */
 def take2[A](n: Int, lst: List[A]): List[A] = (n, lst) match {
  case (0, _)            => Nil
  case (n, Nil)          => Nil
  case (n, Const(h,t))   => Const(h, take2(n-1, t))
 }

 /** Ejercicio 1 - Version 3 */
 def take3[A](n: Int, lst: List[A]): List[A] = {
  def aux[A](n: Int, lst: List[A], acum: List[A]): List[A] = (n, lst) match {
   case (0, _)            => acum
   case (n, Nil)          => acum
   case (n, Const(h,t))   => aux(n-1, t, addEnd(acum,h))
  }
  aux(n, lst, Nil)
 }


 // --------------------------------------------------------------



/** Ejercicio 2 - Version 1: Funcion que recibe una lista y retorna la lista sin el ultimo elemento  */
 def init[A](lst: List[A]): List[A] = {
  def aux[A](lst: List[A], acum: List[A]): List[A] = (lst, acum) match {
   case (Const(h, Nil), Nil)    => Nil
   case (Const(h, Nil), acum)   => acum
   case (Const(h, t), Nil)      => aux(t, addEnd(acum, h))
   case (Const(h, t), acum)     => aux(t, addEnd(acum, h))
  }
  aux(lst, Nil)
 }

 /** Ejercicio 2 - Version 2 */
 def init2[A](lst: List[A]): List[A] = lst match {
  case Const(h, Nil)           => Nil
  case Const(h, t)             => Const(h, init2(t))
 }

 /** Ejercicio 2 - Version 3 */
 def init3[A](lst: List[A]): List[A] = {
  def aux[A](lst: List[A], acum: List[A]): List[A] = lst match {
   case Const(h, Nil)         => acum
   case Const(h, t)           => aux(t, addEnd(acum, h))
  }
  aux(lst, Nil)
 }


 // --------------------------------------------------------------



 /** Ejercicio 3 - Version 1: Funcion que divide una lista en dos dado un parametro n, la primera lista queda de n elementos y la segunda queda con los elementos restantes  */
 def split[A](n: Int, lst: List[A]): (List[A], List[A]) = {
  def aux[A](n: Int, lst: List[A], acum: List[A]): (List[A], List[A]) = (n, lst, acum) match {
   case (0, lst, Nil)             => (Nil, lst)   
   case (n, Nil, Nil)             => (Nil, Nil)
   case (0, lst, acum)            => (acum, lst)
   case (n, Nil, acum)            => (acum, Nil)
   case (n, Const(h, t), Nil)     => aux(n-1, t, addEnd(acum, h))
   case (n, Const(h, t), acum)    => aux(n-1, t, addEnd(acum, h))
   }
  aux(n, lst, Nil)
 }

 /** Ejercicio 3 - Version 2 */
 def split2[A](n: Int, lst: List[A]): (List[A], List[A]) = {
  def aux[A](n: Int, lst: List[A], acum: List[A]):(List[A], List[A]) = (n, lst) match {
   case (n, Nil)                 => (acum, Nil)
   case (0, lst)                 => (acum, lst)
   case (n, Const(h, t))         => aux(n-1, t, addEnd(acum, h))
  }
  aux(n, lst, Nil)
 }


 // --------------------------------------------------------------


 /** Ejercicio 4 - Version 1: Funcion que fusiona dos listas de tipos diferentes en una lista de pares del mismo tamaño */
 def zip[A,B](lst1: List[A], lst2: List[B]): List[(A,B)] = (lst1, lst2) match {
  case (lst1, Nil)                       => Nil  
  case (Nil, lst2)                       => Nil
  case (Const(h1, t1), Const(h2, t2))    => Const((h1, h2), zip(t1, t2))
 }

 /** Ejercicio 4 - Version 2 */
 def zip2[A,B](lst1: List[A], lst2: List[B]): List[(A,B)] = {
  def aux[A,B](lst1: List[A], lst2:List[B], acum: List[(A,B)]): List[(A,B)] = (lst1, lst2) match {
   case (lst1, Nil)                       => acum
   case (Nil, lst2)                       => acum
   case (Const(h1, t1), Const(h2, t2))    => aux(t1, t2, addEnd(acum, (h1,h2)))
  }
  aux(lst1, lst2, Nil)
 }


 // --------------------------------------------------------------


 /** Ejercicio 5: Funcion que separa una lista de tuplas en dos listas distintas */
 def unzip[A,B](lst: List[(A,B)]):(List[A],List[B]) = {
  def aux[A,B](lst: List[(A,B)], acum1: List[A], acum2: List[B]): (List[A], List[B]) = lst match {
   case Nil                          => (acum1, acum2)
   case Const((h1,h2), t)            => aux(t, addEnd(acum1, h1), addEnd(acum2, h2))
  }
  aux(lst, Nil, Nil)
 }


 // --------------------------------------------------------------


 /** Ejercicio 6: Funcion que toma una lista y devuelve una funcion invertida de la misma  */
 def reverse[A](lst: List[A]): List[A] = lst match {
  case Nil              => Nil
  case Const(h, t)      => addStart(h, reverse(t))
 }


 // --------------------------------------------------------------


 /** Ejercicio 7: Funcion que entremezcla un valor entre los elementos de la lista */
 def intersperse[A](elem: A, lst: List[A]): List[A] = lst match {
  case Nil             => Nil
  case Const(h, Nil)   => Const(h, Nil)
  case Const(h, t)     => Const(h, Const(elem, intersperse(elem, t)))
 }


 // --------------------------------------------------------------


 /** Ejercicio 8: Funcion que recibe una lista de lista de valores de tipo A y la transforma en una lista de valores de tipo A  */
 def concat[A](lst: List[List[A]]): List[A] = lst match {
  case Nil            => Nil
  case Const(h, t)    => append(h, concat(t))
 }


 // --------------------------------------------------------------


 /** Ejercicios Taller M3 - Funciones de alto orden  */


  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil                  => z
    case Const(h, t)          => f(h, foldRight(t, z)(f))
  }


  /** Ejercicio 14: Funcion length con foldRight */
  def lengthF[A](lst: List[A]): Int = foldRight(lst, 0)((_, y) => y + 1)


  /** Ejercicio 15: Funcion and con foldRight */
  def and(lst: List[Boolean]): Boolean = foldRight(lst, true)((x, y) => x && y)
  def and2(lst: List[Boolean]): Boolean = foldRight(lst, true)(_&&_)


  /** Ejercicio 16: Funcion takeWhile */
  def takeWhile2[A](lst: List[A])(p: A => Boolean): List[A] =
   foldRight(lst, Nil: List[A])((h, t) => if (p(h)) Const(h, t) else Nil)


  /** Ejercicio 17: Funcion filter */
  def filter[A](lst: List[A])(p: A => Boolean): List[A] =
   foldRight(lst, Nil: List[A])((h, t) => if (p(h)) Const(h, t) else t)


  /** Ejercicio 18: Funcion unzip */
  def unzip[A,B](lst: List[(A, B)]): (List[A], List[B]) =
   foldRight(lst, (Nil, Nil): (List[A], List[B]))(h, t) => (Const(h._1, t._1), Const(h._2, t._2)))


  @annotation.tailrec
  def foldLeft[A,B](lst: List[A], z: B)(f: (B,A) => B): B = lst match {
   case Const(h, t)              => foldLeft(t, f(z, h))(f)
   case Nil                     => z
  }


  /** Ejercicio 19: Funcion lengthL */
  def lengthL[A](lst: List[A]): Int = foldLeft(lst, 0)((y, _) => 1+y) 


  /** Ejercicio 20: Funcion andL */
  def andL(lst: List[Boolean]): Boolean = foldLeft(lst, true)(_&&_)


  /** Ejercicio 21: Funcion takeWhileL */
  def takeWhileL[A](lst: List[A])(p: A => Boolean): List[A] = ???


  /** Ejercicio 22: Funcion filterL */
  def filterL[A](lst: List[A])(p: A => Boolean): List[A] =
   foldLeft(lst, Nil: List[A])((lst, e) => if (p(e)) addEnd(lst, e) else lst)


  /** Ejercicio 23: Funcion unzipL */
  def unzipL[A,B](lst: List[(A,B)]): (List[A], List[B]) =
   foldLeft(lst, (Nil, Nil): (List[A], List[B]))((lst, e) => (addEnd(lst._1, e._1), addEnd(lst._2, e._2)))


}



