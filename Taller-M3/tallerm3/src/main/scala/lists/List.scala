package lists

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Const[+A](h: A, t: List[A]) extends List[A]

object List {

 /** Funcion que recibe una lista y retorna su longitud */
 def length[A](lst:List[A]):Int = lst match {
  case Nil => 0
  case Const(h,t) => 1 + length(t)
 }

 /** Funcion que construye una List a partir de una secuencia de Const */
 def apply[A](as: A*): List[A] = {
  if(as.isEmpty) Nil
  else Const(as.head, apply(as.tail: _*))
 }


 /** Funcion que recibe una lista y retorna una copia de la lista sin el primer elemento */
 def tail[A](ds: List[A]): Any = ds match {
  case Nil => "Empty List"
  case Const(_,t) => t
 }

 def tail2[A](ds: List[A]): List[A] = ds match {
  case Const(_,t) => t
 }


 /** Funcion que recibe una lista y retorna su primer elemento */
 def head[A](ds: List[A]): Any = ds match {
  case Nil => "Empty List"
  case Const(h,_) => h
 }

 def head2[A](ds: List[A]): A = ds match {
  case Const(h,_) => h
 }


 /** Funcion que añade un elemento al inicio de la lista */
 def const[A](h:A, t:List[A]):List[A] = Const(h,t)


 /** Funcion que añade un elemento al final de la lista  */
 def addEnd[A](lst:List[A], elem:A):List[A] = lst match {
  case Nil => Const(elem, Nil)
  case Const(h, t) => Const(h, addEnd(t, elem))
 }


 /** Funcion que concatena dos listas */
 def append[A](lst1:List[A], lst2:List[A]):List[A] = (lst1, lst2) match {
  case (Nil,Nil)          => Nil
  case (lst1,Nil)         => lst1
  case (Nil,lst2)         => lst2
  case (Const(h, t), lst2)  => Const(h, append(t, lst2))
 }


 /** Funcion que elimina los primeros n elementos */
 def drop[A](n:Int, lst:List[A]):List[A] = (n, lst) match {
  case (n,Nil) => Nil
  case (0,lst) => lst
  case (n,Const(h, t)) => drop(n-1, t)
 }


 /** Ejercicios Taller M3 - Construccion de Listas  */

 /** Ejercicio 1: Funcion que toma los n primeros elementos de una lista */
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

 /** Ejercicio 2: Funcion que recibe una lista y retorna la lista sin el ultimo elemento  */
 def init[A](lst: List[A]): List[A] = {
  def aux[A](lst: List[A], acum: List[A]): List[A] = (lst, acum) match {
   case (Const(h, Nil), Nil)    => Nil
   case (Const(h, Nil), acum)   => acum
   case (Const(h, t), Nil)      => aux(t, addEnd(acum, h))
   case (Const(h, t), acum)     => aux(t, addEnd(acum, h))
  }
  aux(lst, Nil)
 }

 /** Ejercicio 3: Funcion que divide una lista en dos dado un parametro n, la primera lista queda de n elementos y la segunda queda con los elementos restantes  */
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

 /** Ejercicio 4: Funcion que fusiona dos listas de tipos diferentes en una lista de pares del mismo tamaño */
 def zip[A,B](lst1: List[A], lst2: List[B]): List[(A,B)] = {
  def aux[A,B](lst1: List[A], lst2: List[B], acum: List[(A,B)]): List[(A,B)] = (lst1, lst2, acum) match {
   case (Nil, Nil, acum) => acum
   case (Const(h1,t1), Const(h2, t2), Nil) => aux(t1, t2,)
   case (Const(h1,t1), Const(h2, t2), acum) => aux(t1, t2,)
  }
  aux(lst1, lst2, Nil)
 }
 



}