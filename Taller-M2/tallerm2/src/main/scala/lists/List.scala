package lists

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Const[+A](h: A, t: List[A]) extends List[A]

object List {

 /** Funcion que recibe una lista de cualquier tipo y retorna su longitud */
 def length[A](lst:List[A]): Int = lst match {
  case Nil => 0
  case Const(h,t) => 1 + length(t)
 }


 /** Funcion que recibe una lista de enteros y retorna la suma de sus elementos */
 def sum(ints: List[Int]): Int = ints match {
  case Nil => 0
  case Const(h,t) => h + sum(t)
 }


 /** Funcion que recibe una lista de doubles y retorna el producto de sus elementos  */
 def product(ds: List[Double]): Double = ds match {
  case Nil => 1
  case Const(h,t) => h * product(t)
 }


 /** Funcion que construye una List a partir de una secuencia de Const */
 def apply[A](as: A*): List[A] = {
  if(as.isEmpty) Nil
  else Const(as.head, apply(as.tail: _*))
 }


 /** Ejercicios Taller M2 - Tipos de datos inmutables  */

 /** Ejercicio 1: Analisis de la expresion match (x vale 9 debido a la coincidencia de patrones) */
 val x = List(4,5,6,7,8) match {
  case Const(x, Const(5, Const(7, _)))              => x
  case Nil                                          => 1
  case Const(x, Const(y, Const(6, Const(7, _))))    => x + y
  case Const(h,t)                                   => h + sum(t)
  case _                                            => 777
 }


 /** Ejercicio 2: Funcion que recibe una lista y retorna una copia de la lista sin el primer elemento */
 def tail[A](ds: List[A]): Any = ds match {
  case Nil => "Empty List"
  case Const(_,t) => t
 }

 def tail2[A](ds: List[A]): List[A] = ds match {
  case Const(_,t) => t
 }

 /** Ejercicio 3: Funcion que recibe una lista y retorna su primer elemento */
 def head[A](ds: List[A]): Any = ds match {
  case Nil => "Empty List"
  case Const(h,_) => h
 }

 def head2[A](ds: List[A]): A = ds match {
  case Const(h,_) => h
 }

 /** Ejercicio 4: Funcion que recibe una lista de valores booleanos y devuelve true si todos los elementos son verdaderos, en caso contrario devuelve false  */
 def and(lst: List[Boolean]): Boolean = lst match {
  case Nil => true
  case Const(false, _) => false
  case Const(true,t) => and(t)
 }

 /** Ejercicio 4 - Solucion del docente */
 def and2(lst: List[Boolean]): Boolean = lst match {
  case Nil => true
  case Const(h,t) => h && and2(t)
 }

 /** Ejercicio 5: Funcion que recibe una lista de valores booleanos y devuelve false si todos los valores son falsos, en caso contrario devuelve true */
 def or(lst: List[Boolean]): Boolean = lst match {
  case Nil => false
  case Const(true,_) => true
  case Const(false,t) => or(t)
 }

 /** Ejercicio 5 - Solucion del docente */
 def or2(lst: List[Boolean]): Boolean = lst match {
  case Nil => false
  case Const(h,t) => h || or2(t)
 }

 /** Ejercicio 6: Funcion que recibe una lista de valores enteros y devuelve su valor maximo */
 def max(lst: List[Int]): Int = {
 
  @annotation.tailrec
  def maxValue(lst: List[Int], maxVal: Int): Int = lst match {
   case Nil => maxVal
   case Const(h,t) => if (h > maxVal) maxValue(t,h) else maxValue(t,maxVal) 
  }
  maxValue(tail2(lst), head2(lst).toInt)

 }

 /** Ejercicio 7: Funcion que recibe una lista de valores Long y devuelve su valor minimo */
 def min(lst: List[Long]): Long = {

  @annotation.tailrec
  def minValue(lst: List[Long], minVal: Long): Long = lst match {
   case Nil => minVal
   case Const(h,t) => if (h < minVal) minValue(t,h) else minValue(t,minVal)
  }
  minValue(tail2(lst), head2(lst).toLong)

 }

 /** Ejercicio 8: Funcion que recibe una lista de valores Double y devuelve una tupla con el valor minimo y el maximo  */
 def minMax(lst: List[Double]): (Double, Double) = {

  @annotation.tailrec
  def minMaxValues(lst: List[Double], tupla: (Double, Double)): (Double, Double) = lst match {
   case Nil => tupla
   case Const(h,t) => minMaxValues(t, (if (h < tupla._1) h else tupla._1, if (h > tupla._2) h else tupla._2))
  }
  minMaxValues(tail2(lst), (head2(lst).toDouble, head2(lst).toDouble))

 }



}