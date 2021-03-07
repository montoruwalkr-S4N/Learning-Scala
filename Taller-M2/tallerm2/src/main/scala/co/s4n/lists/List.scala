package co.s4n.lists

/**
  * Taller de tipos de datos inmutables - Listas
  * @author Alex Montoya Franco
  */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const[+A](h: A, t: List[A]) extends List[A]

object List {


    /**
      * Funcion que calcula la longitud de una lista
      *
      * @param lst    Lista de tipo Any
      * @return       La longitud de la lista lst
      * @note         La lista puede estar vacia
      * @example      length(List(1,2,3))  -->  3
      * 
      */
    def length[A](lst: List[A]): Int = lst match {
        case Nil             => 0
        case Const(h, t)     => 1 + length(t)
    }


    /**
      * Funcion que calcula la suma de los elementos de una lista
      *
      * @param ints    Lista de enteros
      * @return        La suma de los elementos de la lista ints
      * @note          La lista puede estar vacia
      * @example       sum(List(1,2,3))  -->  6
      * 
      */
    def sum(ints: List[Int]): Int = ints match {
        case Nil             => 0
        case Const(h, t)     => h + sum(t)
    }


    /**
      * Funcion que calcula el producto de los elementos de una lista
      *
      * @param ds    Lista de valores double
      * @return      El producto de los elementos de la lista ds
      * @example     product(List(5.0,5.0))  -->  25
      *   
      */
    def product(ds: List[Double]): Double = ds match {
        case Nil             => 1
        case Const(h, t)     => h * product(t)
    }


    /**
      * Constructor de listas
      * 
      */
    def apply[A](as: A*): List[A] = {
        if(as.isEmpty) Nil
            else Const(as.head, apply(as.tail: _*))
    }


    /**
      * 
      * Ejercicio 1
      * Analisis de la expresion match
      * x vale 9 debido a la coincidencia de patrones
      * Se iguala con el tercer case, donde antes del 6, 
      * "x" se iguala a 4 y "y" se iguala 5. Finalmente, se suman
      * estos dos valores 
      * 
      */
    val x = List(4,5,6,7,8) match {
        case Const(x, Const(5, Const(7, _)))               => x
        case Nil                                           => 1
        case Const(x, Const(y, Const(6, Const(7, _))))     => x + y
        case Const(h, t)                                   => h + sum(t)
        case _                                             => 777
    }


    /**
      * Ejercicio 2
      * Funcion que remueve el primer elemento de una lista
      *
      * @param ds       Lista de tipo Any
      * @return         Lista sin el primer elemento de la lista ds
      * @note           La Lista debe contener almenos un elemento
      * @example        tail(List(1,2,3))  -->  List(2,3)
      * 
      */
    def tail[A](ds: List[A]): List[A] = ds match {
        case Const(_, t)        => t
    }


    /**
      * Ejercicio 3
      * Funcion que devuelve el primer elemento de una lista
      * 
      * @param ds     Lista de tipo Any
      * @return       Cabeza de la lista ds
      * @note         La Lista debe contener almenos un elemento
      * @example      head(List(1,2,3))  -->  1
      * 
      */
    def head[A](ds: List[A]): A = ds match {
        case Const(h, _)      => h
    }


    /**
      * Ejercicio 4
      * Funcion que valida si todos los elementos de una lista son true
      *
      * @param lst   Lista de valores booleanos
      * @return      true si todos los elementos de lst son true y false en caso contrario
      * @note        La lista puede ser vacia
      * @example     and(List(true,true,true))  -->  true
      * 
      */
    def and(lst: List[Boolean]): Boolean = lst match {
        case Nil               => true
        case Const(h, t)       => h && and(t)
    }


    /**
      * Ejercicio 5
      * Funcion que valida si todos los elementos de una lista son false
      *
      * @param lst   Lista de valores booleanos
      * @return      false si todos los elementos de lst son false y true en caso contrario
      * @note        La lista puede ser vacia
      * @example     or(List(false,false,false))  -->  false
      * 
      */
    def or(lst: List[Boolean]): Boolean = lst match {
        case Nil              => false
        case Const(h, t)      => h || or(t)
    }


    /**
      * Ejercicio 6
      * Funcion que devuelve el valor maximo de todos los valores en una lista
      *
      * @param lst   Lista de valores enteros
      * @return      Valor maximo de la lista lst
      * @example     max(List(3,8,1))  -->  8
      * 
      */
    def max(lst: List[Int]): Int = {
        @annotation.tailrec
        def maxValue(lst: List[Int], maxVal: Int): Int = lst match {
            case Nil            => maxVal
            case Const(h, t)    => if (h > maxVal) maxValue(t, h) else maxValue(t, maxVal) 
        }
        maxValue(tail(lst), head(lst).toInt)
    }


    /**
      * Ejercicio 7
      * Funcion que devuelve el valor minimo de todos los valores en una lista
      *
      * @param lst   Lista de valores Long
      * @return      Valor minimo de la lista lst
      * @example     min(List(5L,-2L,9L))  -->  -2
      * 
      */
    def min(lst: List[Long]): Long = {
        @annotation.tailrec
        def minValue(lst: List[Long], minVal: Long): Long = lst match {
            case Nil            => minVal
            case Const(h, t)    => if (h < minVal) minValue(t, h) else minValue(t, minVal)
        }
        minValue(tail(lst), head(lst).toLong)
    }


    /**
      * Ejercicio 8
      * Funcion que devuelve el valor minimo y maximo de todos los valores en una lista
      * 
      * @param lst   Lista de valores Double
      * @return      Tupla con el valor minimo y maximo de la lista lst
      * @example     minMax(List(5.0,-2.0,9.0))  -->  (-2,9)
      * 
      */
    def minMax(lst: List[Double]): (Double, Double) = {
        @annotation.tailrec
        def minMaxValues(lst: List[Double], tupla: (Double, Double)): (Double, Double) = lst match {
            case Nil           => tupla
            case Const(h, t)   => minMaxValues(t, (if (h < tupla._1) h else tupla._1, 
                                                   if (h > tupla._2) h else tupla._2))
        }
        minMaxValues(tail(lst), (head(lst).toDouble, head(lst).toDouble))
    }


}
