package co.s4n.lists

/**
  * Taller de construccion de listas y Funciones de alto orden
  * @author Alex Montoya Franco
  */

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Const[+A](h: A, t: List[A]) extends List[A]

object List {


  /**
    * Constructor de listas
    * 
    */
  def apply[A](as: A*) : List[A] ={
    if (as.isEmpty) Nil
      else Const(as.head, apply(as.tail: _*))
  }


  /**
    * Funcion que construye una lista    
    * 
    * @param h        Cabeza de la lista
    * @param t        Cola de la lista
    * @return         Lista construida a partir de h y t
    * @example        const(0,List(1,2,3)) --> List(0,1,2,3)
    *
    */
  def const[A](h: A, t: List[A]): List[A] = Const(h, t)


  /**
    * Funcion que añade un elemento al final de una lista
    *
    * @param lst      Lista de tipo Any
    * @param elem     Elemento para añadir a la lista lst
    * @return         Lista con el elemento elem al final de lst
    * @example        addEnd(List(1,3,5),8) --> List(1,3,5,8)
    * 
    */
  def addEnd[A](lst: List[A], elem: A): List[A] = lst match {
    case Nil              => Const(elem, Nil)
    case Const(h, t)      => Const(h, addEnd(t, elem))
  }


  /**
    * Funcion que concatena dos listas
    *
    * @param lst1   Primera lista a concatenar
    * @param lst2   Segunda lista a concatenar
    * @return       Lista resultado de concatenar lst1 con lst2
    * @example      append(List(1,2), List(3,4)) --> List(1,2,3,4)
    * 
    */
  def append[A](lst1: List[A], lst2: List[A]): List[A] = (lst1, lst2) match {
    case (Nil, Nil)             => Nil
    case (lst1, Nil)            => lst1
    case (Nil, lst2)            => lst2
    case (Const(h, t), lst2)    => Const(h, append(t, lst2))
  }


  /**
    * Ejercicio 1
    * Funcion que toma los n-primeros valores de una lista
    *
    * @param n       Entero que identifica cuantos elementos se quieren tomar
    * @param lst     Lista de tipo Any
    * @return        Lista con los primeros n-elementos de lst
    * @example       take(3, List("a","b","c","d","e")) --> List("a","b","c")
    * 
    */
  def take[A](n: Int, lst: List[A]): List[A] = {
    @annotation.tailrec
    def aux[A](n: Int, lst: List[A], acum: List[A]): List[A] = (n, lst) match {
      case (0, _)                => acum
      case (n, Nil)              => acum
      case (n, Const(h, t))      => aux(n-1, t, addEnd(acum, h))
    }
    aux(n, lst, Nil)
  }


  /**
    * Ejercicio 2
    * Funcion que toma todos los elementos de una lista excepto el ultimo
    *
    * @param lst    Lista de tipo Any
    * @return       Lista con todos los elementos de lst menos el ultimo
    * @note         La lista que se pasa no puede ser vacia
    * @example      init(List(1,2,3,4,5,6)) --> List(1,2,3,4,5)
    * 
    */
  def init[A](lst: List[A]): List[A] = {
    @annotation.tailrec
    def aux[A](lst: List[A], acum: List[A]): List[A] = lst match {
      case Const(h, Nil)         => acum
      case Const(h, t)           => aux(t, addEnd(acum, h))
    }
    aux(lst, Nil)
  }


  /**
    * Ejercicio 3
    * Funcion que divide una lista en dos dado un parametro n, la primera
    * lista queda de n elementos y la segunda queda con los elementos restantes
    *
    * @param n      Entero que indica la particion deseada para la lista
    * @param lst    Lista de tipo Any
    * @return       Tupla con dos listas
    * @example      split(3, List(1,2,3,4,5,6,7)) --> (List(1,2,3),List(4,5,6,7))
    * 
    */
  def split[A](n: Int, lst: List[A]): (List[A], List[A]) = {
    @annotation.tailrec
    def aux[A](n: Int, lst: List[A], acum: List[A]): (List[A], List[A]) = (n, lst) match {
      case (n, Nil)                  => (acum, Nil)
      case (0, lst)                  => (acum, lst)
      case (n, Const(h, t))          => aux(n-1, t, addEnd(acum, h))
    }
    aux(n, lst, Nil)
  }


  /**
    * Ejercicio 4
    * Funcion que fusiona dos listas de tipos diferentes en una lista de pares del mismo tamaño
    *
    * @param lst1     Lista de tipo A
    * @param lst2     Listo de tipo B
    * @return         Lista de tuplas con los primeros elementos de tipo A y los segundos de tipo B
    * @example        zip(List(1,2,3),List(true,false,true,true)) --> List((1,true),(2,false),(3,true))
    * 
    */
  def zip[A,B](lst1: List[A], lst2: List[B]): List[(A,B)] = {
    @annotation.tailrec
    def aux[A,B](lst1: List[A], lst2: List[B], acum: List[(A,B)]): List[(A,B)] = (lst1, lst2) match {
      case (lst1, Nil)                       => acum
      case (Nil, lst2)                       => acum
      case (Const(h1, t1), Const(h2, t2))    => aux(t1, t2, addEnd(acum, (h1, h2)))
    }
    aux(lst1, lst2, Nil)
  }


  /**
    * Ejercicio 5
    * Funcion que separa una lista de tuplas en dos listas distintas
    *
    * @param lst     Lista de tuplas de tipo (A,B)
    * @return        Dos listas: Lista de tipo A y Listo de tipo B
    * @example       unzip(List((1,"a"),(2,"b"),(3,"c"))) --> (List(1,2,3), List("a","b","c"))
    * 
    */
  def unzip[A,B](lst: List[(A,B)]): (List[A], List[B]) = {
    @annotation.tailrec
    def aux[A,B](lst: List[(A,B)], acum1: List[A], acum2: List[B]): (List[A], List[B]) = lst match {
      case Nil                           => (acum1, acum2)
      case Const((h1, h2), t)            => aux(t, addEnd(acum1, h1), addEnd(acum2, h2))
    }
    aux(lst, Nil, Nil)
  }


  /**
    * Ejercicio 6
    * Funcion que devuelve la version invertida de una lsita dada
    *
    * @param lst     Lista de tipo A
    * @return        Lista lst invertida
    * @example       reverse(List("a","b","c")) --> List("c","b","a")
    * 
    */
  def reverse[A](lst: List[A]): List[A] = lst match {
    case Nil                 => Nil
    case Const(h, t)         => addEnd(reverse(t), h)
  }


  /**
    * Ejercicio 7
    * Funcion que entremezcla un valor entre los elementos de una lista
    * 
    * @param elem     Elemento de tipo A a entremezclar
    * @param lst      Lista de tipo A
    * @return         Lista con el elemento elem intercalado entre los valores de la lista lst
    * @example        intersperse(1, List(2,3,4,5)) --> List(2,1,3,1,4,1,5)
    * 
    */
  def intersperse[A](elem: A, lst: List[A]): List[A] = lst match {
    case Nil                => Nil
    case Const(h, Nil)      => Const(h, Nil)
    case Const(h, t)        => Const(h, Const(elem, intersperse(elem, t)))
  }


  /**
    * Ejercicio 8
    * Funcion que recibe una lista de listas de valores de tipo A y la transforma en una lista de 
    * valores de tipo A
    * 
    * @param lst     Lista de listas de tipo A
    * @return        Lista de tipo A basada en lst
    * @example       concat(List(List(1,2,3),List(4,5,6))) --> List(1,2,3,4,5,6)
    * 
    */
  def concat[A](lst: List[List[A]]): List[A] = lst match {
    case Nil               => Nil
    case Const(h, t)       => append(h, concat(t))
  }



  /**
    * Funcion que pliega una lista hacia la derecha aplicando una funcion determinada
    *
    * @param as     Lista de tipo A
    * @param z      Valor base
    * @param f      Funcion a aplicar
    * @return       Resultado de aplicar la funcion f sobre la lista as
    * 
    */
  def foldRight[A,B](as: List[A], z: B)(f: (A,B) => B): B = as match {
    case Nil                  => z
    case Const(h, t)          => f(h, foldRight(t, z)(f))
  }


  /**
    * Ejercicio 14
    * Funcion que devuelve la longitud de una lista usando foldRight
    *
    * @param lst    Lista de tipo A
    * @return       Longitud de la lista lst
    * @example      length(List(1,2,3)) --> 3
    * 
    */
  def length[A](lst: List[A]): Int = foldRight(lst, 0)((_, y) => y + 1)


  /**
    * Ejercicio 15
    * Funcion and usando foldRight
    *
    * @param lst   Lista de valores booleanos
    * @return      True si todos los elementos de la lista lst son true
    * @example     and(List(true,true,true)) --> true
    * 
    */
  def and(lst: List[Boolean]): Boolean = foldRight(lst, true)((x, y) => x && y)

  def and2(lst: List[Boolean]): Boolean = foldRight(lst, true)(_&&_)


  /**
    * Ejercicio 16
    * Funcion que retorna el prefijo mas largo que satisface un predicado p
    *
    * @param lst    Lista de tipo A
    * @param p      Predicado
    * @return       Prefijo mas largo que satisface p
    * @note         El prefijo mas largo puede ser vacio
    * @example      takeWhile(List(1,4,9))(_<5) --> List(1,4)
    * 
    */
  def takeWhile[A](lst: List[A])(p: A => Boolean): List[A] = 
      foldRight(lst, Nil: List[A])((h, t) => if (p(h)) Const(h, t) else Nil)


  /**
    * Ejercicio 17
    * Funcion que filtra los elementos de una lista que satisfacen un predicado p
    *
    * @param lst    Lista de tipo A
    * @param p      Predicado
    * @return       Lista con los elementos que satisfacen p
    * @example      filter(List(1,4,1))(_<3) --> List(1,1)
    * 
    */
  def filter[A](lst: List[A])(p: A => Boolean): List[A] =
      foldRight(lst, Nil: List[A])((h, t) => if (p(h)) Const(h, t) else t)


  /**
    * Ejercicio 18
    * Funcion que separa una lista de tuplas en dos listas distintas
    *
    * @param lst    Lista de tuplas de tipo (A,B)
    * @return       Lista de tipo A y Lista de tipo B
    * @example      unzipR(List((1,"a"),(2,"b"),(3,"c"))) --> (List(1,2,3), List("a","b","c"))
    * 
    */
  def unzipR[A,B](lst: List[(A,B)]): (List[A], List[B]) =
      foldRight(lst, (Nil, Nil): (List[A], List[B]))((h, t) => (Const(h._1, t._1), Const(h._2, t._2)))



  /**
    * Funcion que pliega una lista hacia la izquierda aplicando una funcion determinada
    *
    * @param lst    Lista de tipo A
    * @param z      Valor base
    * @param f      Funcion a aplicar
    * @return       Resultado de aplicar la funcion f sobre la lista lst
    * 
    */
  def foldLeft[A,B](lst: List[A], z: B)(f: (B,A) => B): B = lst match {
    case Const(h, t)         => foldLeft(t, f(z, h))(f)
    case Nil                 => z
  }

  /**
    * Ejercicio 19
    * Funcion que devuelve la longitud de una lista usando foldLeft
    *
    * @param lst   Lista de tipo A
    * @return      Longitud de la lista lst
    * @example     lengthL(List(1,2,3)) --> 3
    * 
    */
  def lengthL[A](lst: List[A]): Int = foldLeft(lst, 0)((y, _) => 1+y) 


  /**
    * Ejercicio 20
    * Funcion and usando foldLeft
    *
    * @param lst   Lista de valores booleanos
    * @return      True si todos los elementos de la lista lst son true
    * @example     andL(List(true,true,true)) --> true
    * 
    */
  def andL(lst: List[Boolean]): Boolean = foldLeft(lst, true)(_&&_)


  /**
    * Ejercicio 21
    * Funcion que retorna el prefijo mas largo que satisface un predicado p
    * usando foldLeft
    *
    * @param lst    Lista de tipo A
    * @param p      Predicado
    * @return       Prefijo mas largo que satisface p
    * @note         El prefijo mas largo puede ser vacio
    * @example      takeWhileL(List(1,2,3,4,5,6))(_<4) --> List(1,2,3)
    * @example      takeWhileL(List("abc","abcbe","a","","aaaaa"))(_.length <= 3) --> List("abc")
    * 
    */
  def takeWhileL[A](lst: List[A])(p: A => Boolean): List[A] = {
    def f(b: (Boolean, List[A]), a: A): (Boolean, List[A]) = b match {
      case (true, lst)        => if (p(a)) (true, addEnd(lst, a)) else (false, lst)
      case (false, lst)       => b
    }
    foldLeft(lst, (true, Nil: List[A]))(f)._2
  }

  /**
    * Ejercicio 22
    * Funcion que filtra los elementos de una lista que satisfacen un predicado p
    * usando foldLeft
    *
    * @param lst    Lista de tipo A
    * @param p      Predicado
    * @return       Lista con los elementos que satisfacen p
    * @example      filterL(List(1,4,1))(_<3) --> List(1,1)
    * 
    */
  def filterL[A](lst: List[A])(p: A => Boolean): List[A] =
      foldLeft(lst, Nil: List[A])((lst, e) => if (p(e)) addEnd(lst, e) else lst)


  /**
    * Ejercicio 23
    * Funcion que separa una lista de tuplas en dos listas distintas usando foldLeft
    *
    * @param lst    Lista de tuplas de tipo (A,B)
    * @return       Lista de tipo A y Lista de tipo B
    * @example      unzipL(List((1,"a"),(2,"b"),(3,"c"))) --> (List(1,2,3), List("a","b","c"))
    * 
    */
  def unzipL[A,B](lst: List[(A,B)]): (List[A], List[B]) =
      foldLeft(lst, (Nil, Nil): (List[A], List[B]))((lst, e) => (addEnd(lst._1, e._1), addEnd(lst._2, e._2)))


}



