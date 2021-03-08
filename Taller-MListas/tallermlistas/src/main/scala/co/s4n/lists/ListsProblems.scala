package co.s4n.lists

/** 
  * Taller de listas basado en los 99 problemas de Haskell
  * @see https://wiki.haskell.org/99_questions/1_to_10
  * @author Alex Montoya Franco
  */
object ListsProblems extends App {


  /**
    * Problema 1
    * Busca el ultimo elemento de una lista dada
    * 
    * @param lst    Lista de tipo Any
    * @return       El ultimo elemento de la lista lst
    * @note         La Lista debe contener almenos un elemento
    * @example      myLast(List(1,2,3))  -->   3
    *
    */
  def myLast[A](lst: List[A]): A = lst match {
    case x :: Nil     => x
    case x :: xs      => myLast(xs)
  }


  /**
    * Problema 2
    * Busca el penultimo elemento de una lista dada
    * 
    * @param lst    Lista de tipo Any
    * @return       El penultimo elemento de la lista lst
    * @note         La Lista debe contener almenos dos elementos
    * @example      myButLast(List(1,2,3,4))  -->   3
    * 
    */
  def myButLast[A](lst: List[A]): A = lst match {
    case x :: y ::	Nil    => x
    case x :: xs           => myButLast(xs)
  }


  /**
    * Problema 3
    * Busca el n-esimo elemento de una lista dada tomando como primer indice el 1
    * 
    * @param lst    Lista de tipo Any
    * @param n      Posicion del elemento que se desea obtener
    * @return       El n-esimo elemento de la lista lst
    * @note         La Lista debe contener almenos un elemento
    * @example      elementAt(List("a","b","c","d"),3)  -->   "c"
    * 
    */
  def elementAt[A](lst: List[A], n: Int): A = (lst, n) match {
    case (x :: _, 1)    	 => x
    case (_ :: xs, n)      => elementAt(xs, n-1)
  }


  /**
    * Problema 4
    * Busca el numero de elementos de una lista dada
    * 
    * @param lst    Lista de tipo Any
    * @return       La longitud de la lista lst
    * @note         La Lista debe contener almenos un elemento
    * @example      myLength(List("a","b","c","d"))  -->  4
    * 
    */
  def myLength[A](lst: List[A]): Int = lst match {
    case x :: Nil       => 1
    case x :: xs        => 1 + myLength(xs)
  }


  /**
    * Problema 4
    * Busca el numero de elementos de una lista dada usando recursividad de cola
    * 
    * @param lst    Lista de tipo Any
    * @return       La longitud de la lista lst
    * @note         La Lista debe contener almenos un elemento
    * @example      myLengthR(List("a","b","c","d"))  -->  4
    * 
    */
  def myLengthR[A](lst: List[A]): Int = {
    @annotation.tailrec
    def aux[A](lst: List[A], n: Int): Int = lst match {
      case Nil          => n
      case x :: xs      => aux(xs, n+1)
    }
    aux(lst, 0)
  }


  /**
    * Problema 5
    * Invierte una lista dada
    * 
    * @param lst    Lista de tipo Any
    * @return       La Lista invertida
    * @note         La Lista puede ser vacia
    * @example      myReverse(List(1,2,3))  -->   List(3,2,1)
    * 
    */
  def myReverse[A](lst: List[A]): List[A] = lst match {
    case x :: xs      => myReverse(xs) ::: List(x)
    case Nil          => Nil
  }


  /**
    * Problema 6
    * Determina si una lista es palindroma, 
    * es decir, que se puede leer igual de izquierda a derecha y de 
    * derecha a izquierda
    * 
    * @param lst    Lista de tipo Any
    * @return       true si la lista es palindroma y false en caso contrario
    * @note         La Lista puede ser vacia
    * @example      isPalindrome(List(1,2,1))  -->   true
    * 
    */
  def isPalindrome[A](lst: List[A]): Boolean = lst match {
    case Nil              => true
    case x :: Nil         => true
    case x :: xs          => (x == xs.last && isPalindrome(xs.init))
  }


  /**
    * Problema 7
    * Aplana una estructura de listas anidadas
    * 
    * @param lst  Lista de tipo NestedLIst
    * @return     La Lista aplanada
    * 
    */
    
  sealed trait NestedList[+A]

  /**
    * Un Elemento de tipo NestedList
    *
    * @constructor    Crea un nuevo elemento con un valor de tipo Any
    * @param elem     Elemento
    * @example        Elem(2)
    * @todo           Terminar de implementar funcion flatten
    * 
    */
  case class Elem[A](elem: A) extends NestedList[A]

  case class Nested[A](lst: List[NestedList[A]]) extends NestedList[A]

  /*
  def flatten[A](lst: NestedList[A]): List[A] = lst match {
    case Elem(elem)             => List(elem)
    case Nested(nestedList)     => nestedList.flatMap(flatten(nestedList))
  }
  */


}