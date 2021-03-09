package co.s4n.forlists

/** 
  * Taller de listas usando listas de comprension(for) basado en los 99 problemas de Haskell
  * @see https://wiki.haskell.org/99_questions/1_to_10
  * @author Alex Montoya Franco
  */
object ForListsProblems extends App {
  

  /**
    * Funcion que retorna la longitud de una lista
    *
    * @param lst   Lista de cualquier tipo
    * @return      Longitud de la lista lst
    * @example     myLongitud(List(2,4,6,8,10)) --> 5
    * @example     myLongitud(List())  --> 0
    * 
    */
  def myLongitud[A](lst: List[A]): Int = (for {
    _ <- lst
  } yield ((a: Int) => a + 1)).foldLeft(0)((e,f) => f(e))


  /**
    * Funcion que retorna el ultimo elemento de una lista
    *
    * @param lst  Lista de cualquier tipo
    * @return     El ultimo elemento de la lista lst
    * @example    myLastFor(List(1,2,3,4,5)) --> 5
    * 
    */
  def myLastFor[A](lst: List[A]): A = (for {
    xi <- lst
  } yield((a: A) => xi)).foldLeft(lst.head)((e,f) => f(e))
  
  
  /**
    * Funcion que retorna el primer elemento de una lista
    *
    * @param lst  Lista de cualquier tipo
    * @return     El primero elemento de la lista lst
    * @example    myHeadFor(List(1,2,3,4,5)) --> 1
    * 
    */
  def myHeadFor[A](lst: List[A]): A = (for {
    xi <- lst
  } yield((a: A) => xi)).foldRight(lst.head)((f,e) => f(e))


  /**
    * Funcion que suma los elementos de una lista
    *
    * @param lst  Lista de tipo Entero
    * @return     La suma de los elementos de la lista lst
    * @example    mySumFor(List(5,10))  --> 15
    * 
    */
  def mySumFor(lst: List[Int]): Int = (for {
    xi <- lst
  } yield((elem: Int) => elem + xi)).foldLeft(0)((e,f) => f(e))


  /**
    * Funcion que retorna el k-esimo elemento de una lista empezando por el indice 0
    *
    * @param k       Entero indicando la posicion del elemento que se desea obtener
    * @param lst     Lista de cualquier tipo
    * @return        K-esimo elemento de la lista lst
    * @example       myKthElem(0,List(3,6,9))  --> 3
    * @example       myKthElem(2,List(3,6,9))  --> 9
    * @example       myKthElem(4,List(3,6,9))  --> -1
    * 
    */
  def myKthElem[A](k: Int, lst: List[A]) = (for {
    xi <- lst
  } yield ((t:(Int, Option[A])) => (t._2) match {
                                      case None    => if (t._1 == k) (t._1, Some(xi)) else (t._1+1, None)
                                      case Some(x) => (t._1, Some(x))
                                })).foldLeft((0, None: Option[A]))((e,f) => f(e))._2.getOrElse(-1)


  /**
    * Funcion que copia los elementos de una lista en otra lista
    *
    * @param lst  Lista de cualquier tipo a copia
    * @return     Lista copiada
    * @example    copyFor(List(1,2,3,4,5)) --> List(1, 2, 3, 4, 5)
    * 
    */
  def copyFor[A](lst: List[A]): List[A] = (for {
    xi <- lst
  } yield ((xs: List[A]) => xi :: xs)).foldRight(Nil: List[A])((f,e) => f(e))


  /**
    * Funcion que invierte una lista
    *
    * @param lst    Lista de cualquier tipo
    * @return       Lista invertida
    * @example      reverseFor(List(1,2,3))  --> List(3,2,1)
    * @example      reverseFor(List(false,false,true)) --> List(true,false,false)
    * 
    */
  def reverseFor[A](lst: List[A]): List[A] = (for {
    xi <- lst
  } yield ((xs: List[A]) => xs ::: List(xi))).foldRight(Nil: List[A])((f,e) => f(e))


  def reverseForL[A](lst: List[A]): List[A] = (for {
    xi <- lst
  } yield ((xs: List[A]) => xs ::: List(xi))).foldLeft(Nil: List[A])((e,f) => f(e))




}