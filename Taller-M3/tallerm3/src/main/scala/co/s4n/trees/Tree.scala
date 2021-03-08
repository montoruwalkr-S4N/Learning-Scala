package co.s4n.trees

/**
  * Taller de construccion de Listas - Arboles
  * @author Alex Montoya Franco
  */

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {


    /**
      * Ejercicio 11
      * Funcion que cuenta el numero de nodos Leaf y Branch en un arbol 
      *
      * @param tree     Arbol de tipo A
      * @return         Entero con la cantidad de hojas y ramas del arbol tree
      * @note           No se permiten arboles vacios
      * @example        size(Branch(Leaf(10),Leaf(20))) --> 3
      * 
      */
    def size[A](tree: Tree[A]): Int = tree match {
        case Leaf(_)              => 1
        case Branch(l, r)         => 1 + size(l) + size(r)
    }


    /**
      * Ejercicio 12
      * Funcion que retorna la longitud maxima de profundidad desde la raiz a cualquier hoja
      *
      * @param tree    Arbol de tipo A
      * @return        Longitud maxima del arbol tree
      * @note          No se permiten arboles vacios
      * @example       depth(Leaf(10)) --> 1
      * 
      */
    def depth[A](tree: Tree[A]): Int = tree match {
        case Leaf(_)              => 1
        case Branch(l, r)         => Math.max(depth(l), depth(r)) + 1
    }


    /**
      * Funcion que suma las hojas de un arbol de enteros
      *
      * @param tree    Arbol de valores enteros
      * @return        Suma de los valores de las hojas del arbol tree
      * @note          No se permiten arboles vacios
      * 
      */
    def sumarLeafs(tree: Tree[Int]): Int = tree match {
        case Leaf(value)            => value
        case Branch(l, r)           => sumarLeafs(l) + sumarLeafs(r)
    }
 

}