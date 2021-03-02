package trees

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {


 /** Ejercicios Taller M3 - Arboles */


 /** Ejercicio 11: Funcion que cuenta el numero de nodos Leaf y Branch en un arbol */
 def size[A](tree: Tree[A]): Int = tree match {
  case Leaf(_)              => 1
  case Branch(l, r)         => 1 + size(l) + size(r)
 }


 /** Ejercicio 12: Funcion que retorna la longitud maxima de profundidad desde la raiz a cualquier hoja */
 def depth[A](tree: Tree[A]): Int = tree match {
  case Leaf(_)               => 1
  case Branch(l, r)          => Math.max(depth(l), depth(r)) + 1
 }


 /** Funcion adicional: Sumar las hojas de un arbol de enteros */
 def sumarLeafs(tree: Tree[Int]): Int = tree match {
  case Leaf(value)            => value
  case Branch(l, r)           => sumarLeafs(l) + sumarLeafs(r)
 }
 
}