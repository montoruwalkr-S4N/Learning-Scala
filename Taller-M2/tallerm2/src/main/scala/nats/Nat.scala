package nats


/** Ejercicios Taller M2 - Definicion de tipos algebraicos - Naturales  */


/** Ejercicio 9: Implementacion de Nat en Scala */

sealed trait Nat

case object Cero extends Nat

case class Suc(nat: Nat) extends Nat


object Nat {

 /** Ejercicio 10: Funcion que recibe un numero natural Nat y retorna su equivalente en Int  */
 def fromNatToInt(nat: Nat): Int = nat match {
  case Cero => 0
  case Suc(nat) => 1 + fromNatToInt(nat)
 }


 /** Ejercicio 11: Funcion que recibe un valor entero positivo (incluye 0) y retorna su equivalente en numero natural Nat */
 def fromIntToNat(n: Int): Nat = {

  @annotation.tailrec
  def aux(n: Int, nat: Nat): Nat = n match {
   case 0 => nat
   case n => aux(n-1, Suc(nat))
  }
  aux(n,Cero)

 }

}