package nats

sealed trait Nat

case object Cero extends Nat

case class Suc(nat: Nat) extends Nat


object Nat {

 /** Funcion que recibe un numero natural Nat y retorna su equivalente en Int  */
 def fromNatToInt(nat: Nat): Int = nat match {
  case Cero => 0
  case Suc(nat) => 1 + fromNatToInt(nat)
 }


 /** Funcion que recibe un valor entero positivo (incluye 0) y retorna su equivalente en numero natural Nat */
 def fromIntToNat(n: Int): Nat = {

  @annotation.tailrec
  def aux(n: Int, nat: Nat): Nat = n match {
   case 0 => nat
   case n => aux(n-1, Suc(nat))
  }
  aux(n,Cero)

 }


 /** Ejercicios Taller M3 - Naturales  */


 /** Ejercicio 9: Funcion que recibe dos Naturales (Nat) y se encarga de sumarlos */
 def addNat(nat1: Nat, nat2: Nat) : Nat = nat1 match {
  case Cero             => nat2
  case Suc(n)           => addNat(n, Suc(nat2))
 }

 /** Ejercicio 10: Funcion que recibe dos Naturales (Nat) y se encarga de multiplicarlos */
 def prodNat(nat1: Nat, nat2: Nat): Nat = {
  def aux(nat1: Nat, nat2: Nat, acum: Nat) : Nat = nat1 match {
   case Cero           => Cero
   case Suc(Cero)      => acum
   case Suc(n)         => aux(n, nat2, addNat(acum, nat2))
  }
  aux(nat1, nat2, nat2)
 } 



}