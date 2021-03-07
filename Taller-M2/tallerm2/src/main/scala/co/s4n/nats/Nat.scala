package co.s4n.nats

/**
  * Taller de tipos de datos inmutables
  * Definicion de tipos algebraicos - Naturales
  * @author Alex Montoya Franco
  */

/**
  * Ejercicio 9
  * Implementacion de Nat en Scala
  * 
  */
sealed trait Nat
case object Cero extends Nat
case class Suc(nat: Nat) extends Nat

object Nat {


    /**
      * Ejercicio 10
      * Funcion que devuelve el equivalente en Int de un natural Nat
      *
      * @param nat    Natural que puede ser Cero o Suc(Nat)
      * @return       Entero equivalente al natural nat
      * @example      fromNatToInt(Suc(Cero))  -->  1
      * 
      */
    def fromNatToInt(nat: Nat): Int = nat match {
        case Cero         => 0
        case Suc(nat)     => 1 + fromNatToInt(nat)
    }


    /**
      * Ejercicio 11
      * Funcion que produce naturales Nat equivalentes a un entero dado
      *
      * @param n    Numero entero positivo (incluyendo el 0)
      * @return     Natural Nat equivalente al entero n
      * @example    fromIntToNat(1)  -->  Suc(Cero)
      * 
      */
    def fromIntToNat(n: Int): Nat = {
        @annotation.tailrec
        def aux(n: Int, nat: Nat): Nat = n match {
            case 0       => nat
            case n       => aux(n-1, Suc(nat))
        }
        aux(n, Cero)
    }


}