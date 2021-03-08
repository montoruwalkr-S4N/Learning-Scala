package co.s4n.nats

/**
  * Taller de construccion de Listas - Naturales
  * @author Alex Montoya Franco
  */

sealed trait Nat
case object Cero extends Nat
case class Suc(nat: Nat) extends Nat

object Nat {


    /**
      * Ejercicio 9
      * Funcion que suma dos naturales Nat
      *
      * @param nat1     Primer natural Nat a sumar
      * @param nat2     Segundo natural Nat a sumar
      * @return         Suma de nat1 con nat2
      * @example        addNat(Cero, Suc(Cero)) --> Suc(Cero)
      * 
      */
    def addNat(nat1: Nat, nat2: Nat): Nat = nat1 match {
        case Cero             => nat2
        case Suc(n)           => addNat(n, Suc(nat2))
    }


    /**
      * Ejercicio 10
      * Funcion que multiplica dos naturales Nat
      * 
      * @param nat1     Primer natural Nat a multiplicar
      * @param nat2     Segundo natural Nat a multiplicar
      * @return         Multiplicacion de nat1 con nat2
      * @example        prodNat(Cero, Suc(Cero)) --> Cero
      * 
      */
    def prodNat(nat1: Nat, nat2: Nat): Nat = {
        @annotation.tailrec
        def aux(nat1: Nat, nat2: Nat, acum: Nat): Nat = nat1 match {
            case Cero           => Cero
            case Suc(Cero)      => acum
            case Suc(n)         => aux(n, nat2, addNat(acum, nat2))
        }
        aux(nat1, nat2, nat2)
    } 


}