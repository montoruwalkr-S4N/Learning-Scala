package ejercicios

/** Funcion literal que recibe los dos lados rectos de un triangulo rectangulo y calcula su area */
object Ejercicio1 {

 val areaTrianguloRectangulo = (a: Int, b: Int) => { b * a / 2 }

}


/** Funcion literal que recibe el radio de un circulo y calcula su area */
object Ejercicio2 {

 import scala.math.Pi
 import scala.math.pow
 
 val areaDeUnCirculo = new Function1[Double, Double] {
  def apply(r: Double): Double = { Pi * pow(r,2) }

 }
}


/** Funcion literal que recibe el devengado y las deducciones de una persona y retorna su salario */
object Ejercicio3 {

 val calSalario = (devengado: Double, deducciones: Double) => { devengado - deducciones }

}


/** Funcion literal que recibe el devengado y las deducciones de una persona y retorna su salario teniendo en cuenta el valor de un bono */
object Ejercicio4 {

 val calSalarioBono = (devengado: Double, deducciones: Double) => { devengado * 1.10 - deducciones }

}


/** Funcion de alto orden que recibe una funcion que calcula salario, el devengado y las deducciones de una persona y retorna su salario */
object Ejercicio5 {

 def compSalario(f: (Double, Double) => Double, devengado: Double, deducciones: Double) = f(devengado, deducciones)

}


/** Funcion que genera funciones que computan diferentes bonos  */
object Ejercicio6 {

 def genCalSalarioBono(bono: Double): (Double, Double) => Double = {
  (devengado: Double, deducciones: Double) => {
   devengado * bono - deducciones
  }
 }

}


/** Funcion literal que da un bono del 5% usando el generador de funciones del Ejercicio 6  */
object Ejercicio7 {

 val calSalario5 = Ejercicio6.genCalSalarioBono(0.5)

}


/** Funcion literal que da un bono del 20% usando el generador de funciones del Ejercicio 6  */
object Ejercicio8 {

 val calSalario20 = Ejercicio6.genCalSalarioBono(0.20)

}


/** Funcion que recibe el devengado y las deducciones de una persona y retorna su salario usando un bono externo (clausura) */
object Ejercicio9 {

 val bono = 0.20
 
 def calSalarioBonoClausura(devengado: Double, deducciones: Double) =  { devengado * bono - deducciones }

}


/** Ejercicio 10 se encuentra en el fichero de tests */


/** Aplicacion parcial de genCalSalarioBono con un valor del 15% */
object Ejercicio11 {

 val tmp = Ejercicio6.genCalSalarioBono(_)
 val calSalario15 = tmp(1.15)
 
}


/** Aplicacion parcial de genCalSalarioBono con un valor del 20% */
object Ejercicio12 {

 val tmp = Ejercicio6.genCalSalarioBono(_)
 val calSalario20 = tmp(2.0)

}


/** Ejercicio 13 tiene error de formacion */


/** Ejercicio 14 tiene error de formacion */


/** Funcion de factorial utilizando recursividad  */
object Ejercicio15 {

 def factorial(n: Int): Int = n match {
  case 0 => 1
  case 1 => 1
  case _ => n * factorial(n-1)
 } 

}


/** Funcion de fibonacci de forma recursiva*/
object Ejercicio16 {

 def fibonacci(n: Int): Int = n match {
  case 0 => 0
  case 1 => 1
  case _ => fibonacci(n-1) + fibonacci(n-2)
 }

}


/** Funcion de factorial bajo recursividad de cola  */
object Ejercicio17 {

 def factorial(n: Int): Int = {

  @annotation.tailrec
  def factorial2(n: Int, a:Int): Int = n match {
   case 0 => a
   case 1 => a
   case n => factorial2(n-1,n*a)
  }
  factorial2(n,1)

 } 

}


