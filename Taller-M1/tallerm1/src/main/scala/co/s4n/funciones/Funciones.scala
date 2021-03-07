package co.s4n.funciones

/** 
  * Taller de Funciones y Recursion en Scala
  * @author Alex Montoya Franco
  */
object Funciones extends App {


    /**
     * Ejercicio 1
     * Funcion literal que calcula el area de un triangulo rectangulo
     * 
     * @param a    Lado recto del triangulo
     * @param b    Lado recto del triangulo
     * @return     Area del triangulo rectangulo
     * @example    areaTrianguloRectangulo(2,5)  -->   5
     */
    object Ejercicio1 {
        val areaTrianguloRectangulo = (a: Int, b: Int) => { 
            b * a / 2 
        }
    }


    /**
     * Ejercicio 2
     * Funcion literal que calcula el area de un circulo
     * 
     * @param r    Radio del circulo
     * @return     Area del circulo
     * @example    areaDeUnCirculo(2)  -->   12.56
     */
    object Ejercicio2 {
        import scala.math.Pi
        import scala.math.pow

        val areaDeUnCirculo = new Function1[Double, Double] {
            def apply(r: Double): Double = { 
                Pi * pow(r,2) 
            }
        }
    }


    /**
     * Ejercicio 3
     * Funcion literal que calcula el salario de una persona
     * 
     * @param devengado     Salario base de la persona
     * @param deducciones   Deducciones al salario base
     * @return              Salario final de la persona
     * @example             calSalario(5000,300)  -->   4700
     */
    object Ejercicio3 {
        val calSalario = (devengado: Double, deducciones: Double) => { 
            devengado - deducciones 
        }
    }


    /**
     * Ejercicio 4
     * Funcion literal que calcula el salario de una persona considerando 
     * un bono del 10%
     * 
     * @param devengado     Salario base de la persona
     * @param deducciones   Deducciones al salario base
     * @return              Salario final de la persona con bono del 10%
     * @example             calSalario(5000,300)  -->   5200
     */
    object Ejercicio4 {
        val calSalarioBono = (devengado: Double, deducciones: Double) => { 
            devengado * 1.10 - deducciones 
        }
    }


    /**
     * Ejercicio 5
     * Funcion de alto orden que calcula salarios de personas usando funciones
     * previamente definidas
     * 
     * @param f             Funcion que calcula salario
     * @param devengado     Salario base de la persona
     * @param deducciones   Deducciones al salario base
     * @return              Salario final de la persona usando la funcion f
     * @example             compSalario(Ejercicio3.calSalario, 5000, 300)     -->   4700
     * @example             compSalario(Ejercicio4.calSalarioBono, 5000, 300) -->   5200
     */
    object Ejercicio5 {
        def compSalario(f: (Double, Double) => Double, 
                        devengado: Double, 
                        deducciones: Double) = f(devengado, deducciones)
    }


    /**
     * Ejercicio 6
     * Funcion de alto orden que genera funciones para calcular salarios usando
     * diferentes bonos
     * 
     * @param bono          Bono para adicionar al salario final
     * @return              Funcion que calcula salario usando bono
     */
    object Ejercicio6 {
        def genCalSalarioBono(bono: Double): (Double, Double) => Double = {
            (devengado: Double, deducciones: Double) => {
                devengado * bono - deducciones
            }
        }
    }


    /**
     * Ejercicio 7
     * Funcion literal que da un bono del 5% usando el generador de 
     * funciones genCalSalarioBono
     * 
     * @return              Salario de una persona con bono del 5%
     */
    object Ejercicio7 {
        val calSalario5 = Ejercicio6.genCalSalarioBono(0.5)
    }


    /**
     * Ejercicio 8
     * Funcion literal que da un bono del 20% usando el generador de 
     * funciones genCalSalarioBono
     * 
     * @return              Salario de una persona con bono del 20%
     */
    object Ejercicio8 {
        val calSalario20 = Ejercicio6.genCalSalarioBono(0.20)
    }


    /**
     * Ejercicio 9
     * Funcion que calcula el salario de una persona usando un bono de tipo clausura
     * 
     * @param devengado     Salario base de la persona
     * @param deducciones   Deducciones al salario base
     * @return              Salario final de la persona usando el bono externo
     * @example             calSalarioBonoClausura(5000,300)   -->   700
     */
    object Ejercicio9 {
        val bono = 0.20
        def calSalarioBonoClausura(devengado: Double, deducciones: Double) =  { 
            devengado * bono - deducciones 
        }
    }


    /**
     * Ejercicio 11
     * Funcion que aplica parcialmente genCalSalarioBono con un valor del 15%
     * 
     * @return              Funcion que calcula salario usando bono del 15%
     * @example             calSalario15(5000,300)   -->   5450
     */
    object Ejercicio11 {
        val tmp = Ejercicio6.genCalSalarioBono(_)
        val calSalario15 = tmp(1.15)
    }


    /**
     * Ejercicio 12
     * Funcion que aplica parcialmente genCalSalarioBono con un valor del 20%
     * 
     * @return              Funcion que calcula salario usando bono del 20%
     * @example             calSalario20(5000,300)   -->   9700
     */
    object Ejercicio12 {
        val tmp = Ejercicio6.genCalSalarioBono(_)
        val calSalario20 = tmp(2.0)
    }


    /**
     * Ejercicio 15
     * Funcion que calcula el factorial de un numero usando recursividad
     * 
     * @param n             Numero para calcular factorial
     * @return              Factorial de n
     * @example             factorial(7)  -->   5040
     */
    object Ejercicio15 {
        def factorial(n: Int): Int = n match {
            case 0       => 1
            case 1       => 1
            case _       => n * factorial(n-1)
        } 
    }


    /**
     * Ejercicio 16
     * Funcion que calcula el fibonacci de un numero usando recursividad
     * 
     * @param n             Numero para calcular fibonacci
     * @return              Fibonacci de n
     * @example             fibonacci(9)  -->   34
     */
    object Ejercicio16 {
        def fibonacci(n: Int): Int = n match {
            case 0       => 0
            case 1       => 1
            case _       => fibonacci(n-1) + fibonacci(n-2)
        }
    }


    /**
     * Ejercicio 17
     * Funcion que calcula el factorial de un numero usando recursividad de cola
     * 
     * @param n             Numero para calcular factorial
     * @return              Factorial de n
     * @example             factorial(3)  -->   6
     */
    object Ejercicio17 {
        def factorial(n: Int): Int = {
            @annotation.tailrec
            def aux(n: Int, a:Int): Int = n match {
                case 0        => a
                case 1        => a
                case n        => aux(n-1,n*a)
            }
            aux(n,1)
        } 
    }

    
}