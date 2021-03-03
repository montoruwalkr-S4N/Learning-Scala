/** Ejercicios Taller-M4 - Programacion Orientada a Objetos en Scala  */

/** Literales objetos  */

/** Ejercicio 1 */
object comp {

 def cuadrado(n: Float) = {
  n * n
 }
 
 def cubo(n: Double) = {
  n * cuadrado(n.toFloat)
 }

}
// comp.cubo(3)


/** Ejercicio 2 */
object comp2 {

 def cuadrado(n: Long) = {
  n * n
 }

 def cubo(n: Long) = {
  n * cuadrado(n)
 }

}
// comp2.cubo(3)



/** Ejercicio 3 */
object prueba {

 def x = {
  println("x")
  1
 }
 
 val y = {
  println("y")
  x + 2
 }

 def z = {
  println("z")
  x
  //x + "c"
 }

}

/**  Ejercicio 4 */
class Gato(val nombre: String, val color: String, val comida: String)

val gato1 = new Gato("Io", "Fawn", "Churrus")
val gato2 = new Gato("Make", "Red", "Leche")
val gato3 = new Gato("Docker", "Blue", "Cuido")


/** Ejercicio 5  */
object VentaDeChurrus {

 def despachar(gato: Gato): Boolean = gato.comida == "Churrus"

}


/** Ejercicio 6  */
class Conductor(val nombre: String, val apellido: String, val totalCarreras: Int, val carrerasTerminadas: Int) {
 def getCarrerasNoTerminadas():Int = totalCarreras - carrerasTerminadas
}

class Escuderia(val nombre: String, val conductor: Conductor)

// val conductor1 = new Conductor("David", "Rios", 5, 2)
// conductor1.nombre
// conductor1.getCarrerasNoTerminadas()


/** Ejercicio 7 */
class Contador(val contador: Int) {
 def incr(): Contador = new Contador(contador + 1)
 def decr(): Contador = new Contador(contador - 1)
}
// new Contador(10).incr.decr.incr.incr.contador


/** Ejercicio 8 */
class Contador2(val contador: Int) {
 def incr(param: Int = 1): Contador2 =	new Contador2(contador + param)
 def decr(param: Int = 1): Contador2 =	new Contador2(contador - param)
}
// new Contador2(10).incr().contador
// new Contador2(10).incr(5).contador


/** Ejercicio 9 */
class Sumador(monto: Int) {
 def adicionar(valor: Int) = valor + monto
}