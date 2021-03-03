package classes

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
// prueba.x + prueba.y + prueba.z

/**  Ejercicio 4 */
class Gato(val nombre: String, val color: String, val comida: String)

// val gato1 = new Gato("Io", "Fawn", "Churrus")
// val gato2 = new Gato("Make", "Red", "Leche")
// val gato3 = new Gato("Docker", "Blue", "Cuido")


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

class Contador3(val contador: Int) {
 def ajuste(sum: Sumador): Contador3 = new Contador3(sum.adicionar(contador)) 
}

// val sumador = new Sumador(10)
// sumador.adicionar(4)
// val contador = new Contador3(2)
// contador.ajuste(sumador).contador


/** Objetos de compañia */

/** Ejercicio 10 */
class Persona(val nombre: String, val apellido: String) {
 def name = s"$nombre $apellido"
}

// val p1 = new Persona("Alex", "Montoya")
// p1.name

object Persona {
 def apply(fullName: String): Persona = {
  val partes = fullName.split(" ")
  val nombre = partes(0)
  val apellido = partes(1)
  new Persona(nombre, apellido)
 }
}

// val p2 = Persona("Alex Montoya")
// p2.name


/** Ejercicio 11 */
class Director(val nombre: String, val apellido: String, val nacimiento: Int) {
 def name: String = s"$nombre $apellido"

 def copy(nombre: String = this.nombre,
          apellido: String = this.apellido,
	  nacimiento: Int = this.nacimiento): Director =
    new Director(nombre, apellido, nacimiento)
}

class Pelicula (val nombre: String, val presentacion: Int, val rangoIMDB: Double, val director: Director) {

 def directorEdad = presentacion - director.nacimiento

 def esDirigidaPor(director: Director) = this.director == director

 def copy(
 nombre: String = this.nombre,
 presentacion: Int = this.presentacion,
 rangoIMDB: Double = this.rangoIMDB,
 director: Director = this.director): Pelicula =
 new Pelicula(nombre, presentacion, rangoIMDB, director)

}

// val director1 = new Director("Jose", "Perez", 1998)
// director1.name
// val pelicula1 = new Pelicula("Titanic", 2000, 9.5, director1)
// pelicula1.directorEdad
// pelicula1.esDirigidaPor(director1)


object Director {

 def apply(nombre: String, apellido: String, nacimiento: Int): Director =
     new Director(nombre, apellido, nacimiento)

 def esMayor(director1: Director, director2: Director): Director = {
  if (director1.nacimiento < director2.nacimiento) director1 else director2
 }
 
}

// val director1 = Director("Jose", "Perez", 1998)
// val director2 = Director("Pablo", "Gonzalez", 1995)
// Director.esMayor(director1, director2)