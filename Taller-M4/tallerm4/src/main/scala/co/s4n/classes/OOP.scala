package co.s4n.classes

/** 
  * Taller de Programacion orientada a objetos en Scala
  * @author Alex Montoya Franco
  */

/**
  * Ejercicio 1
  * Funcion cuadrado que recibe un n de tipo double y calcula su cuadrado
  * Funcion cubo que recibe un n de tipo double y calcula su cubo
  * 
  * @param n      Numero de tipo double
  * @return       Cuadrado o cubo de n dependiendo de la funcion
  * @example      comp.cubo(3)  -->  27
  * 
  */
object comp {
    def cuadrado(n: Double) = n * n
    def cubo(n: Double) = n * cuadrado(n)
}


/**
  * Ejercicio 2
  * Funcion cuadrado que recibe un n de tipo Long y calcula su cuadrado
  * Funcion cubo que recibe un n de tipo Long y calcula su cubo
  * 
  * @param n      Numero de tipo Long
  * @return       Cuadrado o cubo de n dependiendo de la funcion
  * @example      comp2.cubo(3)  -->  27
  * 
  */
object comp2 {
    def cuadrado(n: Long) = n * n
    def cubo(n: Long) = n * cuadrado(n)
}


/**
  * Ejercicio 4
  * Definicion de la clase Gato
  * 
  * @constructor     Construye objetos de tipo Gato
  * @param nombre    Nombre del gato
  * @param color     Color del gato
  * @param comida    Comida favorita del gato
  * @example         val gato = new Gato("Firulai","Blanco","Atun")
  * 
  */
class Gato(val nombre: String, val color: String, val comida: String)

object Gatos {
    val gato1 = new Gato("Io", "Fawn", "Churrus")
    val gato2 = new Gato("Make", "Red", "Leche")
    val gato3 = new Gato("Docker", "Blue", "Cuido")
}


/**
  * Ejercicio 5
  * Funcion que acepta un Gato y retorna true si la comida favorita del gato son los Churrus
  * 
  * @param gato    Objeto de tipo Gato
  * @return        True si la comida favorita del gato son los Churrus, false en caso contrario
  * @example       VentaDeChurros.despachar(Gatos.gato1)  -->  true
  * 
  */
object VentaDeChurrus {
    def despachar(gato: Gato): Boolean = gato.comida == "Churrus"
}


/**
  * 
  * Ejercicio 6
  * Definicion de la clase conductor
  * 
  * @constructor                Construye objetos de tipo Conductor
  * @param nombre               Nombre del conductor
  * @param apellido             Apellido del conductor
  * @param totalCarreras        Total carreras del conductor
  * @param carrerasTerminadas   Total carreras terminadas del conductor
  * @note                       Todos los atributos del constructor tienen por defecto su metodo get
  * @example                    val conductor1 = new Conductor("David", "Rios", 5, 2) - conductor1.nombre
  * 
  */
class Conductor(val nombre: String, val apellido: String, val totalCarreras: Int, val carrerasTerminadas: Int) {
    /**
      * Funcion que retorna las carreras no terminadas del conductor
      *
      * @return      Total de carreras no terminadas
      * @example     conducto1.getCarrerasNoTerminadas()  -->  3
      *     
      */
    def getCarrerasNoTerminadas(): Int = totalCarreras - carrerasTerminadas
}

/**
  * Ejercicio 6
  * Definicion de la clase Escuderia
  *
  * @constructor       Construye objetos de tipo Escuderia
  * @param nombre      Nombre de la escuderia
  * @param conductor   Objeto conductor
  * @example           val escuderia1 = new Escuderia("001", conductor1)
  * 
  */
class Escuderia(val nombre: String, val conductor: Conductor)


/**
  * Ejercicio 7
  * Definicion de la clase Contador
  * 
  * @constructor       Construye objetos de tipo Contador
  * @param contador    Entero con el valor del contador
  * @example           new Contador(10).incr.decr.incr.incr.contador
  * 
  */
class Contador(val contador: Int) {
    def incr(): Contador = new Contador(contador + 1)
    def decr(): Contador = new Contador(contador - 1)
}


/**
  * Ejercicio 8
  * Definicion de la clase Contador2
  *
  * @constructor      Construye objetos de tipo Contador2
  * @param contador   Entero con el valor del contador
  * @example          new Contador2(10).incr(5).contador
  * @example          new Contador2(10).decr(2).contador
  * 
  */
class Contador2(val contador: Int) {
    def incr(param: Int = 1): Contador2 =	new Contador2(contador + param)
    def decr(param: Int = 1): Contador2 =	new Contador2(contador - param)
}


/**
  * Ejercicio 9 
  * Definicion de clase Sumador
  * 
  * @constructor     Construye objetos de tipo Sumador
  * @param monto     Entero con el valor del monto del sumador
  * @example         new Sumador(10).adicionar(4) --> 14
  */
class Sumador(monto: Int) {
    def adicionar(valor: Int) = valor + monto
}

/**
  * Ejercicio 9
  * Definicion de la clase Contador3
  *
  * @constructor      Construye objetos de tipo Contador3
  * @param contador   Entero con el valor del contador
  * @example          new Contador3(2).ajuste(new Sumador(10).adicionar(contador)).contador
  * 
  */
class Contador3(val contador: Int) {
    def ajuste(sum: Sumador): Contador3 = new Contador3(new Sumador(10).adicionar(contador)) 
}


/**
  * Ejercicio 10
  * Definicion de la clase Persona
  * 
  * @constructor      Construye objetos de tipo Persona
  * @param nombre     Nombre de la persona
  * @param apellido   Apellido de la persona
  * @example          val persona = new Persona("Alex", "Montoya") - persona.name
  * 
  */
class Persona(val nombre: String, val apellido: String) {
    def name = s"$nombre $apellido"
}

/**
  * Ejercicio 10
  * Objeto de compañia para la clase Persona
  * @example        val persona2 = Persona("Alex Montoya") - persona2.name
  * 
  */
object Persona {
    def apply(fullName: String): Persona = {
        val partes = fullName.split(" ")
        val nombre = partes(0)
        val apellido = partes(1)
        new Persona(nombre, apellido)
    }
}


/**
  * Ejercicio 11
  * Definicion de la clase Director
  * 
  * @constructor         Construye objetos de tipo Director
  * @param nombre        Nombre del director
  * @param apellido      Apellido del director
  * @param nacimiento    Año de nacimiento del director
  * @example             val director1 = new Director("Jose", "Perez", 1998) - director1.name
  * 
  */
class Director(val nombre: String, val apellido: String, val nacimiento: Int) {
    def name: String = s"$nombre $apellido"

    def copy(nombre: String = this.nombre,
             apellido: String = this.apellido,
             nacimiento: Int = this.nacimiento): Director =
        new Director(nombre, apellido, nacimiento)
}

/**
  * Ejercicio 11
  * Definicion de la clase Pelicula
  *
  * @constructor           Construye objetos de tipo Pelicula
  * @param nombre          Nombre de la pelicula
  * @param presentacion    Año de presentacion de la pelicula
  * @param rangoIMDB       Rango IMDB de la pelicula
  * @param director        Director de la pelicula
  * @example               val pelicula1 = new Pelicula("Titanic", 2000, 9.5, director1) - pelicula1.directorEdad
  * 
  */
class Pelicula (val nombre: String, val presentacion: Int, val rangoIMDB: Double, val director: Director) {
    def directorEdad = presentacion - director.nacimiento

    def esDirigidaPor(director: Director) = this.director == director

    def copy(nombre: String = this.nombre,
             presentacion: Int = this.presentacion,
             rangoIMDB: Double = this.rangoIMDB,
             director: Director = this.director): Pelicula =
        new Pelicula(nombre, presentacion, rangoIMDB, director)
}

/**
  * Ejercicio 11
  * Objeto de compañia para la clase Director
  * @example      val director1 = Director("Jose", "Perez", 1990)
  * @example      val director2 = Director("Pablo", "Gonzalez", 1980)
  * @example      Director.esMayor(director1, director2)
  * 
  */
object Director {
    def apply(nombre: String, apellido: String, nacimiento: Int): Director =
        new Director(nombre, apellido, nacimiento)

    def esMayor(director1: Director, director2: Director): Director = {
        if (director1.nacimiento < director2.nacimiento) director1 else director2
    }
}

/**
  * Ejercicio 11
  * Objeto de compañia para la clase Pelicula
  * @example     val pelicula1 = Pelicula("Titanic", 2015, 4.5, director1)
  * @example     val pelicula2 = Pelicula("La guerra de los mundos", 2015, 3.7, director2)
  * @example     Pelicula.mejorCalificada(pelicula1, pelicula2).nombre
  * @example     Pelicula.mayorDirectorEnElTiempo(pelicula1, pelicula2).nombre
  * 
  */
object Pelicula {
    def apply(nombre: String, presentacion: Int, rangoIMDB: Double, director: Director): Pelicula =
        new Pelicula(nombre, presentacion, rangoIMDB, director)

    def mejorCalificada(pelicula1: Pelicula, pelicula2: Pelicula): Pelicula = {
        if (pelicula1.rangoIMDB > pelicula2.rangoIMDB) pelicula1 else pelicula2
    }

    def mayorDirectorEnElTiempo(pelicula1: Pelicula, pelicula2: Pelicula): Director = {
        if (pelicula1.directorEdad > pelicula2.directorEdad) pelicula1.director else pelicula2.director
    } 
}