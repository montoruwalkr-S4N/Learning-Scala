# Learning Scala - Talleres S4N Campus

### **Hecho por:** Alex Montoya Franco
### **Con la guia y apoyo de:** Campus S4N, Docentes y Compañeros 

En este repositorio se encuentran todos los talleres de Scala desarrollados en el campus de S4N

## Pasos para ejecutar los talleres:

1. Acceder a la carpeta del taller correspondiente

```console
user@machine:~$ cd Taller-M<x>/tallerm<x>/
user@machine:~/Taller-M<x>/tallerm<x>$
```

2. Acceder al REPL de Scala

```console
user@machine:~/Taller-M<X>/tallerm<x>$ sbt console
scala> 
```

3. Importar el paquete correspondiente al taller y el objeto principal para simplificar el llamado de las funciones

Por ejemplo, para el Taller-M1:

```console
scala> import co.s4n.funciones._
scala> import co.s4n.funciones.Funciones._
```

4. Ejecutar las funciones que se deseen explorar

Por ejemplo:

```console
scala> Ejercicio1.areaTrianguloRectangulo(2,5)
val res0: Int = 5

scala> Ejercicio15.factorial(7)
val res1: Int = 5040
```


## Pasos para ejecutar los tests:

1. Acceder a la carpeta del taller correspondiente

```console
user@machine:~$ cd Taller-M<x>/tallerm<x>/
user@machine:~/Taller-M<x>/tallerm<x>$
```

2. Acceder a la consola de sbt

```console
user@machine:~/Taller-M<X>/tallerm<x>$ sbt
sbt:tallerm<x>>
```

3. Ejecutar tests

3.1. Ejecutar un test particular

```console
sbt:tallerm<x>> testOnly co.s4n.funciones.Ejercicio1Spec
```

3.2. Ejecutar todos los tests

```console
sbt:tallerm<x>> test
```