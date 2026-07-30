

# Lumi
El lenguaje de programación Lumi <br/>

Un lenguaje de programación funcional basado en λ-cálculo para investigación científica. <br/>
Esperamos que este lenguaje se convierta en una herramienta eficiente y amigable que ilumine el camino de los investigadores.

## Características (Actualizando...)
* Núcleo minimalista con complementos de paquetes y macros
* Variables globales o variables estáticas dentro de funciones
* Implementación de recursión mediante el combinator Y
* Etiquetado de tipos (las variables locales en funciones no son obligatorias, son opcionales)
* Sistema de gestión de memoria o algoritmo de asignación de registros
* Evaluación estricta, considerando proporcionar evaluación perezosa
* Punteros (¿necesarios o no?)
* Call-CC y uso de Call-CC para control de bucles y manejo de excepciones (¿es aconsejable o no? ¿hay una alternativa mejor?)
* REPL y pruebas unitarias
* Sistema de indentación amigable <br/>

¡Bienvenidos a utilizar Issues como un foro para discutir y solicitar características!


## Kits de Lumi
Intérprete, compilador y REPL (Bucle Lectura–Evaluación–Impresión) de Lumi.

#### Ejecutar programa Lumi
```shell
lumi <source_file.lumi> [command line arguments]
```

#### Compilar programa Lumi
```shell
lumi -c <source_file.lumi> <output_file.lumo>
```

#### REPL interactivo
```shell
lumi
```
